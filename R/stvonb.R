#' Fit a von Bertalanffy growth curve with spatially  varying parameters.
#' 
#' @param data An sf data.frame with point geometries, an age column, and a length column.
#' @param nodes Either an sf data.frame giving the point location of nodes or an integer giving the number of nodes which will be smartly chosen.
#' @param ... Additional arguments, particularly those to pass to nnspline::create_nnspline, robustifyRTMB::robustly_optimize, and mcreportRTMB::mcreport.
#'   
#' @return A list containing the model fit, a template spline, and mcreported spline realizations
#' 
#' @export
fit_stvonb<- function(
        data,
        nodes = data,
        ...
    ) {
    call<- match.call(expand.dots = TRUE) |> as.list()

    if( "sf" %in% class(nodes) ) {
        nodes<- unique(sf::st_coordinates(nodes))
    } else if( is.numeric(nodes) ) {
        nodes<- nnspline::nnodes(
            sf::st_coordinates(data),
            n_nodes = max(1, floor(nodes)),
            max.it = 200
        )$nodes
    } else { stop("Nodes must be given as an sf data.frame or as an integer.") }
    spline_args<- call[names(call) %in% names(formals(nnspline::create_nnspline))]
    spline_args$x<- unique(sf::st_coordinates(data))
    spline_args$nodes<- nodes
    spline<- do.call(nnspline::create_nnspline, spline_args)
    data$spline_idx<- nnspline::nns(
        sf::st_coordinates(data),
        spline,
        index = TRUE
    )
    
    # Fit a model to the data
    robopt_args<- call[names(call) %in% names(formals(robustifyRTMB::robustly_optimize))]
    environment(nll)<- environment()
    robopt_args$func<- nll
    robopt_args$parameters<- list(
        working_spline_parameters = matrix(
            0,
            nrow = length(spline$parameters),
            ncol = 2
        ),
        node_values = matrix(
            0,
            nrow = length(spline$node_values),
            ncol = 2
        ),
        mu_rate = 0,
        mu_length = with(
            data,
            log(mean(length[age > quantile(age, 0.8)]))
        ),
        working_shift = 0,
        working_sd = 0
    )
    robopt_args$random<- c("node_values")
    robopt_args$smooth<- c("node_values")
    robopt_args$nodes<- spline$nodes
    fit<- do.call(robustifyRTMB::robustly_optimize, robopt_args)

    # Get mcreports of spline parameters and node values
    mcreport_args<- call[names(call) %in% names(formals(mcreportRTMB::mcreport))]
    mcreport_args$obj<- fit$obj
    mcreport_args$sdr<- fit$sdr
    mc<- do.call(mcreportRTMB::mcreport, mcreport_args)

    ans<- list(
        fit = fit,
        template_spline = spline,
        mcreport = mc,
        call = call
    )
    return( ans )
}

#' Predict spatial growth curves
#' 
#' @param fit A fitted model from fit_stvonb
#' @param raster A stars raster
#' @param age Ages to use for length-at-age predictions, if any.
#' @param quantiles The quantiles to predict. Defaults to a 95% confidence interval around the mean
#' 
#' @return A list with the spatial von Bertalanffy parameter predictions and spatial length-at-age predictions
#' 
#' @export
predict_stvonb<- function(
        fit,
        raster,
        age,
        quantiles = c(0.05, 0.5, 0.95)
    ) {
    ans<- list(
        parameters = NA,
        length_at_age = NA
    )

    # Reduce raster to just the raster part
    raster_dimensions<- stars::st_dimensions(raster)[attr(stars::st_dimensions(raster), "raster")$dimensions]
    raster<- stars::st_as_stars(
        list(value = array(0, dim(raster_dimensions))),
        dimensions = raster_dimensions
    )

    # 1.) recreate spline with the same nodes but x is updated to raster locations
    spline<- nnspline::create_nnspline(
        x = sf::st_coordinates(raster) |> as.matrix(),
        nodes = fit$template_spline$nodes,
        n_parents = fit$template_spline$n_parents,
        parameters = fit$template_spline$parameters,
        covariance_function = fit$template_spline$covariance_function,
        node_graph = fit$template_spline$node_graph,
        LT = fit$template_spline$LT
    )

    # 2.) Predict the rate and max_length parameters for each replicate
    mc<- fit$mcreport
    vonb_predictions<- c(raster[1], raster[1], along = "variable")
    names(vonb_predictions)<- "value"
    vonb_predictions<- stars::st_set_dimensions(
        vonb_predictions,
        "variable",
        values = c("rate", "max_length")
    )

    mcreport_args<- fit$call[names(fit$call) %in% names(formals(mcreportRTMB::mcreport))]
    parallel<- if( "parallel" %in% names(mcreport_args) ) mcreport_args$parallel else 1
    trace<- if( "trace" %in% names(mcreport_args) ) mcreport_args$trace else TRUE
    if( (parallel > 1) && requireNamespace("parallel", quietly = TRUE) ) {
        lapplyfn<- parallel::mclapply
    } else {
        lapplyfn<- lapply
    }
    vonb_predictions<- lapplyfn(
        seq(ncol(mc[[1]])),
        function(i, ...) {
            if( trace ) {
                cat(
                    paste0(
                        "(",
                        i,
                        " / ",
                        ncol(mc[[1]]),
                        ")\n"
                    )
                )
            }
            rate_spline<- nnspline::update_spline(
                spline,
                parameters = mc$spline_parameters[, 1, i],
                node_values = mc$node_values[, 1, i]
            )
            max_length_spline<- nnspline::update_spline(
                spline,
                parameters = mc$spline_parameters[, 2, i],
                node_values = mc$node_values[, 2, i]
            )
            vonb_predictions$value[, , 1]<- exp(mc$fixed_parameters[1, i] + rate_spline$values)
            vonb_predictions$value[, , 2]<- exp(mc$fixed_parameters[2, i] + max_length_spline$values)
            return(vonb_predictions)
        },
        mc.cores = parallel,
        mc.preschedule = FALSE
    )
    vonb_predictions<- do.call(
        c,
        c(
            vonb_predictions,
            list(along = "replicate")
        )
    )

    vonb_quantiles<- stars::st_apply(
        vonb_predictions,
        MARGIN = 1:3,
        quantile,
        probs = quantiles,
        na.rm = TRUE
    )
    vonb_quantiles<- aperm(
        vonb_quantiles,
        c(2:4, 1)
    )
    
    ans$parameters<- vonb_quantiles
    if( missing(age) ) return(ans)

    # 3.) Predict the length at age for each replicate
    length_predictions<- lapply(
        age,
        function(a) {
            age_prediction<- vonb_predictions[, , , 1, ]
            age_prediction<- stars::st_set_dimensions(
                age_prediction,
                "variable",
                names = "age",
                values = as.character(a)
            )
            age_prediction$value[, , 1, ]<- vonb(
                age = a,
                rate = vonb_predictions$value[, , 1, ],
                max_length = vonb_predictions$value[, , 2, ],
                shift = array(
                    rep(
                        mc$fixed_parameters[3, ],
                        each = prod(dim(vonb_predictions)[1:2])
                    ),
                    dim = dim(vonb_predictions$value[, , 1, ])
                )
            )
            return(age_prediction)
        }
    )
    length_predictions<- do.call(
        c,
        c(
            length_predictions,
            list(along = "age")
        )
    )
    length_quantiles<- stars::st_apply(
        length_predictions,
        MARGIN = 1:3,
        quantile,
        probs = quantiles,
        na.rm = TRUE
    )
    length_quantiles<- aperm(
        length_quantiles,
        c(2:4, 1)
    )
    ans$length_at_age<- length_quantiles

    return(ans)
}