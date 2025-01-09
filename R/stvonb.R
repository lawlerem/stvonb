#' Fit a von Bertalanffy growth curve with spatially  varying parameters.
#' 
#' @param data 
#'     An sf data.frame with point geometries, an age column, and a length 
#'     column.
#' @param nodes 
#'     Either an sf data.frame giving the point location of nodes or an integer 
#'     giving the number of nodes which will be smartly chosen.
#' @param ... Additional arguments, particularly those to pass to 
#'     nnspline::create_nnspline, robustifyRTMB::robustly_optimize, and 
#'     mcreportRTMB::mcreport.
#'   
#' @return A list containing:
#'   * robust_optimization
#'      The output of robustifyRTMB::robustly_optimize
#'   * template_spline
#'       An nnspline objet used to hold the nodes and node graph used for
#'       the model
#'   * mcreport
#'       The output of mcreportRTMB::mcreport for the spline parameters,
#'       node values, and mean rate, max length, and length zero age.
#'   * call
#'       The function call used.
#' 
#' @export
fit_stvonb<- function(
        data,
        nodes = data,
        ...
    ) {
    call<- match.call(expand.dots = TRUE) |> as.list()

    if( "sf" %in% (nodes |> class()) ) {
        nodes<- nodes |> sf::st_coordinates() |> unique()
    } else if( nodes |> is.integer() ) {
        nodes<- nnspline::nnodes(
            data |> sf::st_coordinates(),
            n_nodes = max(1, nodes |> floor()),
            max.it = 200
        )$nodes
    } else { stop("Nodes must be given as an sf data.frame or as an integer.") }
    arg_names<- nnspline::create_nnspline |> formals() |> names()
    spline_args<- call[(call |> names()) %in% arg_names]
    spline_args$x<- data |> sf::st_coordinates() |> unique()
    spline_args$nodes<- nodes
    spline<- nnspline::create_nnspline |> do.call(spline_args)
    data$spline_idx<- data |> 
        sf::st_coordinates() |>
        nnspline::nns(
            spline,
            index = TRUE
        )
    
    # Fit a model to the data
    arg_names<- robustifyRTMB::robustly_optimize |> formals() |> names()
    robopt_args<- call[(call |> names()) %in% arg_names]
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
            length[age > quantile(age, 0.8)] |> mean() |> log()
        ),
        working_shift = 0,
        working_sd = 0
    )
    robopt_args$random<- "node_values"
    robopt_args$smooth<- "node_values"
    robopt_args$nodes<- spline$nodes
    fit<- robustifyRTMB::robustly_optimize |> do.call(robopt_args)

    # Get mcreports of spline parameters and node values
    arg_names<- mcreportRTMB::mcreport |> formals() |> names()
    mcreport_args<- call[(call |> names()) %in% arg_names]
    mcreport_args$obj<- fit$obj
    mcreport_args$sdr<- fit$sdr
    mc<- mcreportRTMB::mcreport |> do.call(mcreport_args)

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
#' @param fit 
#'     A fitted model from fit_stvonb
#' @param raster 
#'     A stars raster
#' @param age 
#'     Ages to use for length-at-age predictions, if any.
#' @param quantiles 
#'     The quantiles to predict. Defaults to c(0.05, 0.5, 0.95).
#' 
#' @return A list with the spatial von Bertalanffy parameter predictions and 
#'     spatial length-at-age predictions
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
        x = raster |> sf::st_coordinates() |> as.matrix(),
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
    vonb_predictions<- vonb_predictions |>
        stars::st_set_dimensions(
            "variable",
            values = c("rate", "max_length")
        )


    arg_names<- mcreportRTMB::mcreport |> formals() |> names()
    mcreport_args<- fit$call[(fit$call |> names()) %in% arg_names]
    parallel<- if( "parallel" %in% (mcreport_args |> names()) ) {
        mcreport_args$parallel
     } else {
        1
     }
    silent<- if( "silent" %in% (mcreport_args |> names()) ) {
        mcreport_args$silent 
    } else {
        TRUE
    }
    if( (parallel > 1) && requireNamespace("parallel", quietly = TRUE) ) {
        lapplyfn<- parallel::mclapply
    } else {
        lapplyfn<- lapply
    }
    interpolate_replicate<- function(i, ...) {
        if( !silent ) {
            cat(
                paste0(
                    "\rUpdating mcreplicate: (", i, " / ", 
                    mc[[1]] |> ncol(), ")"
                )
            )
            if( i == (mc[[1]] |> ncol()) ) cat("\n")
            flush.console()
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
    }
    vonb_predictions<- mc[[1]] |> 
        ncol() |> 
        seq() |>
        lapplyfn(
            interpolate_replicate,
            mc.cores = parallel,
            mc.preschedule = FALSE
        )
    vonb_predictions<- c |> 
        do.call(
            c(
                vonb_predictions,
                list(along = "replicate")
            )
        )

    vonb_quantiles<- vonb_predictions |>
        stars::st_apply(
            MARGIN = 1:3,
            quantile,
            probs = quantiles,
            na.rm = TRUE
        )
    vonb_quantiles<- vonb_quantiles |> aperm(c(2:4, 1))
    
    ans$parameters<- vonb_quantiles
    if( missing(age) ) return( ans )

    # 3.) Predict the length at age for each replicate
    length_predictions<- age |>
        lapply(
            function(a) {
                age_prediction<- vonb_predictions[, , , 1, ]
                age_prediction<- age_prediction |>
                    stars::st_set_dimensions(
                        "variable",
                        names = "age",
                        values = a |> as.character()
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
    length_predictions<- c |>
        do.call(
            c(
                length_predictions,
                list(along = "age")
            )
        )
    length_quantiles<- length_predictions |>
        stars::st_apply(
            MARGIN = 1:3,
            quantile,
            probs = quantiles,
            na.rm = TRUE
        )
    length_quantiles<- length_quantiles |> aperm(c(2:4, 1))
    ans$length_at_age<- length_quantiles

    return( ans )
}