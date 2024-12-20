vonb<- function(age, rate, max_length, shift) max_length * (1 - exp(-rate * (age - shift)))

nll<- function(pars) {
    RTMB::getAll(pars)
    ll<- 0

    spline_parameters<- exp(working_spline_parameters)

    rate_spline<- nnspline::update_spline(
        spline,
        parameters = spline_parameters[, 1],
        node_values = node_values[, 1]
    )
    ll<- ll + nnspline::dspline(node_values[, 1], rate_spline, log = TRUE)

    max_length_spline<- nnspline::update_spline(
        spline,
        parameters = spline_parameters[, 2],
        node_values = node_values[, 2]
    )
    ll<- ll + nnspline::dspline(node_values[, 2], max_length_spline, log = TRUE)

    shift<- -exp(working_shift)
    data$prediction<- vonb(
        data$age,
        exp(mu_rate + rate_spline$values[data$spline_idx]),
        exp(mu_length + max_length_spline$values[data$spline_idx]),
        shift
    )
    # raw_ll<- RTMB::dlnorm(
    #     x = log(data$length),
    #     meanlog = log(data$prediction),
    #     sdlog = exp(working_sd),
    #     log = TRUE
    # )
    raw_ll<- RTMB::dnorm(
        data$length,
        data$prediction,
        exp(working_sd) * sqrt(data$prediction),
        log = TRUE
    )
    ll<- ll + sum(robustifyRTMB::robustify(raw_ll, robustness, "ll"))
    weights<- robustifyRTMB::robust_weight(raw_ll, robustness, "ll")

    fixed_parameters<- c(mu_rate, mu_length, shift)
    mcreportRTMB::MCREPORT(spline_parameters)
    mcreportRTMB::MCREPORT(node_values)
    mcreportRTMB::MCREPORT(fixed_parameters)
    mcreportRTMB::MCREPORT(weights)
    return( -ll )
}