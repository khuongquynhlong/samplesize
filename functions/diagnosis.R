# sensitivity
fun_sen_est <- function(sens, d, alpha, p) {
    z <- qnorm(1-alpha/2)
    n <- z^2*sens*(1-sens)/(d^2*p)
    #n <- n/(1-nonrep)
    return(ceiling(n))
}


# specificity
fun_spec_est <- function(spec, d, alpha, p) {
    z <- qnorm(1-alpha/2)
    n <- z^2*spec*(1-spec)/(d^2*(1-p))
    #n <- n/(1-nonrep)
    return(ceiling(n))
}
