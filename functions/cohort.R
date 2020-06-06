##### Uoc luong nguy co tuong doi #####
fun_cohort_est <- function(p1, p2, alpha, eps, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*((1-p1)/p1+(1-p2)/p2)/log(1-eps, base = exp(1))^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### Kiem dinh gia thuyet cho nguy co tuong doi #####
fun_cohort_hypo <- function(p1, p2, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- mean(c(p1, p2))
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_cohort_hypo_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  p <- mean(c(p1, p2))
  z_b <- (sqrt(n*(p1-p2)^2)-z_a*sqrt(2*p*(1-p)))/sqrt(p1*(1-p1)+p2*(1-p2))
  power <- pnorm(z_b)
  return(round(power, 2))
}