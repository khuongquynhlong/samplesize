##### Uoc luong OR voi sai so tuong doi #####
fun_case_est <- function(p1, p2, alpha, eps, nonrep = 0, deseff = 1) {
  z = qnorm(1-alpha/2)
  n = z^2/(log(1-eps))^2*(1/(p1*(1-p1))+1/(p2*(1-p2)))
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### So sanh 2 OR #####
fun_case_hypo <- function(p1, p2, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- (z_a*sqrt(2*p2*(1-p2)) + z_b*sqrt(p1*(1-p1) + p2*(1-p2)))^2/(p1-p2)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_case_hypo_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- (sqrt(n*(p1-p2)^2) - z_a*sqrt(2*p2*(1-p2)))/sqrt(p1*(1-p1) + p2*(1-p2))
  power <- pnorm(z_b)
  return(round(power, 2))
}