##### Uoc luong 1 ty le ####
fun1_1prop_est <- function(p, d, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*p*(1-p)/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun2_1prop_est <- function(p, eps, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*(1-p)/(eps^2*p)
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### So sanh voi ty le quan the #####
fun_1prop_hypo <- function(p_0, p_a, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- (z_a*sqrt(p_0*(1-p_0))+z_b*sqrt(p_a*(1-p_a)))^2/(p_a-p_0)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_1prop_hypo_power <- function(p_0, p_a, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- (sqrt(n*(p_a-p_0)^2)-z_a*sqrt(p_0*(1-p_0)))/sqrt(p_a*(1-p_a))
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### Uoc luong khac biet 2 ty le #####
fun_2props_est <- function(p1, p2, alpha, d, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*(p1*(1-p1)+p2*(1-p2))/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### So sanh 2 ty le #####
fun_2props_hypo <- function(p1, p2, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- (p1+p2)/2
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2props_hypo_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  p <- (p1+p2)/2
  z_b <- (sqrt(n*(p1-p2)^2)-z_a*sqrt(2*p*(1-p)))/sqrt(p1*(1-p1)+p2*(1-p2))
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### So sanh 2 ty le (ty le nho) #####
fun_2props_hypo_small <- function(p1, p2, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- (z_a+z_b)^2/(2*(asin(sqrt(p2))-asin(sqrt(p1)))^2)
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2props_hypo_small_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- sqrt(2*n*(asin(sqrt(p2))-asin(sqrt(p1)))^2)-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}