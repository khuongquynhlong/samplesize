##### NC 2 mau doc lap, kiem dinh 2 ty le #####
fun_2props_ind_hypo <- function(alpha, power, p1, p2, nonrep, deseff) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- (p1+p2)/2
  es <- (p1-p2)/sqrt(p*(1-p))
  n <- 2*((z_a+z_b)/es)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2props_ind_hypo_power <- function(alpha, n, p1, p2) {
  z_a <- qnorm(1-alpha/2)
  p <- (p1+p2)/2
  es <- abs((p1-p2)/sqrt(p*(1-p)))
  z_b <- sqrt(n/2)*es-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC song con #####
fun_survive <- function(hr, alpha, power, p1, p2, nonrep, deseff) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- (hr+1)^2*(z_a+z_b)/((hr-1)^2*(2-p1-p2))
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_survive_power <- function(hr, alpha, n, p1, p2) {
  z_a <- qnorm(1-alpha/2)
  z_b <- n*(hr-1)^2*(2-p1-p2)/(hr+1)^2-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC tuong duong voi bien dinh luong #####
fun_equi_cont <- function(alpha, power, m1, m2, d, sd, nonrep, deseff) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  h <- (abs(m1-m2)-d)/sd
  n <- 2*(z_a+z_b)/h^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_equi_cont_power <- function(alpha, n, m1, m2, d, sd) {
  z_a <- qnorm(1-alpha/2)
  h <- (abs(m1-m2)-d)/sd
  z_b <- (n*h^2)/2-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}