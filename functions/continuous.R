##### Uoc luong 1 trung binh #####
fun1_1mean_est <- function(sd, d, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*sd^2/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun2_1mean_est <- function(sd, alpha, mean, eps, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*sd^2/(eps^2*mean^2)
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### Kiem dinh gia thuyet cho 1 trung binh #####
fun_1mean_hypo <- function(sd, m_0, m_a, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- sd^2*(z_a+z_b)^2/(m_0-m_a)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_1mean_hypo_power <- function(sd, m_0, m_a, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- sqrt(n*(m_0-m_a)^2/sd^2)-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### Uoc luong khac biet giua 2 trung binh #####
fun_2means_est <- function(sd, d, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- 2*z^2*sd^2/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### So sanh 2 trung binh #####
fun_2means_hypo <- function(m1, m2, sd1, sd2, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  sd <- sqrt((sd1^2+sd2^2)/2)
  n <- 2*sd^2*(z_a+z_b)^2/(m1-m2)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2means_hypo_power <- function(m1, m2, sd1, sd2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  sd <- sqrt((sd1^2+sd2^2)/2)
  z_b <- sqrt(n*(m1-m2)^2/(2*sd^2))-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}