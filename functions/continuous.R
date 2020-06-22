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

##### Nghiên cứu 2 mẫu độc lập, xác định sự khác biệt 2 trung bình"
fun1_2means_ind_est <- function(sd, d, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- 2*z^2*sd^2/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun2_2means_ind_est <- function(n1, sd1, n2, sd2, d, alpha, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
  n <- 2*z^2*sd^2/d^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

##### Kiem dinh gia thuyet cho 1 trung binh #####
fun_1mean_hypo <- function(m1, m0, sd, alpha, power, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  es <- (m1-m0)/sd
  n <- ((z_a+z_b)/es)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_1mean_hypo_power <- function(m1, m0, sd, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  es <- (m1-m0)/sd
  z_b <- sqrt(n)*es-z_a
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