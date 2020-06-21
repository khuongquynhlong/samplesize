##### NC 2 mau doc lap, kiem dinh 2 trung binh #####
fun_2means_ind_hypo <- function(alpha, power, m1, m2, sd, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  es <- (m1-m2)/sd
  n <- 2*((z_a+z_b)/es)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2means_ind_hypo_power <- function(alpha, n, m1, m2, sd) {
  z_a <- qnorm(1-alpha/2)
  es <- (m1-m2)/sd
  z_b <- es*sqrt(n/2)-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC 2 mau ghep cap, kiem dinh 2 trung binh #####
fun_2means_pair_hypo <- function(alpha, power, m, sd, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  es <- m/sd
  n <- ((z_a+z_b)/es)^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_2means_pair_hypo_power <- function(alpha, n, m, sd) {
  z_a <- qnorm(1-alpha/2)
  es <- m/sd
  z_b <- es*sqrt(n)-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC 2 mau doc lap, kiem dinh 2 ty le #####
fun_2props_ind_hypo <- function(alpha, power, p1, p2, nonrep = 0, deseff = 1) {
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
fun_survive <- function(hr, alpha, power, p1, p2, nonrep = 0, deseff = 1) {
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
fun_equi_cont <- function(alpha, power, m1, m2, d, sd, nonrep = 0, deseff = 1) {
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

##### NC tuong duong voi bien dinh tinh #####
fun_equi_cat <- function(alpha, power, p1, p2, d, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  h <- (abs(p1-p2)-d)/sqrt(p1*(1-p1)+p2*(1-p2))
  n <- 2*(z_a+z_b)/h^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_equi_cat_power <- function(alpha, n, p1, p2, d) {
  z_a <- qnorm(1-alpha/2)
  h <- (abs(p1-p2)-d)/sqrt(p1*(1-p1)+p2*(1-p2))
  z_b <- (n*h^2)/2-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC khong kem hon voi bien dinh luong #####
fun_noninfer_cont <- function(alpha, power, m1, m2, d, sd, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha)
  z_b <- qnorm(power)
  h <- (abs(m1-m2)-d)/sd
  n <- 2*(z_a+z_b)/h^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_noninfer_cont_power <- function(alpha, n, m1, m2, d, sd) {
  z_a <- qnorm(1-alpha)
  h <- (abs(m1-m2)-d)/sd
  z_b <- (n*h^2)/2-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}

##### NC khong kem hon voi bien dinh tinh #####
fun_noninfer_cat <- function(alpha, power, p1, p2, d, nonrep = 0, deseff = 1) {
  z_a <- qnorm(1-alpha)
  z_b <- qnorm(power)
  h <- (abs(p1-p2)-d)/sqrt(p1*(1-p1)+p2*(1-p2))
  n <- 2*(z_a+z_b)/h^2
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}

fun_noninfer_cat_power <- function(alpha, n, p1, p2, d) {
  z_a <- qnorm(1-alpha)
  h <- (abs(p1-p2)-d)/sqrt(p1*(1-p1)+p2*(1-p2))
  z_b <- (n*h^2)/2-z_a
  power <- pnorm(z_b)
  return(round(power, 2))
}


##### NC thu nghiem lam sang theo cum #####
fun_cluster_randomize <- function(sd, alpha, power, vif, gamma, delta) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- 2*sd^2*(z_a+z_b)^2*vif/(gamma*delta^2)
  return(ceiling(n))
}