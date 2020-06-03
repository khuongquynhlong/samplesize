##### Lay mau ngau nhien don #####
fun_simple_random <- function(N, P, alpha, d, eps, nonrep = 0, deseff = 1) {
  z <- qnorm(1-alpha/2)
  n <- z^2*P*(1-P)*N/(d^2*(N-1)+z^2*P*(1-P))
  n <- n*deseff/(1-nonrep)
  return(ceiling(n))
}