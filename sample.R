library(ggplot2)
library(plotly)

eps <- seq(0.1, 0.3, 0.1)
p <- seq(0.1, 0.99, 0.01)
alpha <- 0.05
fun2_1prop_est <- function(p, eps, alpha) {
  z <- qnorm(1-alpha/2)
  n <- ceiling(z^2*(1-p)/(eps^2*p))
  return(n)
}
df <- expand.grid(eps, p)
names(df) <- c("eps", "p")
df$n <- fun2_1prop_est(p = df$p, eps = df$eps, alpha = alpha)
df$eps <- as.factor(df$eps)
# plot <- ggplot(df, aes(x = p, y = n, color = eps)) +
#   geom_line() + geom_point() +
#   theme_minimal()
# ggplotly(plot)

plot_ly(df, x = ~p, y = ~n, color = ~eps,
        type = "scatter", mode = "lines+markers")

##################################

p1 <- 0.6
p2 <- c(0.52, 0.54, 0.56, 0.58)
power <- seq(0, 0.99, 0.01)
alpha <- 0.05
df <- expand.grid(p1, p2, power)
colnames(df) <- c("p1", "p2", "power")
fun_2props_hypo <- function(p1, p2, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- mean(c(p1, p2))
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
  return(ceiling(n))
}
df$n <- fun_2props_hypo(p1 = df$p1, p2 = df$p2, alpha = alpha, power = df$power)
df$p2 <- as.factor(df$p2)
plot <- ggplot(df, aes(x = n, y = power, color = p2)) +
  geom_line() + geom_point() + geom_hline(yintercept = 0.8) +
  scale_y_continuous(limits = c(0, 1))
ggplotly(plot)

a <- "0.1 0.2 0.3"
b <- as.numeric(unlist(strsplit(a, " ")))

