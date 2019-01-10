library(GA)

Rastrigin <- function(x1, x2) {20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, color.palette = bl2gr.colors)

GA <- ga(type = "real-valued", fitness = function(x) - Rastrigin(x[1],x[2]), 
         min = c(-5.12, -5.12), max = c(5.12,5.12), popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)


