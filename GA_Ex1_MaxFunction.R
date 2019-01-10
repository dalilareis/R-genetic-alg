library(GA)

f <- function(x) (x^2+x)*cos(x)
min <- -10; max <- 10
curve(f, min, max, n = 1000)

GA <- ga(type = "real-valued", fitness = f, min = min, max = max, monitor = FALSE)
summary(GA)
plot(GA)

#better looking graph
curve(f, min, max, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)
