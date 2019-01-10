library(GA)

f <- function(x) abs(x) + cos(x)
min <- -20; max <- +20
curve(f, min, max)

fitness <- function(x) - f(x)

#-----------------------------------Observe iterations--------------------------------------
monitor <- function(obj) {
  curve(f, min, max, main = paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, -obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}

#----------------------------------Run model-------------------------------------------------
GA <- ga(type = "real-valued", fitness = fitness, min = min, max = max, monitor = monitor)
plot(GA)
summary(GA)


