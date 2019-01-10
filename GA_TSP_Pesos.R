library(GA)

data("eurodist", package = "datasets")
D <- as.matrix(eurodist)
Peso <- matrix(rexp(21*21, rate=10), nrow = 21, ncol=21)
total <- D * Peso

#Given a tour, calculate the distance-------------------------------------
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

#-------------------------------------Fitness (inverse total dist)------------------------------------
tpsFitness <- function(tour, ...) {
  1 / tourLength(tour, ...)
}

#-----------------------------------Run Model-----------------------------------------------------
GA.fit <- ga(type = "permutation", fitness = tpsFitness, distMatrix = total, min = 1, 
             max = attr(eurodist, "Size"), popSize = 10, maxiter = 500, run = 100, pmutation = 0.2, 
             monitor = NULL)

#------------------------------------Plot graph--------------------------------------------------
mds <- cmdscale(eurodist)
x <- mds[, 1]
y <- -mds[, 2]
n <- length(x)

plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Tour after GA converged")
points(x, y, pch = 16, cex = 1.5, col = "grey")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10), col = "lightgrey")
tour <- GA.fit@solution[1,]
tour <- c(tour, tour[1])
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
       col = "steelblue", lwd = 2)
text(x, y - 100, labels(eurodist), cex = 0.8)
