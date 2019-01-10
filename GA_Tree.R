library(GA)
library(spuRs)

data("trees", package = "spuRs")
tree <- trees[trees$ID == "1.3.11", 2:3]

#Theta representa a, b e c
richards <- function(x, theta) {
  theta[1] * (1 - exp(-theta[2] * x))^theta[3]
}

#----------------------------Fitness (quadratica)----------------------------------------------
fitnessL2 <- function(theta, x, y) {
  - sum((y - richards(x, theta))^2)
}

#------------------------------Run model-------------------------------------------
GA2 <- ga(type = "real-valued", fitness = fitnessL2, x = tree$Age, y = tree$Vol, min = c(3000, 0, 2),
          max = c(4000, 1, 4), popSize = 500, crossover = gareal_spCrossover, maxiter = 5000,
          run = 200, names = c("a", "b", "c"))
summary(GA2)


