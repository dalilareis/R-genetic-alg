library(ggplot2)
library(genalg)

item <- c("pocketknife", "beans", "potatoes", "unions", "sleeping bag", "rope", "compass")
survivalpoints <- c(10, 20, 15, 2, 30, 10, 30)
hungerpoints <- c(0, 20, 15, 0, 0, 0, 0)
weight <- c(1, 5, 10, 1, 7, 5, 1)
weightlimit <- 15
dataset <- data.frame(item=item, totalpoints=survivalpoints + hungerpoints,
                      weight = weight)

#cromossoma
chromosome <- c(1,1,0,0,1,0,0)
dataset [chromosome == 1, ]
cat (chromosome %*% dataset$totalpoints)

#evaluation function
evalFunc <- function(x) {
  current_solution_totalpoints <- x %*% dataset$totalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(-current_solution_totalpoints)
}

#run and view model
iter = 100
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))
plot(GAmodel)

#best solution
bestSolution <- GAmodel$population[which.min(GAmodel$evaluations),]
dataset[bestSolution == 1,]

