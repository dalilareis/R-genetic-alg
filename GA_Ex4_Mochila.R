library(GA)

item <- c("pocketknife", "beans", "potatoes", "unions", "sleeping bag", "rope", "compass")
survivalpoints <- c(10, 20, 15, 2, 30, 10, 30)
weight <- c(1, 5, 10, 1, 7, 5, 1)
weightlimit <- 15
dataset <- data.frame(item=item, survivalpoints=survivalpoints, weight = weight)

#Funcao fitness 
fitness <- function(x) 
{ 
  f <- sum(x * survivalpoints) #Maximizar pontos
  penalty <- sum(weight) * abs(sum(x * weight) - weightlimit) #Penalizacao por peso
  f - penalty #Valor da fitness (maximizar pts e minimizar penalty/weight)
}

#Correr modelo
GA <- ga(type = "binary", fitness = fitness, nBits = length(weight), maxiter = 1000, 
         run = 200, popSize = 20)
summary(GA)

#Mostrar solucao
bestSolution <- GAmodel$population[which.min(GAmodel$evaluations),]
dataset[bestSolution == 1,]
