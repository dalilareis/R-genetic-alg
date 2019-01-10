library(GA)

f <- function(x) { 100 * (x[1]^2 - x[2])^2 + (1 - x[1])^2 }
c1 <- function(x) { x[1]*x[2] + x[1] - x[2] + 1.5 }
c2 <- function(x) { 10 - x[1]*x[2] }

fitness <- function(x)
{
  f <- -f(x) # we need to maximise -f(x)
  pen <- sqrt(.Machine$double.xmax) # penalty term
  penalty1 <- max(c1(x),0)*pen # penalisation for 1st inequality constraint
  penalty2 <- max(c2(x),0)*pen # penalisation for 2nd inequality constraint
  f - penalty1 - penalty2 # fitness function value
}

GA = ga("real-valued", fitness = fitness,
        min = c(0,0), max = c(1,13),
        maxiter = 5000, run = 1000, seed = 123)
summary(GA)
plot(GA)