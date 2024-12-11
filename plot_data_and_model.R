#Script to plot data and model

growth_data <- read.csv("experiment.csv")

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

N0 <- exp(6.8941709)# population size 0 when t=0

r <- 0.0100086 #
  
K <- 6e+10 #

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") 
  
  #geom_point()

  #scale_y_continuous(trans='log10')


# Time at which to calculate population size
t <- 4980

# Calculate population size at time t
population_size <- N0 * exp(r * t)
population_size

