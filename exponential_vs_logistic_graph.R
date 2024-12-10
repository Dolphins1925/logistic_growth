# Load required package
#library(ggplot2)
library(ggplot2)

# Parameters
N0 <- 986.5075       # Initial population size
r <- 0.0100086       # Growth rate
K <- 6e+10           # Carrying capacity
time <- seq(0, 5000, by = 10) # Time points for plotting

# Define logistic growth function
logistic_fun <- function(t) {
  (N0 * K * exp(r * t)) / (K - N0 + N0 * exp(r * t))
}

# Define exponential growth function
exponential_fun <- function(t) {
  N0 * exp(r * t)
}

# Calculate population sizes
logistic_growth <- sapply(time, logistic_fun)
exponential_growth <- sapply(time, exponential_fun)

# Create a data frame for ggplot
growth_data <- data.frame(
  Time = rep(time, 2),
  Population = c(logistic_growth, exponential_growth),
  Model = rep(c("Logistic", "Exponential"), each = length(time))
)

# Plot the growth curves
ggplot(data = growth_data, aes(x = Time, y = Population, color = Model)) +
  geom_line(size = 1) +
  scale_y_log10() +  # Log scale for better comparison
  labs(title = "Comparison of Exponential and Logistic Growth",
       x = "Time (minutes)",
       y = "Population Size (log scale)",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")
