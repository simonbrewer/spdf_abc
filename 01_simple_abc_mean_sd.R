## Simple example using ABC to estimate mean and sigma
## https://rpubs.com/boussau/BasicABC

set.seed(42)
n <- 100

## Define population parameters
mu <- 4.3
sigma <- 2.7

## Simulate some data
data <- rnorm(n, mean = mu, sd = sigma)

## Define flat priors for mu and sigma and functions to draw samples
draw_mu <- function () {
  return (runif(1, min=0, max=10))
}
draw_sigma <- function () {
  return (runif(1, min=0, max=10))
}

## Create general function to simulate some data with given parameters
simulate_data <- function (n, mu, sigma) { 
  return(rnorm(n, mean = mu, sd = sigma))
}

## ----------------------------------------------------------------------------
## Compare using three quantiles (0.1, 0.5, 0.9)

## Define a function to calculate quantiles
# We choose to use 3 quantiles.
compute_quantiles <- function(data) {
  return (quantile(data, probs=c(0.1, 0.5, 0.9)))
}

## Function to calculate distance (Euclidean)
calculate_distance <- function (true, simulated) {
  distance = sqrt(sum(mapply(function(x,y) (x-y)^2, true, simulated)))
  return(distance)
}

## Function to accept or reject a simulation based on
## a) Calculated distance
## b) Distance threshold
accept_reject <- function (true, simulated, acceptance_threshold) {
  distance = calculate_distance(compute_quantiles(true), 
                                compute_quantiles(simulated))
  if((distance < acceptance_threshold) ) return(T) else return(F)
}

## Combine all of this into one big function
## Runs for nbit iterations
## Draws random parameters
## Calculate distance
## Accepts or rejects
## Returns data frame with all simulations plus 0/1 accept/reject
sample_by_rejection <- function (true_data, nbit, acceptance_threshold, accept_reject_function) {
  n <- length(true_data)
  accepted_rejected <- vector(length = nbit)
  sampled_mus <- sampled_sigmas <- vector(length = nbit, mode = "numeric")
  for (i in 1:nbit){
    mu <- draw_mu()
    sigma <- draw_sigma()
    parameters <- list("mu"=mu, "sigma"=sigma)
    simulated_data <- simulate_data(n, mu, sigma)
    accepted_rejected[i] <- accept_reject_function(true_data, 
                                                   simulated_data, 
                                                   acceptance_threshold)
    sampled_mus[i] <- mu
    sampled_sigmas[i] <- sigma
  }
  return(data.frame(cbind("accepted_or_rejected" = accepted_rejected, 
                          "sampled_mus" = sampled_mus, 
                          "sampled_sigmas" = sampled_sigmas)))
}

system.time(
  sampled_parameter_values_squared_distances <- 
    sample_by_rejection(data, 200000, 0.5, 
                        accept_reject)
)

## Acceptance rates
sum(sampled_parameter_values_squared_distances$accepted_or_rejected)
sum(sampled_parameter_values_squared_distances$accepted_or_rejected) / 200000

## Use coda to plot and summarize accepted samples
library(coda)
acc_samples <- 
  mcmc(sampled_parameter_values_squared_distances[which(sampled_parameter_values_squared_distances$accepted_or_rejected == 1), c(2,3)])

summary(acc_samples)

plot(acc_samples)
