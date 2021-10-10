## Simple ABC 
set.seed(42)
library(coda)

## Flat priors for the two factors
draw_pop <- function () {
  return (runif(1, min=0.1, max=1000))
}
draw_spdf <- function () {
  return (runif(1, min=0, max=1))
}

## Function to simulate population size given scaling factors
simulate_data <- function (clim, pop_scale, spdf_scale) { 
  return((clim * pop_scale) * spdf_scale)
}

# Function to compute the quantiles.
# We choose to use 3 quantiles.
calc_distance <- function(true, simulated) {
  return(sum((true - simulated)^2))
}

## Accept or reject based on the first method to compare a simulated sample to the observed data
accept_reject <- function (true, simulated, acceptance_threshold) {
  distance <- calc_distance(true, simulated)
  # print(distance)
  if((distance < acceptance_threshold) ) return(T) else return(F)
}

## Combining all the above elements to get the full rejection sampler
sample_by_rejection <- function (data, nbit, acceptance_threshold, accept_or_reject_function) {
  n <- length(data)
  accepted_or_rejected <- vector(length = nbit)
  sampled_pop_scale <- vector(length = nbit, mode = "numeric")
  sampled_spdf_scale <- vector (length = nbit, mode = "numeric")
  for (i in 1:nbit){
    pop_scale <- draw_pop()
    spdf_scale <- draw_spdf()
    parameters <- list("pop_scale" = pop_scale, "spdf_scale" = spdf_scale)
    simulated_data <- simulate_data(n, pop_scale, spdf_scale)
    accepted_or_rejected[i] <- accept_or_reject_function(data, simulated_data, acceptance_threshold)
    sampled_pop_scale[i] <- pop_scale
    sampled_spdf_scale[i] <- spdf_scale
  }
  return(data.frame(cbind("accepted_or_rejected" = accepted_or_rejected, 
                          "sampled_pop_scale" = sampled_pop_scale, 
                          "sampled_spdf_scale" = sampled_spdf_scale)))
}

mydat <- dat$spdf

system.time(
  sampled_parameter_values <- 
    sample_by_rejection(mydat, 200000, 1e6, 
                        accept_reject)
)

## Acceptance
sum(sampled_parameter_values$accepted_or_rejected)
sum(sampled_parameter_values$accepted_or_rejected) / 200000

samples_as_mcmc <- 
  mcmc(sampled_parameter_values[which(sampled_parameter_values$accepted_or_rejected==1),c(2,3)])

summary(samples_as_mcmc)

plot(samples_as_mcmc)

stop()
##-----------------------------------------------------------------------------
## Log normal priors for the two factors
# quick plot
x <- seq(0, 1000, length = 200)
plot(x, dlnorm(x, 5), type = 'l', lwd = 2)
draw_pop <- function () {
  return (rlnorm(1, meanlog = 6))
}
x <- seq(0, 0.1, length = 200)
plot(x, dlnorm(x, -7), type = 'l', lwd = 2)
draw_spdf <- function () {
  return (rlnorm(1, meanlog = -6))
}

system.time(
  sampled_parameter_values <- 
    sample_by_rejection(mydat, 200000, 1e4, 
                        accept_reject)
)

## Acceptance
sum(sampled_parameter_values$accepted_or_rejected)
sum(sampled_parameter_values$accepted_or_rejected) / 200000

samples_as_mcmc <- 
  mcmc(sampled_parameter_values[which(sampled_parameter_values$accepted_or_rejected==1),c(2,3)])

summary(samples_as_mcmc)

plot(samples_as_mcmc)

