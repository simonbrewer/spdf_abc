## Derive a population curve from the Trace data
## This is a very simple scaling of the values

set.seed(42)
source("00_functions.R")

dat <- read.csv("./data/AGU_Bonneville_cSPD_T21KClim.csv")

## Start poopn growth at 3000
dat <- subset(dat, CalBP <= 3000)

plot(dat$CalBP, dat$AT, type ='l')

dat$t <- 1:nrow(dat)

## Population as simple scaling
pop_scale <- 100
pop_sd <- 25
# dat$P <- (dat$AT - min(dat$AT)) / (max(dat$AT) - min(dat$AT))
dat$P <- (dat$AT * pop_scale) + rnorm(nrow(dat), 0, pop_sd)
plot(dat$t, dat$P)

plot(dat$AT, dat$P)

## What does lm tell us?
summary(lm(P ~ AT, dat))

## Constant scaling to SPDF
spdf_fac <- 0.001 ## 0.1% of people leave a trace
dat$spdf <- dat$P * spdf_fac
plot(dat$t, dat$spdf)
# plot(dat$t, dat$AT)

write.csv(dat, "./data/test_spdf_simple.csv", row.names = FALSE)
