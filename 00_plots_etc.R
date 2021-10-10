## Quick plots

dat <- read.csv("./data/AGU_Bonneville_cSPD_T21KClim.csv")

head(dat)

plot(dat$CalBP, dat$MxT, type = 'l')

plot(dat$CalBP, dat$GP, type = 'l')

plot(dat$CalBP, dat$SPD, type = 'l')

plot(dat$CalBP, dat$GP, type = 'l', xlim = c(0, 2500))

plot(dat$CalBP, dat$ST, type = 'l', xlim = c(0, 2500))

