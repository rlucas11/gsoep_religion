library(metafor)
library(tidyverse)

data <- read_csv("results/clpm.latent.states.aggregated.estimates.csv")

yivi <- escalc(measure="COR", ri=neu.rt.cl, ni=samplesize, data=data)
meta <- rma(yi, vi, data=yivi, method="ML")
summary(meta)



dataRi <- read_csv("results/riclpm.latent.states.aggregated.estimates.csv")

yiviRi <- escalc(measure="COR", ri=neu.rt.cl, ni=samplesize, data=dataRi)
metaRi <- rma(yi, vi, data=yiviRi, method="ML")
summary(metaRi)


data <- read_csv("results/clpm.observed.states.aggregated.estimates.csv")

yivi <- escalc(measure="COR", ri=opn.rt.cl, ni=samplesize, data=data)
meta <- rma(yi, vi, data=yivi, method="ML")
summary(meta)



dataRi <- read_csv("results/riclpm.observed.states.aggregated.estimates.csv")

yiviRi <- escalc(measure="COR", ri=opn.rt.cl, ni=samplesize, data=dataRi)
metaRi <- rma(yi, vi, data=yiviRi, method="ML")
summary(metaRi)

