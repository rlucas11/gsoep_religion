## This code is for the revision
## Runs #clpm.latent.all.rev.R#
## Source model files
source("scripts/measurementModelMod.R") ## Lavaan model name: measurement_modelMod
source("scripts/originalModelMod.R") ## Lavaan model name: model1_mod
source("scripts/fullRiclpm.rev.R") ## Lavaan model name: model1_riclpmMod

################################################################################
## Setup
################################################################################

library(tidyverse)
library(lavaan)

## Read cleaned data
data <- read_csv("data/filteredData.csv")


## Set Results Location (comment when testing)
location <- "results"
## location <- "testResults"

################################################################################
## Measurement Model
################################################################################

measurement.model <- sem(measurement_modelMod,
    missing = "FIML",
    estimator = "MLR",
    data = data
)
summary(measurement.model)
fit.measurement <- fitMeasures(measurement.model)
est.measurement <- standardizedSolution(measurement.model)
measurement.results <- list(
    summary(measurement.model),
    fit.measurement,
    est.measurement
)

save(measurement.results,
     file = paste0(location, "/measurement.results.rev.RData")
     )


################################################################################
## Original Model: CLPM, Latent, All Traits
################################################################################

clpm.latent.all <- sem(model1_mod,
                       missing = "FIML",
                       estimator = "MLR",
                       data = data)
## Save to file because it takes so long to run
summary(clpm.latent.all)
fit.all.latent.clpm.full <- fitMeasures(clpm.latent.all)
est.all.latent.clpm.full <- standardizedSolution(clpm.latent.all,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )

clpm.latent.results <- list(
    fit.all.latent.clpm.full,
    est.all.latent.clpm.full,
    summary(clpm.latent.all)
)
save(clpm.latent.results,
    file = paste0(location, "/clpm.latent.results.rev.RData")
    )

################################################################################
## RICLPM, Latent, All Traits
################################################################################
riclpm.latent.all <- sem(model1_riclpmMod,
                       missing = "FIML",
                       estimator = "MLR",
                       data = data)
## Save to file because it takes so long to run
summary(riclpm.latent.all)
fit.all.latent.riclpm.full <- fitMeasures(riclpm.latent.all)
est.all.latent.riclpm.full <- standardizedSolution(riclpm.latent.all,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )

riclpm.latent.results <- list(
    fit.all.latent.riclpm.full,
    est.all.latent.riclpm.full,
    summary(riclpm.latent.all)
)

save(riclpm.latent.results,
    file = paste0(location, "/riclpm.latent.results.rev.RData")
    )

