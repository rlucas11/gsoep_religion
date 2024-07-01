## This code is for the revision
## Runs #clpm.latent.all.rev.R#
## Source model files
source("scripts/measurementModelMod.R") ## Lavaan model name: measurement_modelMod
source("scripts/originalModelMod2.R") ## Lavaan model name: model1_main
source("scripts/fullRiclpmMod2.R") ## Lavaan model name: model1_riclpmMod
source("scripts/clpmObservedMod2.R") ## Lavaan model name: clpm_observed
source("scripts/riclpmObservedMod2.R") ## Lavaan model name: riclpm_observed

################################################################################
## Setup
################################################################################

## Read cleaned data
data <- read_csv("data/filteredData.csv")


################################################################################
## Original Model: CLPM, Latent, All Traits
################################################################################


clpm.latent.all <- sem(model1_main,
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
    file = paste0(location, "/clpm.latent.results.rev2.RData")
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
    file = paste0(location, "/riclpm.latent.results.rev2.RData")
    )


################################################################################
## CLPM, Observed, All Traits
################################################################################

model.all.observed.clpm <- sem(clpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
    )
summary(model.all.observed.clpm)
fit.all.observed.clpm.full <- fitMeasures(model.all.observed.clpm)
est.all.observed.clpm.full <- standardizedSolution(model.all.observed.clpm,
                     type = "std.all", se = TRUE, zstat = TRUE,
                     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
                     )

## Predicted correlations based on CLPM Model
cor.all.observed.clpm.full <- as.data.frame(cov2cor(
    fitted(model.all.observed.clpm)$cov))
write_csv(
    cor.all.observed.clpm.full,
    paste0(location, "/predictCorsClpm.rev2.csv")
)


clpm.observed.results <- list(
    fit.all.observed.clpm.full,
    est.all.observed.clpm.full,
    summary(model.all.observed.clpm)
)

save(clpm.observed.results,
    file = paste0(location, "/clpm.observed.results.rev2.RData")
    )


################################################################################
## RICLPM, Observed, All Traits
################################################################################

model.all.observed.riclpm <- sem(riclpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
    )
summary(model.all.observed.riclpm)
fit.all.observed.riclpm.full <- fitMeasures(model.all.observed.riclpm)
est.all.observed.riclpm.full <- standardizedSolution(model.all.observed.riclpm,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)

## Predicted correlations based on RICLPM Model
cor.all.observed.riclpm.full <- as.data.frame(cov2cor(
    fitted(model.all.observed.riclpm)$cov
))
write_csv(
    cor.all.observed.riclpm.full,
    paste0(location, "/predictedCorRiclpm.rev2.csv")
)

riclpm.observed.results <- list(
    fit.all.observed.riclpm.full,
    est.all.observed.riclpm.full,
    summary(model.all.observed.riclpm)
)
save(riclpm.observed.results,
    file = paste0(location, "/riclpm.observed.results.rev2.RData")
    )

