## This code is for the revision
## Runs #clpm.latent.all.rev.R#
## Source model files
source("scripts/clpmObservedMod2.R") ## Lavaan model name: clpm_observed

################################################################################
## Setup
################################################################################

## Read cleaned data
data <- read_csv("data/filteredData.csv")




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


