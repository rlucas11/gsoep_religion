## Source model files
source("scripts/originalModel.R")
source("scripts/clpmUni.R")
source("scripts/riclpmUni.R")
source("scripts/gclpm.R")
source("scripts/fullRiclpm.R")
source("scripts/riclpmObserved.R")
source("scripts/clpmObserved.R")

################################################################################
## All Traits, Observed Variables
################################################################################

## CLPM
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
cor.all.observed.clpm.full <- cov2cor(fitted(model.all.observed.clpm)$cov)


## RI-CLPM
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
cor.all.observed.riclpm.full <- cov2cor(fitted(model.all.observed.riclpm)$cov)


