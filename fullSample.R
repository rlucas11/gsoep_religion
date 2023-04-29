## Source model files
source("scripts/measurementModel.R")
source("scripts/originalModel.R")
source("scripts/fullRiclpm.R")
source("scripts/clpmObserved.R")
source("scripts/riclpmObserved.R")
source("scripts/clpmUni.R")
source("scripts/riclpmUni.R")
source("scripts/clpmUniObserved.R")
source("scripts/riclpmUniObserved.R")

## ## Testing
## ## Preserve original data
## dataOld <- data
## ## Select random subset
## data <- data |>
##     mutate(select = sample(c(1:20), size = 1, replace = TRUE)) |>
##     filter(select == 1)
location <- "testResults"

## Set Results Location (comment when testing)
location <- "results"


################################################################################
## Measurement Model
################################################################################

measurement.model <- sem(measurement_model,
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
save(measurement.results, paste0(location, "measurement.results.RData")


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
    file = paste0(location, "/clpm.latent.results.RData")
    )

################################################################################
## RICLPM, Latent, All Traits
################################################################################

riclpm.latent.all <- sem(model1_riclpm,
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
    file = paste0(location, "/riclpm.latent.results.RData")
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
    paste0(location, "/predictCorsClpm.csv")
)


clpm.observed.results <- list(
    fit.all.observed.clpm.full,
    est.all.observed.clpm.full,
    summary(model.all.observed.clpm)
)

save(clpm.observed.results,
    file = paste0(location, "/clpm.observed.results.RData")
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
    paste0(location, "/predictedCorRiclpm.csv")
)

riclpm.observed.results <- list(
    fit.all.observed.riclpm.full,
    est.all.observed.riclpm.full,
    summary(model.all.observed.riclpm)
)
save(riclpm.observed.results,
    file = paste0(location, "/riclpm.observed.results.RData")
    )


################################################################################
## CLPM, single trait, latent
################################################################################

traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- traitModelNames
agrFit <- sem(clpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = agr,
    em.h1.iter.max = 20000
)
est.agr.clpm.latent <- standardizedSolution(agrFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.agr.clpm.latent <- fitMeasures(agrFit)
agr.clpm.latent.results <- list(
    est.agr.clpm.latent,
    fit.agr.clpm.latent,
    summary(agrFit)
)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- traitModelNames
cnsFit <- sem(clpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = cns,
    em.h1.iter.max = 20000
)
est.cns.clpm.latent <- standardizedSolution(cnsFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.cns.clpm.latent <- fitMeasures(cnsFit)
cns.clpm.latent.results <- list(
    est.cns.clpm.latent,
    fit.cns.clpm.latent,
    summary(cnsFit)
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- traitModelNames
extFit <- sem(clpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = ext,
    em.h1.iter.max = 20000
)
est.ext.clpm.latent <- standardizedSolution(extFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.ext.clpm.latent <- fitMeasures(extFit)
ext.clpm.latent.results <- list(
    est.ext.clpm.latent,
    fit.ext.clpm.latent,
    summary(extFit)
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- traitModelNames
neuFit <- sem(clpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = neu,
    em.h1.iter.max = 20000
)
est.neu.clpm.latent <- standardizedSolution(neuFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.neu.clpm.latent <- fitMeasures(neuFit)
neu.clpm.latent.results <- list(
    est.neu.clpm.latent,
    fit.neu.clpm.latent,
    summary(neuFit)
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- traitModelNames
opnFit <- sem(clpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = opn,
    em.h1.iter.max = 20000
)
est.opn.clpm.latent <- standardizedSolution(opnFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.opn.clpm.latent <- fitMeasures(opnFit)
opn.clpm.latent.results <- list(
    est.opn.clpm.latent,
    fit.opn.clpm.latent,
    summary(opnFit)
)

single.clpm.latent.results <- list(
    agr.clpm.latent.results,
    cns.clpm.latent.results,
    ext.clpm.latent.results,
    neu.clpm.latent.results,
    opn.clpm.latent.results
)
save(single.clpm.latent.results,
    file = paste0(location, "/single.clpm.latent.results.RData")
    )


################################################################################
## RICLPM, Latent, Single Trait
################################################################################

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- traitModelNames
agrFit <- sem(riclpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = agr,
    em.h1.iter.max = 20000
)
est.agr.riclpm.latent <- standardizedSolution(agrFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.agr.riclpm.latent <- fitMeasures(agrFit)
agr.riclpm.latent.results <- list(
    est.agr.riclpm.latent,
    fit.agr.riclpm.latent,
    summary(agrFit)
)


## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- traitModelNames
cnsFit <- sem(riclpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = cns,
    em.h1.iter.max = 20000
)
est.cns.riclpm.latent <- standardizedSolution(cnsFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.cns.riclpm.latent <- fitMeasures(cnsFit)
cns.riclpm.latent.results <- list(
    est.cns.riclpm.latent,
    fit.cns.riclpm.latent,
    summary(cnsFit)
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- traitModelNames
extFit <- sem(riclpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = ext,
    em.h1.iter.max = 20000
)
est.ext.riclpm.latent <- standardizedSolution(extFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.ext.riclpm.latent <- fitMeasures(extFit)
ext.riclpm.latent.results <- list(
    est.ext.riclpm.latent,
    fit.ext.riclpm.latent,
    summary(extFit)
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- traitModelNames
neuFit <- sem(riclpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = neu,
    em.h1.iter.max = 20000
)
est.neu.riclpm.latent <- standardizedSolution(neuFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.neu.riclpm.latent <- fitMeasures(neuFit)
neu.riclpm.latent.results <- list(
    est.neu.riclpm.latent,
    fit.neu.riclpm.latent,
    summary(neuFit)
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- traitModelNames
opnFit <- sem(riclpmUni,
    missing = "FIML",
    estimator = "MLR",
    data = opn,
    em.h1.iter.max = 20000
)
est.opn.riclpm.latent <- standardizedSolution(opnFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.opn.riclpm.latent <- fitMeasures(opnFit)
opn.riclpm.latent.results <- list(
    est.opn.riclpm.latent,
    fit.opn.riclpm.latent,
    summary(opnFit)
)

single.riclpm.latent.results <- list(
    agr.riclpm.latent.results,
    cns.riclpm.latent.results,
    ext.riclpm.latent.results,
    neu.riclpm.latent.results,
    opn.riclpm.latent.results
)
save(single.riclpm.latent.results,
    file = paste0(location, "/single.riclpm.latent.results.RData")
    )


################################################################################
## CLPM, Observed, Single Trait
################################################################################

traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "tr05", "tr09", "tr13", "tr17", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17",
    "first.state"
)

## Agreeableness

agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- traitModelNames
agrFit <- sem(clpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = agr,
    em.h1.iter.max = 20000
)
est.agr.clpm.obs <- standardizedSolution(agrFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.agr.clpm.obs <- fitMeasures(agrFit)
agr.clpm.obs.results <- list(
    est.agr.clpm.obs,
    fit.agr.clpm.obs
)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- traitModelNames
cnsFit <- sem(clpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = cns,
    em.h1.iter.max = 20000
)
est.cns.clpm.obs <- standardizedSolution(cnsFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.cns.clpm.obs <- fitMeasures(cnsFit)
cns.clpm.obs.results <- list(
    est.cns.clpm.obs,
    fit.cns.clpm.obs
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- traitModelNames
extFit <- sem(clpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = ext,
    em.h1.iter.max = 20000
)
est.ext.clpm.obs <- standardizedSolution(extFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.ext.clpm.obs <- fitMeasures(extFit)
ext.clpm.obs.results <- list(
    est.ext.clpm.obs,
    fit.ext.clpm.obs
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- traitModelNames
neuFit <- sem(clpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = neu,
    em.h1.iter.max = 20000
)
est.neu.clpm.obs <- standardizedSolution(neuFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.neu.clpm.obs <- fitMeasures(neuFit)
neu.clpm.obs.results <- list(
    est.neu.clpm.obs,
    fit.neu.clpm.obs
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- traitModelNames
opnFit <- sem(clpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = opn,
    em.h1.iter.max = 20000
)
est.opn.clpm.obs <- standardizedSolution(opnFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.opn.clpm.obs <- fitMeasures(opnFit)
opn.clpm.obs.results <- list(
    est.opn.clpm.obs,
    fit.opn.clpm.obs
)

single.clpm.obs.results <- list(
    agr.clpm.obs.results,
    cns.clpm.obs.results,
    ext.clpm.obs.results,
    neu.clpm.obs.results,
    opn.clpm.obs.results
)
save(single.clpm.obs.results,
    file = paste0(location, "/single.clpm.obs.results.RData")
    )


################################################################################
## RICLPM, Observed, Single Trait
################################################################################

## Agreeableness

agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- traitModelNames
agrFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = agr,
    em.h1.iter.max = 20000
)
est.agr.riclpm.obs <- standardizedSolution(agrFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.agr.riclpm.obs <- fitMeasures(agrFit)
agr.riclpm.obs.results <- list(
    est.agr.riclpm.obs,
    fit.agr.riclpm.obs
)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- traitModelNames
cnsFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = cns,
    em.h1.iter.max = 20000
)
est.cns.riclpm.obs <- standardizedSolution(cnsFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.cns.riclpm.obs <- fitMeasures(cnsFit)
cns.riclpm.obs.results <- list(
    est.cns.riclpm.obs,
    fit.cns.riclpm.obs
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- traitModelNames
extFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = ext,
    em.h1.iter.max = 20000
)
est.ext.riclpm.obs <- standardizedSolution(extFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.ext.riclpm.obs <- fitMeasures(extFit)
ext.riclpm.obs.results <- list(
    est.ext.riclpm.obs,
    fit.ext.riclpm.obs
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- traitModelNames
neuFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = neu,
    em.h1.iter.max = 20000
)
est.neu.riclpm.obs <- standardizedSolution(neuFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.neu.riclpm.obs <- fitMeasures(neuFit)
neu.riclpm.obs.results <- list(
    est.neu.riclpm.obs,
    fit.neu.riclpm.obs
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- traitModelNames
opnFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = opn,
    em.h1.iter.max = 20000
)
est.opn.riclpm.obs <- standardizedSolution(opnFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.opn.riclpm.obs <- fitMeasures(opnFit)
opn.riclpm.obs.results <- list(
    est.opn.riclpm.obs,
    fit.opn.riclpm.obs
)

single.riclpm.obs.results <- list(
    agr.riclpm.obs.results,
    cns.riclpm.obs.results,
    ext.riclpm.obs.results,
    neu.riclpm.obs.results,
    opn.riclpm.obs.results
)
save(single.riclpm.obs.results,
    file = paste0(location, "/single.riclpm.obs.results.RData")
)


################################################################################
## Collect results for full sample models
################################################################################

## Load libraries (if not already done)
library(tidyverse)
library(ggplot2)

## Setup labels
traitLabels <- matrix(
    c(
        "agr", "a",
        "cns", "c",
        "ext", "e",
        "neu", "n",
        "opn", "o"
    ),
    nrow = 5, ncol = 2, byrow = TRUE
)


#### Functions to extract and summarize
#### For models with all traits
## Extract averages across multiple waves
extractAvg <- function(results, trait) {
    pLabel <- paste0(
        "c_r",
        trait
    )
    pLabel2 <- paste0(
        "c_",
        trait,
        "r"
    )
    ## Religion predicted from trait
    agg <- results %>%
        filter(label == pLabel) %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    ## Trait predicted from religion
    agg2 <- results %>%
        filter(label == pLabel2) %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    return(c(agg, agg2))
}

## Extract results from full set
extractParameterEstimates <- function(results,
                                      model,
                                      type,
                                      variables) {
    result <- data.frame(
        est = numeric(),
        ci.lower = numeric(),
        ci.upper = numeric(),
        est.1 = numeric(),
        ci.lower.1 = numeric(),
        ci.upper.1 = numeric(),
        trait = character(),
        model = character(),
        type = character(),
        variables = character()
    )
    for (i in 1:nrow(traitLabels)) {
        trait <- traitLabels[i, 2]
        result[i, ] <- c(
            extractAvg(results, trait),
            traitLabels[i,1],
            model,
            type,
            variables
        )
    }
    return(result)
}

#### Functions to extract results
#### These are for single-trait models
## Extract average values across waves
extractAvgSingle <- function(results) {
    ## Religion predicted from trait
    agg <- results %>%
        filter(label == "cl_t") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    ## Trait predicted from religion
    agg2 <- results %>%
        filter(label == "cl_r") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    return(c(agg, agg2))
}

## Extract estimates across all models
extractParameterEstimatesSingle <- function(results,
                                            model,
                                            type,
                                            variables) {
    result <- data.frame(
        est = numeric(),
        ci.lower = numeric(),
        ci.upper = numeric(),
        est.1 = numeric(),
        ci.lower.1 = numeric(),
        ci.upper.1 = numeric(),
        trait = character(),
        model = character(),
        type = character(),
        variables = character()
    )
    for (i in 1:nrow(traitLabels)) {
        trait <- traitLabels[i, 2]
        result[i, ] <- c(
            extractAvgSingle(results[[i]][[1]]),
            traitLabels[i,1],
            model,
            type,
            variables
        )
    }
    return(result)
}

location <- "testResults"
location <- "results"

## Load results for each set of models
load(paste0(location, "/clpm.latent.results.RData"))
results.c.l.a <- clpm.latent.results[2][[1]]
c.l.a <- extractParameterEstimates(
    results.c.l.a,
    "clpm",
    "latent",
    "all"
)
c.l.a

load(paste0(location, "/riclpm.latent.results.RData"))
results.r.l.a <- riclpm.latent.results[2][[1]]
r.l.a <- extractParameterEstimates(
    results.r.l.a,
    "riclpm",
    "latent",
    "all"
)
r.l.a

load(paste0(location, "/clpm.observed.results.RData"))
results.c.o.a <- clpm.observed.results[2][[1]]
c.o.a <- extractParameterEstimates(
    results.c.o.a,
    "clpm",
    "observed",
    "all"
)
c.o.a

load(paste0(location, "/riclpm.observed.results.RData"))
results.r.o.a <- riclpm.observed.results[2][[1]]
r.o.a <- extractParameterEstimates(
    results.r.o.a,
    "riclpm",
    "observed",
    "all"
)
r.o.a

load(paste0(location, "/single.clpm.latent.results.RData"))
c.l.s <- extractParameterEstimatesSingle(
    single.clpm.latent.results,
    "clpm",
    "latent",
    "single"
)
c.l.s

load(paste0(location, "/single.riclpm.latent.results.RData"))
r.l.s <- extractParameterEstimatesSingle(
    single.riclpm.latent.results,
    "riclpm",
    "latent",
    "single"
)
r.l.s

load(paste0(location, "/single.clpm.obs.results.RData"))
c.o.s <- extractParameterEstimatesSingle(
    single.clpm.obs.results,
    "clpm",
    "observed",
    "single"
)
c.o.s

load(paste0(location, "/single.riclpm.obs.results.RData"))
r.o.s <- extractParameterEstimatesSingle(
    single.riclpm.obs.results,
    "riclpm",
    "observed",
    "single"
)
r.o.s



## Collect data from all models
plotData <- rbind(
    c.l.a,
    r.l.a,
    c.o.a,
    r.o.a,
    c.l.s,
    r.l.s,
    c.o.s,
    r.o.s
)


plotData %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = ci.lower,
            ymax = ci.upper,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5), size=3) +
        geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
        coord_flip() +
        theme_bw() +
        scale_color_grey() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()
        )

################################################################################
## Compare pattern of correlations
################################################################################

source("scripts/usefulFunctions.R")


## Get actual stabilities
clpm.a.a <- summarizeR(cor(data[, c("agr05", "agr09", "agr13", "agr17")], use = "pair"))
clpm.a.c <- summarizeR(cor(data[, c("cns05", "cns09", "cns13", "cns17")], use = "pair"))
clpm.a.e <- summarizeR(cor(data[, c("ext05", "ext09", "ext13", "ext17")], use = "pair"))
clpm.a.n <- summarizeR(cor(data[, c("neu05", "neu09", "neu13", "neu17")], use = "pair"))
clpm.a.o <- summarizeR(cor(data[, c("opn05", "opn09", "opn13", "opn17")], use = "pair"))
clpm.a.r <- summarizeR(cor(data[, c("relig05", "relig09", "relig13", "relig17")], use = "pair"))

## Get model predicted stabilities
predictedC <- read_csv("results/predictCorsClpm.csv")
predictedR <- read_csv("results/predictedCorRiclpm.csv")

## CLPM
clpm.p.a <- summarizeR(as.matrix(predictedC[1:4, 1:4]))
clpm.p.c <- summarizeR(as.matrix(predictedC[5:8, 5:8]))
clpm.p.e <- summarizeR(as.matrix(predictedC[9:12, 9:12]))
clpm.p.n <- summarizeR(as.matrix(predictedC[13:16, 13:16]))
clpm.p.o <- summarizeR(as.matrix(predictedC[17:20, 17:20]))
clpm.p.r <- summarizeR(as.matrix(predictedC[21:24, 21:24]))

## RICLPM
riclpm.p.a <- summarizeR(as.matrix(predictedR[1:4, 1:4]))
riclpm.p.c <- summarizeR(as.matrix(predictedR[5:8, 5:8]))
riclpm.p.e <- summarizeR(as.matrix(predictedR[9:12, 9:12]))
riclpm.p.n <- summarizeR(as.matrix(predictedR[13:16, 13:16]))
riclpm.p.o <- summarizeR(as.matrix(predictedR[17:20, 17:20]))
riclpm.p.r <- summarizeR(as.matrix(predictedR[21:24, 21:24]))

a.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.a, clpm.p.a, riclpm.p.a),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)
c.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.c, clpm.p.c, riclpm.p.c),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)
e.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.e, clpm.p.e, riclpm.p.e),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)
n.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.n, clpm.p.n, riclpm.p.n),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)
o.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.o, clpm.p.o, riclpm.p.o),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)
r.cors <- data.frame(
    Lag = rep(seq(4, 12, by = 4), 3),
    Stability = c(clpm.a.r, clpm.p.r, riclpm.p.r),
    Model = c(
        rep("Actual", 3),
        rep("CLPM", 3),
        rep("RI-CLPM", 3)
    )
)

corsForPlot <- list(
    a.cors,
    c.cors,
    e.cors,
    n.cors,
    o.cors,
    r.cors
)

save(corsForPlot,
    file = paste0(location, "corsForPlot.RData")
    )


aPlot <- ggplot(
    data = a.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula = y ~ log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1)) +
        theme(legend.position = "none") +
        ggtitle("Agreeableness") +
        scale_x_continuous(breaks = c(4, 8, 12))

cPlot <- ggplot(
    data = c.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula=y~log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1))+
        theme(legend.position = "none") +
        ggtitle("Conscientiousness") +
        scale_x_continuous(breaks = c(4, 8, 12))

ePlot <- ggplot(
    data = e.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula=y~log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1))+
        theme(legend.position = "none") +
        ggtitle("Extraversion") +
        scale_x_continuous(breaks = c(4, 8, 12))

nPlot <- ggplot(
    data = n.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula=y~log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1)) +
        theme(legend.position = "none") +
        ggtitle("Neuroticism") +
        scale_x_continuous(breaks = c(4, 8, 12))

oPlot <- ggplot(
    data = o.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula=y~log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1)) +
        theme(legend.position = "none") +
        ggtitle("Openness") +
        scale_x_continuous(breaks = c(4, 8, 12))

rPlot <- ggplot(
    data = r.cors,
    aes(
        x = Lag,
        y = Stability,
        group = Model,
        linetype = Model
    )
) +
    geom_smooth(formula=y~log(x), span = 2, color = "black") +
        theme_bw() +
        scale_y_continuous(limits = c(0, 1))+
        theme(legend.position = "none") +
        ggtitle("Religiosity") +
        scale_x_continuous(breaks = c(4, 8, 12))


grid.arrange(aPlot, cPlot, ePlot, nPlot, oPlot, rPlot, nrow = 2)
