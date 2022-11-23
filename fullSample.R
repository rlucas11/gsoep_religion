## Source model files
source("scripts/originalModel.R")
source("scripts/fullRiclpm.R")
source("scripts/clpmObserved.R")
source("scripts/riclpmObserved.R")
source("scripts/clpmUni.R")
source("scripts/riclpmUni.R")
source("scripts/clpmUniObserved.R")
source("scripts/riclpmUniObserved.R")

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
    est.all.latent.clpm.full
)
save(clpm.latent.results, file = "results/clpm.latent.results.RData")

################################################################################
## RICLPM, Latent, All Traits
################################################################################

riclpm.latent.all <- sem(model1_riclpm,
                       missing = "FIML",
                       estimator = "MLR",
                       data = data)
## Save to file because it takes so long to run
summary(riclpm.latent.all)
fit.all.latent.riclpm.full <- fitMeasures(clpm.latent.all)
est.all.latent.riclpm.full <- standardizedSolution(clpm.latent.all,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )

riclpm.latent.results <- list(
    fit.all.latent.riclpm.full,
    est.all.latent.riclpm.full
)
save(riclpm.latent.results, file = "results/riclpm.latent.results.RData")


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
cor.all.observed.clpm.full <- cov2cor(fitted(model.all.observed.clpm)$cov)
write_csv(cor.all.observed.clpm.full, "results/predictCorsClpm.csv")

clpm.observed.results <- list(
    fit.all.observed.clpm.full,
    est.all.observed.clpm.full
)

save(clpm.observed.results, file = "clpm.observed.results.RData")


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
cor.all.observed.riclpm.full <- cov2cor(fitted(model.all.observed.riclpm)$cov)
write_csv(cor.all.observed.riclpm.full, "results/predictedCorRiclpm.csv")

riclpm.observed.results <- list(
    fit.all.observed.riclpm.full,
    est.all.observed.riclpm.full
)
save(riclpm.observed.results, file = "riclpm.observed.results.RData")

################################################################################
## CLPM, single trait, latent
################################################################################

traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17",
    "first.state"
)

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"), first.state)
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
    fit.agr.clpm.latent
)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"), first.state)
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
    fit.cns.clpm.latent
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"), first.state)
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
    fit.ext.clpm.latent
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"), first.state)
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
    fit.neu.clpm.latent
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"), first.state)
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
    fit.opn.clpm.latent
)

single.clpm.latent.results <- list(
    agr.clpm.latent.results,
    cns.clpm.latent.results,
    ext.clpm.latent.results,
    neu.clpm.latent.results,
    opn.clpm.latent.results
)
save(single.clpm.latent.results, file = "results/single.clpm.latent.results")

################################################################################
## RICLPM, Latent, Single Trait
################################################################################

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"), first.state)
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
    fit.agr.riclpm.latent
)


## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"), first.state)
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
    fit.cns.riclpm.latent
)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"), first.state)
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
    fit.ext.riclpm.latent
)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"), first.state)
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
    fit.neu.riclpm.latent
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"), first.state)
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
    fit.opn.riclpm.latent
)

single.riclpm.latent.results <- list(
    agr.riclpm.latent.results,
    cns.riclpm.latent.results,
    ext.riclpm.latent.results,
    neu.riclpm.latent.results,
    opn.riclpm.latent.results
)
save(single.riclpm.latent.results, file = "results/single.riclpm.latent.results")

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
    select(contains("agr"), contains("relig"), first.state)
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
    select(contains("cns"), contains("relig"), first.state)
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
    select(contains("ext"), contains("relig"), first.state)
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
    select(contains("neu"), contains("relig"), first.state)
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
    select(contains("opn"), contains("relig"), first.state)
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
save(single.clpm.obs.results, file = "results/single.clpm.obs.results")

################################################################################
## RICLPM, Observed, Single Trait
################################################################################

## Agreeableness

agr <- data %>%
    select(contains("agr"), contains("relig"), first.state)
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
    select(contains("cns"), contains("relig"), first.state)
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
    select(contains("ext"), contains("relig"), first.state)
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
    select(contains("neu"), contains("relig"), first.state)
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
    select(contains("opn"), contains("relig"), first.state)
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
save(single.riclpm.obs.results, file = "results/single.riclpm.obs.results")