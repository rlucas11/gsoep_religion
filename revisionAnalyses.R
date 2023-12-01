################################################################################
## Setup
################################################################################

## Read cleaned data
data <- read_csv("data/filteredData.csv")

traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "x1", "x2", "x3", "x4", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "y1", "y2", "y3", "y4"
)

## Load model
source('scripts/dpm.R')

## Set Results Location (change when testing)
location <- "results"


################################################################################
## Trait analyses
################################################################################

## Agreeableness
agr <- data %>%
    select(contains("agr") | contains("relig"))

names(agr) <- traitModelNames

agrFit <- sem(dpm,
    data = agr,
    missing = "FIML",
    estimator = "MLR"
)
summary(agrFit)

est.agr.dpm.obs <- standardizedSolution(agrFit,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE,
    level = .95,
    output = "data.frame"
)
fit.agr.dpm.results <- fitMeasures(agrFit)
agr.dpm.obs.results <- list(
    est.agr.dpm.obs,
    fit.agr.dpm.results
)


## Conscientiousness
cns <- data %>%
    select(contains("cns") | contains("relig"))

names(cns) <- traitModelNames

cnsFit <- sem(dpm,
    data = cns,
    missing = "FIML",
    estimator = "MLR"
)
summary(cnsFit)

est.cns.dpm.obs <- standardizedSolution(cnsFit,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE,
    level = .95,
    output = "data.frame"
)
fit.cns.dpm.results <- fitMeasures(cnsFit)
cns.dpm.obs.results <- list(
    est.cns.dpm.obs,
    fit.cns.dpm.results
)


## Extraversion
ext <- data %>%
    select(contains("ext") | contains("relig"))

names(ext) <- traitModelNames

extFit <- sem(dpm,
    data = ext,
    missing = "FIML",
    estimator = "MLR"
)
summary(extFit)

est.ext.dpm.obs <- standardizedSolution(extFit,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE,
    level = .95,
    output = "data.frame"
)
fit.ext.dpm.results <- fitMeasures(extFit)
ext.dpm.obs.results <- list(
    est.ext.dpm.obs,
    fit.ext.dpm.results
)


## Neuroticism
neu <- data %>%
    select(contains("neu") | contains("relig"))

names(neu) <- traitModelNames

neuFit <- sem(dpm,
    data = neu,
    missing = "FIML",
    estimator = "MLR"
)
summary(neuFit)

est.neu.dpm.obs <- standardizedSolution(neuFit,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE,
    level = .95,
    output = "data.frame"
)
fit.neu.dpm.results <- fitMeasures(neuFit)
neu.dpm.obs.results <- list(
    est.neu.dpm.obs,
    fit.neu.dpm.results
)


## Openness
opn <- data %>%
    select(contains("opn") | contains("relig"))

names(opn) <- traitModelNames

opnFit <- sem(dpm,
    data = opn,
    missing = "FIML",
    estimator = "MLR"
)
summary(opnFit)

est.opn.dpm.obs <- standardizedSolution(opnFit,
    type = "std.all",
    se = TRUE,
    zstat = TRUE,
    pvalue = TRUE,
    ci = TRUE,
    level = .95,
    output = "data.frame"
)
fit.opn.dpm.results <- fitMeasures(opnFit)
opn.dpm.obs.results <- list(
    est.opn.dpm.obs,
    fit.opn.dpm.results
)

## Save Results
single.dpm.obs.results <- list(
    agr.dpm.obs.results,
    cns.dpm.obs.results,
    ext.dpm.obs.results,
    neu.dpm.obs.results,
    opn.dpm.obs.results
)

save(single.dpm.obs.results,
    file = paste0(location, "/single.dpm.obs.results.RData")
)


