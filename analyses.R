## Load libraries
library(lavaan)
library(tidyverse)

## Source model files
source("scripts/originalModel.R")
source("scripts/clpmUni.R")
source("scripts/riclpmUni.R")
source("scripts/gclpm.R")
source("scripts/fullRiclpm.R")
source("scripts/riclpmObserved.R")
source("scripts/clpmObserved.R")

## Read Data
data <- read_csv("data/final.csv")

## Rename religion variales so recoded versions are used in model
names(data)[65:72] <- c(paste0(rep("relig", 4),
                                c("05", "09", "13", "17"),
                                rep("_orig", 4)),
                         paste0(rep("relig", 4),
                                c("05", "09", "13", "17")))


################################################################################
## Reproduce original with full sample
################################################################################
## This takes a long time to run

model1 <- sem(model1_main, missing = "FIML", estimator = "MLR", data = data)
## Save to file because it takes so long to run
sink(file = "info/originalModel.txt", append = TRUE)
summary(model1)
standardizedSolution(model1,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )
sink(file = NULL)

modelRiclpm <- sem(model1_riclpm, missing = "FIML", estimator = "MLR", data = data)
sink(file = "info/fullRiclpmModel.txt", append = TRUE)
summary(modelRiclpm)
standardizedSolution(modelRiclpm,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )
sink(file = NULL)


################################################################################
## All Traits, Observed Variables, full sample
################################################################################

model.all.observed <- sem(riclpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
)
summary(model.all.observed)
fitMeasures(model.all.observed)

standardizedSolution(model.all.observed,
                     type = "std.all", se = TRUE, zstat = TRUE,
                     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
                     )


model.all.observed.clpm <- sem(clpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
    )
summary(model.all.observed.clpm)
fitMeasures(model.all.observed.clpm)
standardizedSolution(model.all.observed.clpm,
                     type = "std.all", se = TRUE, zstat = TRUE,
                     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
                     )



################################################################################
## Single Trait Models
################################################################################

## Each block pulls trait-specific variables and renames to work with the
## generic syntax in the model code.

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

agrClpm <- sem(clpmUni, data = agr, missing = "FIML")
summary(agrClpm)
standardizedSolution(agrClpm)

agrRiclpm <- sem(riclpmUni, data = agr, missing = "FIML", estimator = "MLR")
summary(agrRiclpm)
standardizedSolution(agrRiclpm)

agrGclpm <- sem(gclpmUni, data = agr, missing = "FIML", estimator = "MLR")
summary(agrGclpm)
standardizedSolution(agrGclpm)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

cnsClpm <- sem(clpmUni, data = cns, missing = "FIML")
summary(cnsClpm)
standardizedSolution(cnsClpm)

cnsRiclpm <- sem(riclpmUni, data = cns, missing = "FIML")
summary(cnsRiclpm)


## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

extClpm <- sem(clpmUni, data = ext, missing = "FIML")
summary(extClpm)
standardizedSolution(extClpm)

extRiclpm <- sem(riclpmUni, data = ext, missing = "FIML")
summary(extRiclpm)


## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

neuClpm <- sem(clpmUni, data = neu, missing = "FIML")
summary(neuClpm)
standardizedSolution(neuClpm)

neuRiclpm <- sem(riclpmUni, data = neu, missing = "FIML")
summary(neuRiclpm)


## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

opnClpm <- sem(clpmUni, data = opn, missing = "FIML")
summary(opnClpm)
standardizedSolution(opnClpm)

opnRiclpm <- sem(riclpmUni, data = opn, missing = "FIML")
summary(opnRiclpm)



















