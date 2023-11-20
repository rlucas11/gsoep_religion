################################################################################
## Setup
################################################################################

## Load packages
library(lavaan)
library(tidyverse)

## Get code for creating models
source("~/Projects/code-generator/buildMplus.R")

## Read cleaned data
data <- read_csv("data/filteredRevision.csv")
allItems <- data
data <- data[,c(88:120)]

## Names of all variables
itemNames <- c(
    paste0(
        c(rep("cns", 5), rep("ext", 5), rep("agr", 5), rep("opn", 5), rep("neu", 5)),
        rep(c("05", "09", "13", "17", "19"), 5),
        rep("01", 18),
        c(rep("", 5), rep("", 5), rep("r", 5), rep("", 5), rep("", 5))
    ),
    paste0(
        c(rep("agr", 5), rep("cns", 5), rep("ext", 5), rep("opn", 5), rep("neu", 5)),
        rep(c("05", "09", "13", "17", "19"), 5),
        rep("02", 18),
        c(rep("", 5), rep("r", 5), rep("", 5), rep("", 5), rep("", 5))
    ),
    paste0(
        c(rep("cns", 5), rep("ext", 5), rep("agr", 5), rep("opn", 5), rep("neu", 5)),
        rep(c("05", "09", "13", "17", "19"), 5),
        rep("03", 18),
        c(rep("", 5), rep("r", 5), rep("", 5), rep("", 5), rep("r", 5))
    ),
    paste0("relig", c("05", "07", "09", "11", "13", "15", "17", "19")),
    paste0("relig", c("05", "07", "09", "11", "13", "15", "17", "19"), "r")
)


## Select variable specific sets of names
agrNames <- itemNames[grep("agr", itemNames)]
cnsNames <- itemNames[grep("cns", itemNames)]
extNames <- itemNames[grep("ext", itemNames)]
neuNames <- itemNames[grep("neu", itemNames)]
opnNames <- itemNames[grep("opn", itemNames)]
relNames <- itemNames[grep("rel", itemNames)][1:8] ## Only select reverse-scored


################################################################################
## Trait analyses
################################################################################

## Agreeableness
## Get data
agr <- data %>%
    select(contains("agr") | contains("relig"))
names(agr) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

## MplusAutomation::prepareMplusData(agr, "data/agr.dat")

## Run starts
agrMplus <- run_starts_mplus(agr,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

religMplus <- run_startsy_mplus(
    agr,
    8
)

## Run starts with latent variables
agrItems <- allItems %>%
    select(
        all_of(agrNames),
        contains("relig")
    )

names(agrItems) <- c(
    paste0(
        "x",
        c(1, 3, 5, 7, 8),
        c(
            rep("a", 5),
            rep("b", 5),
            rep("c", 5)
        )
    ),
    paste0(
        "y",
        rep(1:8, 2),
        rep(c("_orig", ""), each = 8)
    )
)
    
                          

## Univariate
agrStartsLatent <- run_startsx_mplus(agrItems,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    xIndicators = 3,
    analysis = "MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=20000;"
)

## Bivariate
agrStartsLatent <- run_starts_mplus(agrItems,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8),
    xIndicators = 3,
    yIndicators = 1,
    analysis = "MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=20000;"
)


agrFit <- sem(startsUniObserved,
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

## Extraversion
ext <- data %>%
    select(contains("ext") | contains("relig"))
names(ext) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

extMplus <- MplusAutomation::prepareMplusData(ext, "data/ext.dat")

extMplus <- run_starts_mplus(ext,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

extItems <- allItems[,extNames]

names(extItems) <- paste0("x",
    c(1, 3, 5, 7, 8),
    c(
        rep("a", 5),
        rep("b", 5),
        rep("c", 5)
    )
)
                          

test <- run_startsx_mplus(extItems,
                          8,
                          xWaves = c(1, 3, 5, 7, 8),
                          xIndicators = 3,
                          analysis="MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=20000;")




## Extraversion
extFit <- sem(startsUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = ext,
    em.h1.iter.max = 20000
    )

est.ext.starts.obs <- standardizedSolution(extFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.ext.starts.obs <- fitMeasures(extFit)
ext.starts.obs.results <- list(
    est.ext.starts.obs,
    fit.ext.starts.obs
)


## Conscientiousness
cns <- data %>%
    select(contains("cns") | contains("relig"))
names(cns) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

MplusAutomation::prepareMplusData(cns, "data/cns.dat")

cnsMplus <- run_starts_mplus(cns,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8),
    analysis="MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=10000;"
)


## Neuroticism
neu <- data %>%
    select(contains("neu") | contains("relig"))
names(neu) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

MplusAutomation::prepareMplusData(neu, "data/neu.dat")

neuMplus <- run_starts_mplus(neu,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8),
    analysis="MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=30000;"
)


## Openness
opn <- data %>%
    select(contains("opn") | contains("relig"))
names(opn) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

MplusAutomation::prepareMplusData(opn, "data/opn.dat")

opnMplus <- run_starts_mplus(opn,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)


## ARTS
agrArts <- run_arts_mplus(agr,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

extArts <- run_arts_mplus(ext,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

cnsArts <- run_arts_mplus(cns,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

neuArts <- run_arts_mplus(neu,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

opnArts <- run_arts_mplus(opn,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)
