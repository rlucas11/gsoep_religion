library(metafor)
library(tidyverse)

################################################################################
## Setup
################################################################################

## Load state religiosity data
load("results/correlationsByState.RData")
religiosity <- out$mean %>%
    data.frame() %>%
    mutate(z.relig = scale(relig))
names(religiosity) <- c(
    "agr", "cns", "ext", "neu", "opn", "relig",
    "state", "z.relig"
)

## Variable names for meta-analysis loop
varNames <- data.frame(
    traitNames = c("agr", "cns", "ext", "neu", "opn"),
    traitVar = paste(c("agr", "cns", "ext", "neu", "opn"),
        "rt.cl",
        sep = "."
        ),
    sampleSizeSingle = paste(c("agr", "cns", "ext", "neu", "opn"),
        "samplesize",
        sep = "."
        ),
    sampleSize = "samplesize"
)

################################################################################
## Functions
################################################################################

stateMeta <- function(trait, data, varNames, single=FALSE, moderator=FALSE) {
    ifelse(single == TRUE,
        ni <- varNames[varNames$traitNames == trait, 3],
        ni <- varNames[varNames$traitNames == trait, 4]
    )
    yivi <- escalc(measure = "COR",
                   ri = eval(parse(text=varNames[varNames$traitNames==trait, 2])),
                   ni = eval(parse(text=ni)),
                   data = data)
    ifelse(moderator == FALSE,
        meta <- rma(yi, vi, data = yivi, method = "ML"),
        meta <- rma(yi, vi, data = yivi, method = "ML", mods = z.relig)
    )
    summary(meta)
}

                       

################################################################################
## Original Model: CLPM, Latent Traits, All Traits
################################################################################

data <- read_csv("results/clpm.latent.states.aggregated.estimates.csv")
data <- left_join(data, religiosity[, c("state", "z.relig")], by = "state")

stateMeta("agr", data, varNames)
stateMeta("agr", data, varNames, moderator = TRUE)

stateMeta("cns", data, varNames)
stateMeta("cns", data, varNames, moderator = TRUE)

stateMeta("ext", data, varNames)
stateMeta("ext", data, varNames, moderator = TRUE)

stateMeta("neu", data, varNames)
stateMeta("neu", data, varNames, moderator = TRUE)

stateMeta("opn", data, varNames)
stateMeta("opn", data, varNames, moderator = TRUE)


################################################################################
## RICLPM With Latent Traits and All Traits Simultaneously
################################################################################

dataRi <- read_csv("results/riclpm.latent.states.aggregated.estimates.csv")
dataRi <- left_join(dataRi, religiosity[, c("state", "z.relig")], by = "state")

stateMeta("agr", dataRi, varNames)
stateMeta("agr", dataRi, varNames, moderator = TRUE)

stateMeta("cns", dataRi, varNames)
stateMeta("cns", dataRi, varNames, moderator = TRUE)

stateMeta("ext", dataRi, varNames)
stateMeta("ext", dataRi, varNames, moderator = TRUE)

stateMeta("neu", dataRi, varNames)
stateMeta("neu", dataRi, varNames, moderator = TRUE)

stateMeta("opn", dataRi, varNames)
stateMeta("opn", dataRi, varNames, moderator = TRUE)



################################################################################
## CLPM With Observed Traits and All Traits Simultaneously
################################################################################

dataObs <- read_csv("results/clpm.observed.states.aggregated.estimates.csv")
dataObs <- left_join(dataObs, religiosity[, c("state", "z.relig")], by = "state")

stateMeta("agr", dataObs, varNames)
stateMeta("agr", dataObs, varNames, moderator = TRUE)

stateMeta("cns", dataObs, varNames)
stateMeta("cns", dataObs, varNames, moderator = TRUE)

stateMeta("ext", dataObs, varNames)
stateMeta("ext", dataObs, varNames, moderator = TRUE)

stateMeta("neu", dataObs, varNames)
stateMeta("neu", dataObs, varNames, moderator = TRUE)

stateMeta("opn", dataObs, varNames)
stateMeta("opn", dataObs, varNames, moderator = TRUE)


################################################################################
## RICLPM With Observed Traits and All Traits Simultaneously
################################################################################

dataRiObs <- read_csv("results/riclpm.observed.states.aggregated.estimates.csv")
dataRiObs <- left_join(dataRiObs, religiosity[, c("state", "z.relig")], by = "state")

stateMeta("agr", dataRiObs, varNames)
stateMeta("agr", dataRiObs, varNames, moderator = TRUE)

stateMeta("cns", dataRiObs, varNames)
stateMeta("cns", dataRiObs, varNames, moderator = TRUE)

stateMeta("ext", dataRiObs, varNames)
stateMeta("ext", dataRiObs, varNames, moderator = TRUE)

stateMeta("neu", dataRiObs, varNames)
stateMeta("neu", dataRiObs, varNames, moderator = TRUE)

stateMeta("opn", dataRiObs, varNames)
stateMeta("opn", dataRiObs, varNames, moderator = TRUE)



################################################################################
## RICLPM: Single-Trait Models
################################################################################

dataRiSingle <- read_csv("results/riclpm.states.single.estimates.csv")
dataRiSingle <- left_join(dataRiSingle, religiosity[, c("state", "z.relig")], by = "state")

stateMeta("agr", dataRiSingle, varNames, single = TRUE, moderator = FALSE)
stateMeta("agr", dataRiSingle, varNames, single = TRUE, moderator = TRUE)

stateMeta("cns", dataRiSingle, varNames, single = TRUE, moderator = FALSE)
stateMeta("cns", dataRiSingle, varNames, single = TRUE, moderator = TRUE)

stateMeta("ext", dataRiSingle, varNames, single = TRUE, moderator = FALSE)
stateMeta("ext", dataRiSingle, varNames, single = TRUE, moderator = TRUE)

stateMeta("neu", dataRiSingle, varNames, single = TRUE, moderator = FALSE)
stateMeta("neu", dataRiSingle, varNames, single = TRUE, moderator = TRUE)

stateMeta("opn", dataRiSingle, varNames, single = TRUE, moderator = FALSE)
stateMeta("opn", dataRiSingle, varNames, single = TRUE, moderator = TRUE)



################################################################################
## Scratch
################################################################################
