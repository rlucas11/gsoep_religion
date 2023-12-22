library(tidyverse)
library(MplusAutomation)

## Read cleaned data
data <- read_csv("data/filteredData.csv")


source("~/Projects/code-generator/buildMplusDpm.R")

traitModelNames <- c(
     "x1a", "x2a", "x3a", "x4a",
     "x1b", "x2b", "x3b", "x4b",
     "x1c", "x2c", "x3c", "x4c",
    "x1", "x2", "x3", "x4", "trMiss",
     "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
     "y1", "y2", "y3", "y4"
 )

## Agreeableness
agr <- data %>%
    select(contains("agr") | contains("relig"))

names(agr) <- traitModelNames

agrOut <- run_dpm_mplus(agr, waves = 4, xIndicators = 3)


## Conscientiousness
cns <- data %>%
    select(contains("cns") | contains("relig"))

names(cns) <- traitModelNames

cnsOut <- run_dpm_mplus(cns, waves = 4, xIndicators = 3)

## Extraverson
ext <- data %>%
    select(contains("ext") | contains("relig"))

names(ext) <- traitModelNames

extOut <- run_dpm_mplus(ext, waves = 4, xIndicators = 3)


## Neuroticism
neu <- data %>%
    select(contains("neu") | contains("relig"))

names(neu) <- traitModelNames

neuOut <- run_dpm_mplus(neu, waves = 4, xIndicators = 3)

## Openness
opn <- data %>%
    select(contains("opn") | contains("relig"))

names(opn) <- traitModelNames

opnOut <- run_dpm_mplus(opn, waves = 4, xIndicators = 3)
