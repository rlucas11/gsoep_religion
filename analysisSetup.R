################################################################################
## Setup
################################################################################

## Load libraries
library(lavaan)
library(tidyverse)
library(purrr)
library(psych)

## Read Data
data <- read_csv("data/final.csv")
bula <- read_csv("data/bula_w.csv")

## Rename religion variales so recoded versions are used in model
names(data)[65:72] <- c(paste0(rep("relig", 4),
                                c("05", "09", "13", "17"),
                                rep("_orig", 4)),
                         paste0(rep("relig", 4),
                                c("05", "09", "13", "17")))

## Add state info to dataframe
data <- left_join(data, bula[,c("pid", "no_move", "first.state")], by="pid")
data <- filter(data, no_move==1)
