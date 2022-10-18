################################################################################
## Setup
################################################################################

## Load libraries
library(tidyverse)
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

## Create cross-wave means for personality measures

data <- data %>%
    rowwise() %>%
    mutate(agr=mean(c(agr05, agr09, agr13, agr17), na.rm=TRUE),
           cns=mean(c(cns05, cns09, cns13, cns17), na.rm=TRUE),
           ext=mean(c(ext05, ext09, ext13, ext17), na.rm=TRUE),
           neu=mean(c(neu05, neu09, neu13, neu17), na.rm=TRUE),
           opn=mean(c(opn05, opn09, opn13, opn17), na.rm=TRUE),
           relig=mean(c(relig05, relig09, relig13, relig17), na.rm=TRUE)
           )

cor(data[,c("agr", "cns", "ext", "neu", "opn", "relig")], use="pair")

## Temporary for testing
data$first.state <- sample(1:17, nrow(data), replace=TRUE)
bula_neu <- sort(unique(data$fake.state))


out <- statsBy(data[,c("agr", "cns", "ext", "neu", "opn", "relig", "first.state")],
               group="first.state",
               cors=TRUE)

print(out, short=FALSE)

