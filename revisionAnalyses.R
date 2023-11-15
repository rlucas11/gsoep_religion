################################################################################
## Load Packages
################################################################################

library(tidyverse)
library(haven)
library(psych)


################################################################################
## Set Basic Info
################################################################################

## Create list of waves, years, and files
soepVersion <- 37
yearsWaves <- data.frame(
  years = c(1984:((1984 + (soepVersion - 1)))),
  wave = c(letters, paste0(
    "b",
    letters[1:(soepVersion - 26)]
  )),
  numericWave = 1:soepVersion
)

## Set path to data to keep confidential data separate from code repository
## Set this to wherever your data are stored
rawPath <- file.path("/home","rich","data","GSOEP","GSOEP37")
## Get info about variables
varInfo <- read_csv("info/variables.csv")


################################################################################
## Define Functions for Cleaning
################################################################################

## Get variable, along with sample, sex, and age (from ppath file)
getVar <- function(varName, fileName) {
  fileName <- paste0(fileName, ".dta")
  path <- file.path(rawPath, "Stata", fileName)
  data <- read_dta(path, col_select = c("pid", "hid", "syear", all_of(varName)))
  pathPath <- file.path(rawPath, "Stata", "ppath.dta")
  ppath <- read_dta(pathPath,
    col_select = c("pid", "psample", "sex", "gebjahr")
  )
  data <- left_join(data, ppath, by = "pid")
}

## Replace missing, keeping varname
replaceMissing <- function(data, varName) {
    mutate(data, {{ varName }} := replace(
        .data[[varName]],
        which(.data[[varName]] < 0), NA
    ))
}


## Transform to wide
soepStartsData <- function(data, varName) {
  data %>%
    arrange(pid, syear) %>%
    filter(!is.na({{varName}})) %>%
    pivot_wider(
      names_from = syear,
      id_cols = c(pid, sex, gebjahr, psample),
      names_prefix = paste0(varName, "_"),
      names_sep = "_",
      values_from = {{varName}},
      names_sort = TRUE
    )
}

## Recode 7-point variables
recode7 <- function(data, varName) {
    data <- mutate(data, "{{varName}}_r" := 8 - {{ varName }})
}

################################################################################
## Extract Data
################################################################################

for (i in 1:(nrow(varInfo))) {
    varName <- varInfo[[i, 2]]
    varFile <- varInfo[[i, 1]]
    if (!file.exists(paste0("data/", varName, "_r_l.csv"))) {
        data <- getVar(varName, varFile)
        data <- data %>%
            filter(syear == 2005 |
                   syear == 2007 |
                   syear == 2009 |
                   syear == 2011 |
                   syear == 2013 |
                   syear == 2015 |
                   syear == 2017 |
                   syear == 2019)
        data <- replaceMissing(data, varName)
        write_csv(data, paste0("data/", varName, "_r_l.csv"))
    }
    if(!file.exists(paste0("data/", varName, "_r_w.csv"))) {
        wide <- soepStartsData(data, varName)
        write_csv(wide, paste0("data/", varName, "_r_w.csv"))
    }
}



## Additional cleaning for religion variable
## Recode '1' in 2013 and 2017 due to different scales
religion <- read_csv("data/pli0098_h_r_w.csv")
religion[which(religion$pli0098_h_2013 == 1), "pli0098_h_2013"] <- 2
religion[which(religion$pli0098_h_2017 == 1), "pli0098_h_2017"] <- 2
religion[which(religion$pli0098_h_2019 == 1), "pli0098_h_2019"] <- 2
## Reverse Score
religion <- religion %>%
    mutate(
        pli0098_h_2005r = 6 - pli0098_h_2005,
        pli0098_h_2007r = 6 - pli0098_h_2007,
        pli0098_h_2009r = 6 - pli0098_h_2009,
        pli0098_h_2011r = 6 - pli0098_h_2011,
        pli0098_h_2013r = 6 - pli0098_h_2013,
        pli0098_h_2015r = 6 - pli0098_h_2015,
        pli0098_h_2017r = 6 - pli0098_h_2017,
        pli0098_h_2019r = 6 - pli0098_h_2019
    )
write_csv(religion, "data/pli0098_h_r_w.csv")


################################################################################
## Create Final Data file
################################################################################
varFiles <- list.files(path = "data", pattern = "pl[[:print:]]*r_w.csv")
varNames <- sapply(strsplit(varFiles, "_r_w"), "[[", 1)
reverse <- c(3, 7, 12, 15)
varNames[reverse]

## Combine files; Reverse score personality items
for (f in varFiles) {
    data <- read_csv(paste0("data/", f))
    if (f %in% paste0(varNames[reverse], "_w.csv")) {
        varStem <- strsplit(f, "_w")[[1]][1]
        varList <- paste0(varStem, c("_2005", "_2009", "_2013", "_2017", "_2019"))
        for (i in varList) {
            data <- data %>%
                mutate({{ i }} := 8 - .data[[i]])
        }
        names(data)[5:8] <- paste(names(data)[5:8], "r", sep = "_")
    }
    ifelse(!exists("combo"),
        combo <- data,
        combo <- full_join(combo,
            select(data, pid, starts_with("pl")),
            by = "pid"
        )
    )
}


names(combo) <- c(
    c("pid", "sex", "gebjahr", "psample"),
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



################################################################################
## Create Scale Scores
################################################################################

combo <- combo %>%
    mutate(
        cns05 = rowMeans(.[, c("cns0501", "cns0502r", "cns0503")], na.rm = TRUE),
        cns09 = rowMeans(.[, c("cns0901", "cns0902r", "cns0903")], na.rm = TRUE),
        cns13 = rowMeans(.[, c("cns1301", "cns1302r", "cns1303")], na.rm = TRUE),
        cns17 = rowMeans(.[, c("cns1701", "cns1702r", "cns1703")], na.rm = TRUE),
        cns19 = rowMeans(.[, c("cns1901", "cns1902r", "cns1903")], na.rm = TRUE),
        ##
        ext05 = rowMeans(.[, c("ext0501", "ext0502", "ext0503r")], na.rm = TRUE),
        ext09 = rowMeans(.[, c("ext0901", "ext0902", "ext0903r")], na.rm = TRUE),
        ext13 = rowMeans(.[, c("ext1301", "ext1302", "ext1303r")], na.rm = TRUE),
        ext17 = rowMeans(.[, c("ext1701", "ext1702", "ext1703r")], na.rm = TRUE),
        ext19 = rowMeans(.[, c("ext1901", "ext1902", "ext1903r")], na.rm = TRUE),
        ##
        agr05 = rowMeans(.[, c("agr0501r", "agr0502", "agr0503")], na.rm = TRUE),
        agr09 = rowMeans(.[, c("agr0901r", "agr0902", "agr0903")], na.rm = TRUE),
        agr13 = rowMeans(.[, c("agr1301r", "agr1302", "agr1303")], na.rm = TRUE),
        agr17 = rowMeans(.[, c("agr1701r", "agr1702", "agr1703")], na.rm = TRUE),
        agr19 = rowMeans(.[, c("agr1901r", "agr1902", "agr1903")], na.rm = TRUE),
        ##
        neu05 = rowMeans(.[, c("neu0501", "neu0502", "neu0503r")], na.rm = TRUE),
        neu09 = rowMeans(.[, c("neu0901", "neu0902", "neu0903r")], na.rm = TRUE),
        neu13 = rowMeans(.[, c("neu1301", "neu1302", "neu1303r")], na.rm = TRUE),
        neu17 = rowMeans(.[, c("neu1701", "neu1702", "neu1703r")], na.rm = TRUE),
        neu19 = rowMeans(.[, c("neu1901", "neu1902", "neu1903r")], na.rm = TRUE),
        ##
        opn05 = rowMeans(.[, c("opn0501", "opn0502", "opn0503")], na.rm = TRUE),
        opn09 = rowMeans(.[, c("opn0901", "opn0902", "opn0903")], na.rm = TRUE),
        opn13 = rowMeans(.[, c("opn1301", "opn1302", "opn1303")], na.rm = TRUE),
        opn17 = rowMeans(.[, c("opn1701", "opn1702", "opn1703")], na.rm = TRUE),
        opn19 = rowMeans(.[, c("opn1901", "opn1902", "opn1903")], na.rm = TRUE)
    )

write_csv(combo, "data/final_r.csv")

## Check correlations for big five items
combo %>%
    select(starts_with("ext")) %>%
    cor(use = "pair")
combo %>%
    select(starts_with("agr")) %>%
    cor(use = "pair")
combo %>%
    select(starts_with("cns")) %>%
    cor(use = "pair")
combo %>%
    select(starts_with("neu")) %>%
    cor(use = "pair")
combo %>%
    select(starts_with("opn")) %>%
    cor(use = "pair")

## Check one set of cross-sectional correlationsji
combo %>%
    select(contains("05")) %>%
    cor(use = "pair")

################################################################################
## hbrutto file for federal state
################################################################################

## Pull State info
hbrutto <- read_dta(file.path(rawPath, "Stata", "hbrutto.dta"),
    col_select = c("hid", "syear", "bula_h")
)

## Pull pids and hids for matching
hids <- read_dta(file.path(rawPath, "Stata", "pl.dta"),
    col_select = c("pid", "hid", "syear")
)
hids <- arrange(hids, syear, pid)

## Merge state info with pids by hid
bula <- full_join(hids, hbrutto, by = c("hid", "syear"))

## Remove household ids with no pid
bula <- filter(bula, !is.na(pid))

## Save long version of bula
write_csv(bula, "data/bula.csv")

bula <- read_csv("data/bula.csv")

## Find first state
first.state <- bula %>%
    group_by(pid) %>%
    mutate(firstYear = min(syear)) %>%
    ungroup() %>%
    filter(syear==firstYear)
names(first.state) <- c("pid", "hid", "syear", "first.state", "firstYear")

## Create wide version
bula_w <- bula %>%
    arrange(pid, syear) %>%
    ##    replaceMissing("bula_h") %>%
    filter(!is.na(bula_h)) %>%
    pivot_wider(
        names_from = syear,
        id_cols = pid,
        names_prefix = paste0("bula_h", "_"),
        names_sep = "_",
        values_from = bula_h,
        names_sort = TRUE
    )

## Check whether respondent changes state (1 = no move)
## In original paper, authors say that about 2% changed state
bula_w$no_move <- apply(bula_w[, 2:38], 1, function(a) length(unique(a[!is.na(a)])))

bula_w <- left_join(bula_w, first.state[,c("pid", "first.state")], by="pid")

write_csv(bula_w, "data/bula_w.csv")

################################################################################
## Analysis Setup
################################################################################
## 
## Read Data
data <- read_csv("data/final_r.csv")
bula <- read_csv("data/bula_w.csv")

## Rename religion variales so recoded versions are used in model
names(data)[80:95] <- c(paste0(rep("relig", 8),
                                c("05", "07", "09", "11", "13", "15", "17", "19"),
                                rep("_orig", 8)),
                        paste0("relig",
                               c("05", "07", "09", "11", "13", "15", "17", "19")))


## Add state info to dataframe
data <- left_join(data, bula[,c("pid", "no_move", "first.state")], by="pid")
data <- filter(data, no_move==1)


################################################################################
## Restrict sample
################################################################################

## Names of all variables
varNames <- c(
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
agrNames <- varNames[grep("agr", varNames)]
cnsNames <- varNames[grep("cns", varNames)]
extNames <- varNames[grep("ext", varNames)]
neuNames <- varNames[grep("neu", varNames)]
opnNames <- varNames[grep("opn", varNames)]
relNames <- varNames[grep("rel", varNames)][1:8] ## Only select reverse-scored


## Calculate number of missing for each trait
data <- data %>%
    rowwise() %>%
    mutate(agrMiss = sum(is.na(c_across(all_of(agrNames)))),
           cnsMiss = sum(is.na(c_across(all_of(cnsNames)))),
           extMiss = sum(is.na(c_across(all_of(extNames)))),
           neuMiss = sum(is.na(c_across(all_of(neuNames)))),
           opnMiss = sum(is.na(c_across(all_of(opnNames)))),
           relMiss = sum(is.na(c_across(all_of(relNames)))))

## Create selection variable
## Selected if they have at least one item per construct
## As per original study
data[which(data$agrMiss < 15 &
    data$cnsMiss < 15 &
    data$extMiss < 15 &
    data$neuMiss < 15 &
    data$opnMiss < 15 &
    data$relMiss < 8), "missingSelect"] <- 1

data <- filter(data, missingSelect == 1)
write_csv(data, "data/filteredRevision.csv")

################################################################################
## Start here if already cleaned data once
################################################################################

source("scripts/revisionModels.R")

source("~/Projects/code-generator/buildMplus.R")
library(lavaan)

data <- read_csv("data/filteredRevision.csv")
allItems <- data
data <- data[,c(88:120)]

## Agreeableness
agr <- data %>%
    select(contains("agr") | contains("relig"))
names(agr) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)



MplusAutomation::prepareMplusData(agr, "data/agr.dat")

agrMplus <- run_starts_mplus(agr,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)

agrItems <- allItems[,agrNames]

names(agrItems) <- paste0("x",
    c(1, 3, 5, 7, 8),
    c(
        rep("a", 5),
        rep("b", 5),
        rep("c", 5)
    )
)
                          

test <- run_startsx_mplus(agrItems,
                          8,
                          xWaves = c(1, 3, 5, 7, 8),
                          xIndicators = 3,
                          analysis="MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=20000;")


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
run_starts_mplus(ext,
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
run_starts_mplus(cns,
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

run_starts_mplus(neu,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8),
    analysis="MODEL=NOCOVARIANCES;\nCOVERAGE=.001;\nITERATIONS=10000;"
)


## Openness
opn <- data %>%
    select(contains("opn") | contains("relig"))
names(opn) <- c(
    paste0("x", c(1, 3, 5, 7, 8)),
    paste0("y", 1:8)
)

MplusAutomation::prepareMplusData(opn, "data/opn.dat")

run_starts_mplus(opn,
    8,
    xWaves = c(1, 3, 5, 7, 8),
    yWaves = c(1, 2, 3, 4, 5, 6, 7, 8)
)
