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
## Define Functions
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
    if (!file.exists(paste0("data/", varName, "_l.csv"))) {
        data <- getVar(varName, varFile)
        data <- data %>%
            filter(syear == 2005 | syear == 2009 | syear == 2013 | syear == 2017)
        data <- replaceMissing(data, varName)
        write_csv(data, paste0("data/", varName, "_l.csv"))
    }
    if(!file.exists(paste0("data/", varName, "_w.csv"))) {
        wide <- soepStartsData(data, varName)
        write_csv(wide, paste0("data/", varName, "_w.csv"))
    }
}



## Additional cleaning for religion variable
## Recode '1' in 2013 and 2017 due to different scales
religion <- read_csv("data/pli0098_h_w.csv")
religion[which(religion$pli0098_h_2013 == 1), "pli0098_h_2013"] <- 2
religion[which(religion$pli0098_h_2017 == 1), "pli0098_h_2017"] <- 2
## Reverse Score
religion <- religion %>%
    mutate(
        pli0098_h_2005r = 6 - pli0098_h_2005,
        pli0098_h_2009r = 6 - pli0098_h_2009,
        pli0098_h_2013r = 6 - pli0098_h_2013,
        pli0098_h_2017r = 6 - pli0098_h_2017
    )
write_csv(religion, "data/pli0098_h_w.csv")


################################################################################
## Create Final Data file
################################################################################
varFiles <- list.files(path = "data", pattern = "pl[[:print:]]*_w.csv")
varNames <- sapply(strsplit(varFiles, "_w"), "[[", 1)
reverse <- c(3, 7, 12, 15)
varNames[reverse]

## Combine files; Reverse score personality items
for (f in varFiles) {
    data <- read_csv(paste0("data/", f))
    if (f %in% paste0(varNames[reverse], "_w.csv")) {
        varStem <- strsplit(f, "_w")[[1]][1]
        varList <- paste0(varStem, c("_2005", "_2009", "_2013", "_2017"))
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
        c(rep("cns", 4), rep("ext", 4), rep("agr", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("01", 15),
        c(rep("", 4), rep("", 4), rep("r", 4), rep("", 4), rep("", 4))
    ),
    paste0(
        c(rep("agr", 4), rep("cns", 4), rep("ext", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("02", 15),
        c(rep("", 4), rep("r", 4), rep("", 4), rep("", 4), rep("", 4))
    ),
    paste0(
        c(rep("cns", 4), rep("ext", 4), rep("agr", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("03", 15),
        c(rep("", 4), rep("r", 4), rep("", 4), rep("", 4), rep("r", 4))
    ),
    paste0("relig", c("05", "09", "13", "17")),
    paste0("relig", c("05", "09", "13", "17"), "r")
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
        ##
        ext05 = rowMeans(.[, c("ext0501", "ext0502", "ext0503r")], na.rm = TRUE),
        ext09 = rowMeans(.[, c("ext0901", "ext0902", "ext0903r")], na.rm = TRUE),
        ext13 = rowMeans(.[, c("ext1301", "ext1302", "ext1303r")], na.rm = TRUE),
        ext17 = rowMeans(.[, c("ext1701", "ext1702", "ext1703r")], na.rm = TRUE),
        ##
        agr05 = rowMeans(.[, c("agr0501r", "agr0502", "agr0503")], na.rm = TRUE),
        agr09 = rowMeans(.[, c("agr0901r", "agr0902", "agr0903")], na.rm = TRUE),
        agr13 = rowMeans(.[, c("agr1301r", "agr1302", "agr1303")], na.rm = TRUE),
        agr17 = rowMeans(.[, c("agr1701r", "agr1702", "agr1703")], na.rm = TRUE),
        ##
        neu05 = rowMeans(.[, c("neu0501", "neu0502", "neu0503r")], na.rm = TRUE),
        neu09 = rowMeans(.[, c("neu0901", "neu0902", "neu0903r")], na.rm = TRUE),
        neu13 = rowMeans(.[, c("neu1301", "neu1302", "neu1303r")], na.rm = TRUE),
        neu17 = rowMeans(.[, c("neu1701", "neu1702", "neu1703r")], na.rm = TRUE),
        ##
        opn05 = rowMeans(.[, c("opn0501", "opn0502", "opn0503")], na.rm = TRUE),
        opn09 = rowMeans(.[, c("opn0901", "opn0902", "opn0903")], na.rm = TRUE),
        opn13 = rowMeans(.[, c("opn1301", "opn1302", "opn1303")], na.rm = TRUE),
        opn17 = rowMeans(.[, c("opn1701", "opn1702", "opn1703")], na.rm = TRUE)
    )

write_csv(combo, "data/final.csv")

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

## Save long version of bula
write_csv(bula, "data/bula.csv")

## Create wide version
bula_w <- bula %>%
    arrange(pid, syear) %>%
    ##    replaceMissing("bula_h") %>%
    filter(!is.na("bula_h")) %>%
    pivot_wider(
        names_from = syear,
        id_cols = c(pid, hid),
        names_prefix = paste0("bula_h", "_"),
        names_sep = "_",
        values_from = bula_h,
        names_sort = TRUE
    )

## Check whether respondent changes state (1 = no move)
## In original, about 2% changed state
bula_w$no_move <- apply(bula_w[, 3:39], 1, function(a) length(unique(a[!is.na(a)])))

write_csv(bula_w, "data/bula_w.csv")

