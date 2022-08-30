################################################################################
## Load Packages
################################################################################

library(tidyverse)
library(haven)
library(psych)


################################################################################
## Set Basic Info
################################################################################

## Match waves, years, and files
soepVersion <- 37
yearsWaves <- data.frame(
  years = c(1984:((1984 + (soepVersion - 1)))),
  wave = c(letters, paste0(
    "b",
    letters[1:(soepVersion - 26)]
  )),
  numericWave = 1:soepVersion
)

## Set path to data
rawPath <- file.path("/home","rich","data","GSOEP","GSOEP37")
## Get info about variables
varInfo <- read_csv("data/variables.csv")


################################################################################
## Define Functions
################################################################################

## Get variable
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

## ## Replace Missing, names each variable 'x'
## replaceMissing <- function(data, varName) {    
##   mutate(data, x = as.numeric(replace(
##     .data[[varName]],
##     which(.data[[varName]] < 0), NA
##   )))
## }

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

for (i in 1:(nrow(varInfo) - 1)) {
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

################################################################################
## Create Final Data file
################################################################################
varFiles <- list.files(path = "data", pattern = "pl[[:print:]]*_w.csv")
varNames <- sapply(strsplit(varFiles, "_w"), "[[", 1)
reverse <- c(3, 6, 10, 14)
varNames[reverse]


rm(combo)
for (f in varFiles) {
    data <- read_csv(paste0("data/", f))
    if (f %in% paste0(varNames[reverse], "_w.csv")) {
        varStem <- strsplit(f, "_w")[[1]][1]
        varList <- paste0(varStem, c("_2005", "_2009", "_2013", "_2017"))
        ## Finish this; add recodes for reveres scoring
        }
    ifelse(!exists("combo"),
        combo <- data,
        combo <- full_join(combo,
            select(data, pid, starts_with("pl")),
            by = "pid"
        )
    )
}


