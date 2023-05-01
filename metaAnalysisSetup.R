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

bulaList <- data.frame(state = c(1:16),
                       stateName = c("Schleswig-Holstein",
                                      "Hamburg",
                                      "Lower Saxony",
                                      "Bremen",
                                      "North Rhine-Westphalia",
                                      "Hesse",
                                      "Rhineland-Palatinate",
                                      "Baden-Wuerttemberg",
                                      "Bavaria",
                                      "Saarland",
                                      "Berlin",
                                      "Brandenburg",
                                      "Mecklenburg-Western Pomerania",
                                      "Saxony",
                                      "Saxony-Anhalt",
                                     "Thuringia"),
                       stringsAsFactors = FALSE)
bulaList <- left_join(bulaList, religiosity, by="state")
bulaList <- bulaList %>%
    arrange(desc(z.relig)) %>%
    mutate(stateName = factor(stateName, unique(stateName)))

bulaLevels <- data.frame(levels=levels(bulaList$stateName))
write_csv(bulaLevels, "info/stateReligiosity.csv")

################################################################################
## Load warnings
################################################################################

## List warnings
load("results/clpm.warnings.RData")
problems <- as.numeric(grep(
    "instabilities|positive\ definite|variances\ are\ negative",
    clpm.warnings
))
load("results/riclpm.warnings.RData")
problemsRi <- as.numeric(grep(
    "instabilities|positive\ definite|variances\ are\ negative",
    riclpm.warnings
))
load("results/clpm.observed.warnings.RData")
problemsObs <- as.numeric(grep(
    "instabilities|positive\ definite|variances\ are\ negative",
    clpm.warnings
))
load("results/riclpm.observed.warnings.RData")
problemsRiObs <- as.numeric(grep(
    "instabilities|positive\ definite|variances\ are\ negative",
    riclpm.warnings
))
load("results/clpm.single.warnings.RData")
warningsList <- lapply(
    stateWarnings,
    function(x) grep("instabilities|positive\ definite|variances\ are\ negative", x)
)
load("results/riclpm.single.warnings.RData")
warningsListRi <- lapply(
    stateWarnings,
    function(x) grep("instabilities|positive\ definite|variances\ are\ negative", x)
)
load("results/clpm.single.obs.warnings.RData")
warningsListObs <- lapply(
    stateWarnings,
    function(x) grep("instabilities|positive\ definite|variances\ are\ negative", x)
)
load("results/riclpm.single.obs.warnings.RData")
warningsListRiObs <- lapply(
    stateWarnings,
    function(x) grep("instabilities|positive\ definite|variances\ are\ negative", x)
)


################################################################################
## Load Data
################################################################################

dataC <- read_csv("results/clpm.latent.states.aggregated.estimates.csv")
dataRi <- read_csv("results/riclpm.latent.states.aggregated.estimates.csv")
dataObs <- read_csv("results/clpm.observed.states.aggregated.estimates.csv")
dataRiObs <- read_csv("results/riclpm.observed.states.aggregated.estimates.csv")
dataSingle <- read_csv("results/clpm.states.single.estimates.csv")
dataRiSingle <- read_csv("results/riclpm.states.single.estimates.csv")
dataSingleObs <- read_csv("results/clpm.states.single.obs.estimates.csv")
dataRiSingleObs <- read_csv("results/riclpm.states.single.obs.estimates.csv")

resultsList <- data.frame(
    results = c(
        "dataC",
        "dataRi",
        "dataObs",
        "dataRiObs",
        "dataSingle",
        "dataRiSingle",
        "dataSingleObs",
        "dataRiSingleObs"
    ),
    warnings = c(
        "problems",
        "problemsRi",
        "problemsObs",
        "problemsRiObs",
        "warningsList",
        "warningsListRi",
        "warningsListObs",
        "warningsListRiObs"
    ),
    model = rep(c(
        "CLPM",
        "RICLPM"
    ), 4),
    type = rep(
        rep(c("Latent", "Observed"),
            each = 2
        ), 2
    ),
    traits = rep(
        c("All", "Single"),
        each = 4
    )
)


################################################################################
## Restructure and combine data
################################################################################

## Function to restructure results to long form
## Also adds state and warnings info
restructureResults <- function(data, problems, model, type) {
    data.l <- data %>%
        left_join(bulaList[, c("state", "stateName", "z.relig")],
            by = "state"
            ) %>%
        mutate(
            Model = model,
            Type = type,
            Traits = "All"
        ) %>%
        pivot_longer(
            cols = c(starts_with("agr"),
                     starts_with("cns"),
                     starts_with("ext"),
                     starts_with("neu"),
                     starts_with("opn")
                     ),
            ##cols=agr.r1:opn.ri.r.ub,
            names_to = c("Trait", "Parameter"),
            names_pattern="([[:alpha:]]*)\\.(.*)"
        )
    data.l[!(data.l$state %in% problems), "select"] <- 1
    return(data.l[, c(
        "state",
        "stateName",
        "z.relig",
        "Model",
        "Type",
        "Traits",
        "Trait",
        "Parameter",
        "value",
        "select",
        "samplesize"
    )])
}

## Function to restructure single-trait results to long form
## Also adds state and warnings info
restructureResultsSingle <- function(data, problems, model, type) {
    nTable <- data %>%
        select(state, contains("samplesize")) %>%
        pivot_longer(
            cols = contains("samplesize"),
            names_to = c("Trait", "var"),
            values_to = "samplesize",
            names_pattern = "([[:alpha:]]*)\\.(.*)"
        )
    for (i in c("agr", "cns", "ext", "neu", "opn")) {
        nTable[
            nTable$Trait == i &
                !(nTable$state %in% problems[[i]]),
            "select"
        ] <- 1
    }
    data.l <- data %>%
        left_join(bulaList[, c("state", "stateName", "z.relig")],
            by = "state"
        ) %>%
        mutate(
            Model = model,
            Type = type,
            Traits = "Single"
        ) %>%
        select(!contains("samplesize")) %>%
        pivot_longer(
            cols = c(starts_with("agr"),
                     starts_with("cns"),
                     starts_with("ext"),
                     starts_with("neu"),
                     starts_with("opn")
                     ),
            ##cols = agr.r1:opn.ri.r.ub,
            names_to = c("Trait", "Parameter"),
            names_pattern = "([[:alpha:]]*)\\.(.*)"
        ) %>%
        left_join(nTable[, -3],
            by = c("state", "Trait")
            )
    return(data.l[, c(
        "state",
        "stateName",
        "z.relig",
        "Model",
        "Type",
        "Traits",
        "Trait",
        "Parameter",
        "value",
        "select",
        "samplesize"
    )])
}


for (i in 1:nrow(resultsList)) {
    if (str_detect(resultsList[i, 1], "Single")) {
        data <- restructureResultsSingle(
            data = eval(parse(text = resultsList[[i, 1]])),
            problems = eval(parse(text = resultsList[[i, 2]])),
            model = resultsList[[i, 3]],
            type = resultsList[[i, 4]]
        )
    } else {
        data <- restructureResults(
            data = eval(parse(text = resultsList[[i, 1]])),
            problems = eval(parse(text = resultsList[[i, 2]])),
            model = resultsList[[i, 3]],
            type = resultsList[[i, 4]]
        )
    }
    if(i == 1) {
        combinedResults <- data
    } else {
        combinedResults <- rbind(combinedResults, data)
    }
}

## Remove attributes for religiosity

write.csv(data.frame(combinedResults), "data/combinedResults.csv", row.names=FALSE)


load("results/clpm.latent.results.RData")
load("results/riclpm.latent.results.RData")
