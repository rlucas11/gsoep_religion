library(metafor)
library(tidyverse)
library(ggplot2)

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
            cols=agr.r1:opn.st.ub,
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
                !(nTable$state %in% warningsList[[i]]),
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
            cols = agr.r1:opn.st.ub,
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

################################################################################
## Plots
################################################################################

results <- read_csv("data/combinedResults.csv")
bulaLevels <- read_csv("info/stateReligiosity.csv")
results$stateName <- factor(results$stateName, levels = bulaLevels$levels)
results$Model <- as.factor(results$Model)
results$Type <- as.factor(results$Type)
results$Traits <- as.factor(results$Traits)

metaSelect <- function(results,
                       parameter,
                       trait,
                       model = NULL,
                       type = NULL,
                       traits = NULL) {
    data <- results %>%
        filter(grepl(parameter, Parameter) &
            grepl(trait, Trait))
    if (!is.null(model)) {
        data <- data %>%
            filter(Model==model)
    }
    if (!is.null(type)) {
        data <- data %>%
            filter(grepl(type, Type))
    }
    if (!is.null(traits)) {
        data <- data %>%
            filter(grepl(traits, Traits))
    }
    data <- data %>%
        pivot_wider(
            names_from = Parameter,
            values_from = value
        )
    return(data)
}



temp <- metaSelect(results, "rt.cl", "opn", model="CLPM")

temp %>%
    filter(!is.na(select)) %>%
    ggplot(
        aes(
            x = stateName,
            y = rt.cl,
            ymin = rt.cl.lb,
            ymax = rt.cl.ub,
            color = Model,
            linetype = Type,
            shape = Traits
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()















################################################################################
#### CLPM with Latent Variables, All Traits
## Load warnings
load("results/clpm.warnings.RData")
## Find any with problems

## Load results
data <- read_csv("results/clpm.latent.states.aggregated.estimates.csv")
data <- data %>%
    left_join(bulaList[, c("state", "stateName", "z.relig")],
              by = "state")  %>%
    mutate(Model="CLPM",
           Type="Latent")
## Restructure
data.l <- data %>%
    pivot_longer(
        cols = agr.r1:opn.st.ub,
        names_to = c("Trait", "Parameter"),
        names_pattern = "([[:alpha:]]*)\\.(.*)"
    )
data.l[!(data.l$state %in% problems), "select"] <- 1    


#### RI-CLPM with Latent Variables, All Traits
## Load warnings
load("results/riclpm.warnings.RData")
## Find any with problems
## Load results
dataRi <- read_csv("results/riclpm.latent.states.aggregated.estimates.csv")
dataRi <- dataRi %>%
    left_join(bulaList[, c("state", "stateName", "z.relig")],
              by = "state")  %>%
    mutate(
        Model = "RICLPM",
        Type = "Latent"
    )
dataRi[!(dataRi$state %in% problemsRi), "select"] <- 1
## Restructure
dataRi.l <- dataRi %>%
    pivot_longer(cols=agr.r1:opn.st.ub)


cl.rt.models.ri <- dataRi %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="RI-CLPM",
           Type="Latent")
names(cl.rt.models.ri) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
cl.rt.models.ri[!(cl.rt.models.ri$stateId %in% problems), "select"] <- 1


dataRi <- read_csv("results/riclpm.latent.states.aggregated.estimates.csv")
dataRi <- left_join(dataRi,
                    bulaList[, c("state", "stateName", "z.relig")],
                    by = "state")

dataObs <- read_csv("results/clpm.observed.states.aggregated.estimates.csv")
dataObs <- left_join(dataObs,
                     bulaList[, c("state", "stateName", "z.relig")],
                     by = "state")

dataRiObs <- read_csv("results/riclpm.observed.states.aggregated.estimates.csv")
dataRiObs <- left_join(dataRiObs,
                       bulaList[, c("state", "stateName", "z.relig")],
                       by = "state")

dataSingle <- read_csv("results/clpm.states.single.estimates.csv")
dataSingle <- left_join(dataSingle,
                        bulaList[, c("state", "stateName", "z.relig")],
                        by = "state")

dataRiSingle <- read_csv("results/riclpm.states.single.estimates.csv")
dataRiSingle <- left_join(dataRiSingle,
                          bulaList[, c("state", "stateName", "z.relig")],
                          by = "state")


## Use grep to find errors
## grep("instabilities|positive\ definite|variances\ are\ negative", clpm.warnings)

## Load results
cl.rt.models.orig <- data %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="CLPM",
           Type="Latent")
names(cl.rt.models.orig) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
load("results/clpm.warnings.RData")
## Find any with problems
problems <- as.numeric(grep("instabilities|positive\ definite|variances\ are\ negative", clpm.warnings))
cl.rt.models.orig[!(cl.rt.models.orig$stateId %in% problems), "select"] <- 1

cl.rt.models.ri <- dataRi %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="RI-CLPM",
           Type="Latent")
names(cl.rt.models.ri) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
load("results/riclpm.warnings.RData")
## Find any with problems
problems <- as.numeric(grep("instabilities|positive\ definite|variances\ are\ negative", riclpm.warnings))
cl.rt.models.ri[!(cl.rt.models.ri$stateId %in% problems), "select"] <- 1


cl.rt.models.obs <- dataObs %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="CLPM",
           Type="Observed")
names(cl.rt.models.obs) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
load("results/clpm.observed.warnings.RData")
## Find any with problems
problems <- as.numeric(grep("instabilities|positive\ definite|variances\ are\ negative", clpm.warnings))
cl.rt.models.obs[!(cl.rt.models.obs$stateId %in% problems), "select"] <- 1


cl.rt.models.ri.obs <- dataRiObs %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="RI-CLPM",
           Type="Observed")
names(cl.rt.models.ri.obs) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
load("results/riclpm.observed.warnings.RData")
## Find any with problems
problems <- as.numeric(grep("instabilities|positive\ definite|variances\ are\ negative", riclpm.warnings))
cl.rt.models.ri.obs[!(cl.rt.models.ri.obs$stateId %in% problems), "select"] <- 1



#### Working here
## Load warnings
load("results/clpm.single.warnings.RData")
warningsList <- lapply(
    stateWarnings,
    function(x) grep("instabilities|positive\ definite|variances\ are\ negative", x)
)

nTable <- dataSingle %>%
    select(state, contains("samplesize")) %>%
    pivot_longer(
        cols = contains("samplesize"),
        names_to = c("Trait", "var"),
        values_to = "samplesize",
        names_pattern = "([[:alpha:]]*)\\.(.*)"
    )

for (i in c("agr","cns","ext","neu","opn")) {
    nTable[nTable$Trait==i &
           !(nTable$state %in% warningsList[[i]]),
           "select"] <- 1
}

dataSingle.l <- dataSingle %>%
    left_join(bulaList[, c("state", "stateName", "z.relig")],
        by = "state"
        ) %>%
    mutate(
        Model = "CLPM",
        Type = "Latent"
    ) %>%
    select(!contains("samplesize")) %>%
        pivot_longer(
            cols = agr.r1:opn.st.ub,
            names_to = c("Trait", "parameter"),
            names_pattern = "([[:alpha:]]*)\\.(.*)"
        ) %>%
    left_join(nTable[, -3],
        by = c("state", "Trait")
    )
    


    


cl.rt.models.single <- dataSingle %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="CLPM",
           Type="Single")
names(cl.rt.models.single) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Find any with problems
problems <- grep("instabilities|positive\ definite|variances\ are\ negative", stateWarnings[["agr"]])
cl.rt.models.single[!(cl.rt.models.single$stateId %in% problems), "select"] <- 1




cl.rt.models.ri.single <- dataRiSingle %>%
    select(starts_with("agr.rt.cl"), state, stateName, z.relig) %>%
    mutate(Model="RI-CLPM",
           Type="Single")
names(cl.rt.models.ri.single) <- c("rt.cl", "ub", "lb", "stateId", "State", "Religiosity", "Model", "Type")
## Load warnings
load("results/riclpm.single.warnings.RData")
## Find any with problems
problems <- grep("instabilities|positive\ definite|variances\ are\ negative", stateWarnings[["agr"]])
cl.rt.models.ri.single[!(cl.rt.models.ri.single$stateId %in% problems), "select"] <- 1





allModels <- rbind(cl.rt.models.orig,
                   cl.rt.models.ri,
                   cl.rt.models.obs,
                   cl.rt.models.ri.obs,
                   cl.rt.models.single,
                   cl.rt.models.ri.single)
allModels$Model <- as.factor(allModels$Model)
allModels$Type <- as.factor(allModels$Type)

testPlot <- ggplot(data=subset(allModels[allModels$select==1,], !is.na(Model)),
                   aes(x=State,
                       y=rt.cl,
                       ymin=lb,
                       ymax=ub,
                       color=Model,
                       linetype=Type)) +
    geom_point(position=position_dodge(width=0.5)) +
    geom_errorbar(width=.05, position=position_dodge(width=0.5)) + coord_flip()
testPlot
    
                       

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
data <- left_join(data, bulaList[, c("state", "stateName", "z.relig")], by = "state")

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
dataRi <- left_join(dataRi, bulaList[, c("state", "stateName", "z.relig")], by = "state")

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
dataObs <- left_join(dataObs, bulaList[, c("state", "stateName", "z.relig")], by = "state")

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
dataRiObs <- left_join(dataRiObs, bulaList[, c("state", "stateName", "z.relig")], by = "state")

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
## CLPM: Single-Trait Models
################################################################################

dataSingle <- read_csv("results/clpm.states.single.estimates.csv")
dataSingle <- left_join(dataSingle, bulaList[, c("state", "stateName", "z.relig")], by = "state")

################################################################################
## RICLPM: Single-Trait Models
################################################################################

dataRiSingle <- read_csv("results/riclpm.states.single.estimates.csv")
dataRiSingle <- left_join(dataRiSingle, bulaList[, c("state", "stateName", "z.relig")], by = "state")

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
