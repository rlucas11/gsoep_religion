library(metafor)
library(tidyverse)
library(ggplot2)


## Read Data
results <- read_csv("data/combinedResults.rev.csv")
bulaLevels <- read_csv("info/stateReligiosity.csv")
results$stateName <- factor(results$stateName, levels = bulaLevels$levels)
results$Model <- as.factor(results$Model)
results$Type <- as.factor(results$Type)
results$Traits <- as.factor(results$Traits)

## Variables to loop over for meta-analysis
varLoop <- expand_grid(trait = c("agr", "cns", "ext", "neu", "opn"),
                       effect = c("rt.cl", "tr.cl"),
                       model = c("CLPM", "RICLPM"),
                       type = c("Latent", "Observed"),
                       variables = c("All", "Single"))



################################################################################
## Functions
################################################################################

## Function to select data for metanalysis
metaSelect <- function(results,
                       parameter,
                       trait,
                       model = NULL,
                       type = NULL,
                       traits = NULL) {
    data <- results %>%
        filter(grepl(parameter, Parameter) &
               grepl(trait, Trait) &
               select==1)
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
    names(data) <- c("state",
                     "State",
                     "Religiosity",
                     "Model",
                     "Type",
                     "Traits",
                     "Trait",
                     "select",
                     "samplesize",
                     "es",
                     "lb",
                     "ub")
    return(data)
}


## Run meta-analysis
stateMeta <- function(data,
                      trait,
                      effect,
                      model,
                      type,
                      traits,
                      moderator=FALSE) {
    metaData <- metaSelect(data,
                           effect,
                           trait,
                           model,
                           type,
                           traits)
    yivi <- escalc(measure = "COR",
                   ri = es,
                   ni = samplesize,
                   data = metaData)
    ifelse(moderator == FALSE,
        meta <- rma(yi, vi, data = yivi, method = "ML"),
        meta <- rma(yi, vi, data = yivi, method = "ML", mods = Religiosity)
    )
    summary(meta)
    return(meta)
}


################################################################################
## Results
################################################################################

## Intercept
metaCombinedResults <- data.frame(
    trait = character(),
    effect = character(),
    model = character(),
    type = character(),
    variables = character(),
    est = numeric(),
    se = numeric(),
    lb = numeric(),
    ub = numeric()
)

for (i in 1:nrow(varLoop)) {
    tempOut <- stateMeta(
        results,
        varLoop[[i, "trait"]],
        varLoop[[i, "effect"]],
        varLoop[[i, "model"]],
        varLoop[[i, "type"]],
        varLoop[[i, "variables"]]
    )
    metaCombinedResults[i, "trait"] <- varLoop[i, "trait"]
    metaCombinedResults[i, "effect"] <- varLoop[i, "effect"]
    metaCombinedResults[i, "model"] <- varLoop[i, "model"]
    metaCombinedResults[i, "type"] <- varLoop[i, "type"]
    metaCombinedResults[i, "variables"] <- varLoop[i, "variables"]
    metaCombinedResults[i, "est"] <- as.numeric(tempOut$b)
    metaCombinedResults[i, "se"] <- as.numeric(tempOut$se)
    metaCombinedResults[i, "lb"] <- as.numeric(tempOut$ci.lb)
    metaCombinedResults[i, "ub"] <- as.numeric(tempOut$ci.ub)
}

write_csv(metaCombinedResults, "results/metaCombinedResults.rev.csv")

metaCombinedResults %>%
    filter(
        effect == "rt.cl"
    ) %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = lb,
            ymax = ub,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
        geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()

ggsave("figures/rt.cl.png")

metaCombinedResults %>%
    filter(
        effect == "tr.cl"
    ) %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = lb,
            ymax = ub,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()

ggsave("figures/tr.cl.png")

## Moderator
metaModResults <- data.frame(
    trait = character(),
    effect = character(),
    model = character(),
    type = character(),
    variables = character(),
    est = numeric(),
    se = numeric(),
    lb = numeric(),
    ub = numeric(),
    mod.est = numeric(),
    mod.se = numeric(),
    mod.lb = numeric(),
    mod.ub = numeric()
)

for (i in 1:nrow(varLoop)) {
    tempOut <- stateMeta(
        results,
        varLoop[[i, "trait"]],
        varLoop[[i, "effect"]],
        varLoop[[i, "model"]],
        varLoop[[i, "type"]],
        varLoop[[i, "variables"]],
        moderator=TRUE
    )
    metaModResults[i, "trait"] <- varLoop[i, "trait"]
    metaModResults[i, "effect"] <- varLoop[i, "effect"]
    metaModResults[i, "model"] <- varLoop[i, "model"]
    metaModResults[i, "type"] <- varLoop[i, "type"]
    metaModResults[i, "variables"] <- varLoop[i, "variables"]
    metaModResults[i, "est"] <- as.numeric(tempOut$b[[1]])
    metaModResults[i, "se"] <- as.numeric(tempOut$se[[1]])
    metaModResults[i, "lb"] <- as.numeric(tempOut$ci.lb[[1]])
    metaModResults[i, "ub"] <- as.numeric(tempOut$ci.ub[[1]])
    metaModResults[i, "mod.est"] <- as.numeric(tempOut$b[[2]])
    metaModResults[i, "mod.se"] <- as.numeric(tempOut$se[[2]])
    metaModResults[i, "mod.lb"] <- as.numeric(tempOut$ci.lb[[2]])
    metaModResults[i, "mod.ub"] <- as.numeric(tempOut$ci.ub[[2]])
}

write_csv(metaModResults, "results/metaModResults.rev.csv")

metaModResults %>%
    filter(
        effect == "rt.cl"
    ) %>%
    ggplot(
        aes(
            x = trait,
            y = mod.est,
            ymin = mod.lb,
            ymax = mod.ub,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()

ggsave("figures/rt.cl.mod.png")

metaModResults %>%
    filter(
        effect == "tr.cl"
    ) %>%
    ggplot(
        aes(
            x = trait,
            y = mod.est,
            ymin = mod.lb,
            ymax = mod.ub,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()

ggsave("figures/tr.cl.mod.png")



################################################################################
## Setup Tables of Estimates and Problems for Supplement
################################################################################

## Read Data
results <- read_csv("data/combinedResults.rev.csv")

## Predicting Religion From Trait
metaFullResultsRt <- results %>%
    select(
        stateName,
        Model,
        Type,
        Traits,
        Trait,
        Parameter,
        value
    ) %>%
    filter(substr(Parameter, 1, 2) == "rt") %>%
        pivot_wider(
            id_cols = c(stateName, Model, Type, Traits),
            names_from = c(Trait, Parameter),
            values_from = value
        )
    

metaProblemsRt <- results %>%
    select(stateName,
        Model,
        Type,
        Traits,
        Trait,
        select
        ) %>%
    group_by(Trait, Traits, Model, stateName, Type) %>%
    summarize(problems = sum(is.na(select))) %>%
    mutate(problem = case_when(problems == 0 ~ "No",
                               problems > 0 ~ "Yes")
           ) %>%
    pivot_wider(
        id_cols = c(stateName, Model, Type, Traits),
        names_from = Trait,
        values_from = problem
    )
                

metaComboRt <- left_join(
    metaFullResultsRt,
    metaProblemsRt,
    by = c(
        "stateName",
        "Model",
        "Type",
        "Traits"
    )
)

write_csv(metaComboRt, "results/metaComboRt.rev.csv")


## Predicting Trait From Religion
metaFullResultsTr <- results %>%
    select(
        stateName,
        Model,
        Type,
        Traits,
        Trait,
        Parameter,
        value
    ) %>%
    filter(substr(Parameter, 1, 2) == "tr") %>%
        pivot_wider(
            id_cols = c(stateName, Model, Type, Traits),
            names_from = c(Trait, Parameter),
            values_from = value
        )
    

metaProblemsTr <- results %>%
    select(stateName,
        Model,
        Type,
        Traits,
        Trait,
        select
        ) %>%
    group_by(Trait, Traits, Model, stateName, Type) %>%
    summarize(problems = sum(is.na(select))) %>%
    mutate(problem = case_when(problems == 0 ~ "No",
                               problems > 0 ~ "Yes")
           ) %>%
    pivot_wider(
        id_cols = c(stateName, Model, Type, Traits),
        names_from = Trait,
        values_from = problem
    )
                

metaComboTr <- left_join(
    metaFullResultsTr,
    metaProblemsTr,
    by = c(
        "stateName",
        "Model",
        "Type",
        "Traits"
    )
)

write_csv(metaComboTr, "results/metaComboTr.rev.csv")
