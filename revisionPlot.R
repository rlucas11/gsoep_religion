library(ggplot2)
library(tidyverse)


## Setup labels
traitLabels <- matrix(
    c(
        "agr", "a",
        "cns", "c",
        "ext", "e",
        "neu", "n",
        "opn", "o"
    ),
    nrow = 5, ncol = 2, byrow = TRUE
)


#### Functions to extract and summarize
#### For models with all traits
## Extract averages across multiple waves
extractAvg <- function(results, trait) {
    pLabel <- paste0(
        "c_r",
        trait
    )
    pLabel2 <- paste0(
        "c_",
        trait,
        "r"
    )
    ## Religion predicted from trait
    agg <- results %>%
        filter(label == pLabel) %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    ## Trait predicted from religion
    agg2 <- results %>%
        filter(label == pLabel2) %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    return(c(agg, agg2))
}

## Extract results from full set
extractParameterEstimates <- function(results,
                                      model,
                                      type,
                                      variables) {
    result <- data.frame(
        est = numeric(),
        ci.lower = numeric(),
        ci.upper = numeric(),
        est.1 = numeric(),
        ci.lower.1 = numeric(),
        ci.upper.1 = numeric(),
        trait = character(),
        model = character(),
        type = character(),
        variables = character()
    )
    for (i in 1:nrow(traitLabels)) {
        trait <- traitLabels[i, 2]
        result[i, ] <- c(
            extractAvg(results, trait),
            traitLabels[i,1],
            model,
            type,
            variables
        )
    }
    return(result)
}

#### Functions to extract results
#### These are for single-trait models
## Extract average values across waves
extractAvgSingle <- function(results) {
    ## Religion predicted from trait
    agg <- results %>%
        filter(label == "cl_t") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    ## Trait predicted from religion
    agg2 <- results %>%
        filter(label == "cl_r") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    return(c(agg, agg2))
}

extractAvgSingleDpm <- function(results) {
    ## Religion predicted from trait
    agg <- results %>%
        filter(label == "c") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    ## Trait predicted from religion
    agg2 <- results %>%
        filter(label == "d") %>%
        select(est.std, ci.lower, ci.upper) %>%
        summarise(
            est = mean(est.std),
            ci.lower = mean(ci.lower),
            ci.upper = mean(ci.upper)
        )
    return(c(agg, agg2))
}


## Extract estimates across all models
extractParameterEstimatesSingle <- function(results,
                                            model,
                                            type,
                                            variables) {
    result <- data.frame(
        est = numeric(),
        ci.lower = numeric(),
        ci.upper = numeric(),
        est.1 = numeric(),
        ci.lower.1 = numeric(),
        ci.upper.1 = numeric(),
        trait = character(),
        model = character(),
        type = character(),
        variables = character()
    )
    for (i in 1:nrow(traitLabels)) {
        trait <- traitLabels[i, 2]
        result[i, ] <- c(
            extractAvgSingle(results[[i]][[1]]),
            traitLabels[i,1],
            model,
            type,
            variables
        )
    }
    return(result)
}

## Extract estimates across all models
## For DPM
extractParameterEstimatesSingleDpm <- function(results,
                                            model,
                                            type,
                                            variables) {
    result <- data.frame(
        est = numeric(),
        ci.lower = numeric(),
        ci.upper = numeric(),
        est.1 = numeric(),
        ci.lower.1 = numeric(),
        ci.upper.1 = numeric(),
        trait = character(),
        model = character(),
        type = character(),
        variables = character()
    )
    for (i in 1:nrow(traitLabels)) {
        trait <- traitLabels[i, 2]
        result[i, ] <- c(
            extractAvgSingleDpm(results[[i]][[1]]),
            traitLabels[i,1],
            model,
            type,
            variables
        )
    }
    return(result)
}


location <- "results"

## Load results for each set of models
load(paste0(location, "/clpm.latent.results.RData"))
results.c.l.a <- clpm.latent.results[2][[1]]
c.l.a <- extractParameterEstimates(
    results.c.l.a,
    "clpm",
    "latent",
    "all"
)

load(paste0(location, "/single.dpm.obs.results.RData"))
d.o.s <- extractParameterEstimatesSingleDpm(
    single.dpm.obs.results,
    "dpm",
    "observed",
    "single"
)


location <- "testResults"
load(paste0(location, "/single.dpm.latent.results.RData"))
d.l.s <- extractParameterEstimatesSingle(
    single.dpm.latent.results,
    "dpm",
    "latent",
    "single"
)






## Collect data from all models
plotData <- rbind(
    c.l.a,
    d.o.s,
    d.l.s
)

## Brute force restructure
plotData1 <- plotData[,c("est", "ci.lower", "ci.upper", "trait", "model", "type", "variables")]
plotData1$direction <- "tr"
plotData2 <- plotData[,c("est.1", "ci.lower.1", "ci.upper.1", "trait", "model", "type", "variables")]
names(plotData2) <- c("est", "ci.lower", "ci.upper", "trait", "model", "type", "variables")
plotData2$direction <- "rt"

plotDataFinal <- rbind(plotData1, plotData2)


plotDataFinal %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = ci.lower,
            ymax = ci.upper,
            color = model,
            linetype = type
        )
    ) +
    geom_point(position = position_dodge(width = 0.75), size=2) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.75)) +
    coord_flip() +
    theme_bw() +
    scale_color_grey() +
    scale_color_manual(values=c("grey", "black")) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(limits = c(-.08, .08)) +
    facet_wrap(vars(direction))

ggsave("test.png")

plotData %>%
    ggplot(
        aes(
            x = trait,
            y = est.1,
            ymin = ci.lower.1,
            ymax = ci.upper.1,
            color = model,
            linetype = type
        )
    ) +
    geom_point(position = position_dodge(width = 0.75), size=2) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.75)) +
    coord_flip() +
    theme_bw() +
    scale_color_grey() +
    scale_color_manual(values=c("grey", "black")) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(limits = c(-.08, .08))

ggsave("test2.png")
