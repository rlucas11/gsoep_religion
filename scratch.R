library(tidyverse)

load("results/clpm.errors.RData")
load("results/clpm.warnings.RData")
clpm.errors
clpm.warnings

load("results/riclpm.errors.RData")
load("results/riclpm.warnings.RData")
riclpm.errors
riclpm.warnings

load("results/clpm.observed.errors.RData")
load("results/clpm.observed.warnings.RData")
clpm.errors
clpm.warnings

load("results/riclpm.observed.errors.RData")
load("results/riclpm.observed.warnings.RData")
riclpm.errors
riclpm.warnings


load("results/riclpm.single.errors.RData")
load("results/riclpm.single.warnings.RData")

load("results/clpm.single.errors.RData")
load("results/clpm.single.warnings.RData")


if (length(stateWarnings[[5]][[4]])>0) {
    if (grepl("solution\ has\ NOT", stateWarnings[[5]][[4]])) {
        print("nope")
    }
}

fit <- NULL
testVar <- stateWarnings[[5]][[5]]
if (length(testVar) == 0) {
    fit <- "yes"
} else
    { if (!(TRUE %in% grepl("solution\ has\ NOT", testVar))) {
        fit <- "yes"
    }
}
fit

grepl("solution\ has\ NOT", stateWarnings[[5]][[3]])



TRUE %in% grepl("solution\ has\ NOT", testVar)

load("results/correlationsByState.RData")


cor(data[, paste0(
    rep(
        c(
            "agr",
            "cns",
            "ext",
            "neu",
            "opn",
            "relig"
        ),
        each = 4
    ),
    c("05", "09", "13", "17")
)], use = "pair")

         
load("info/clpm.latent.all.RData")


################################################################################
## Correlations by state
################################################################################
load("results/correlationsByState.RData")
states <- read_csv("info/states.csv")

## Initialize data frame
corTab <- data.frame(
    State = numeric(),
    Agreeableness = numeric(),
    Conscientiousness = numeric(),
    Extraversion = numeric(),
    Neuroticism = numeric(),
    Openness = numeric(),
    Religiosity = numeric(),
    N = numeric()
)

## Extract results
for (i in 1:16) {
    corTab[i, ] <- c(
        states[i, 2],
        out$r[i][[1]][6, 1:5],
        out$mean[i, 6],
        out$n[i, 7]
    )
}

## Add overall rs
corTab <- rbind(corTab, c(
    "Pooled Within",
    out$rwg[6, 1:5],
    NA,
    sum(corTab[1:16, 8])
))

corTab <- rbind(corTab, c(
    "Raw Correlation",
    out$raw[6, 1:5],
    NA,
    corTab[17, 8]
))

papaja::apa_table(corTab,
                  midrules=c(16),
                  align=rep("r", 8),
                  format.args=list(na_string=""),
                  col_spanners=list(`Correlation with Religiosity`=c(2, 6)),
                  caption="Within-state correlations between each personality trait and religiosity. Sample size and mean religiosity are presented in the rightmost columns.")



################################################################################
## meta-analysis
################################################################################

temp <- metaSelect(
    results,
    "rt.cl",
    "agr",
    "CLPM",
    "Latent",
    "All"
)

load("results/clpm.errors.RData")
load("results/clpm.warnings.RData")


################################################################################
## Collect results for full sample models
################################################################################

load("testResults/clpm.latent.results.RData")
results <- clpm.latent.results[2][[1]]

library(tidyverse)
library(ggplot2)

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

extractAvg(results, "a")


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
        
load("testResults/clpm.latent.results.RData")
results.c.l.a <- clpm.latent.results[2][[1]]
c.l.a <- extractParameterEstimates(results.c.l.a, "clpm", "latent", "all")


load("testResults/riclpm.latent.results.RData")
results.r.l.a <- riclpm.latent.results[2][[1]]
r.l.a <- extractParameterEstimates(results.r.l.a, "riclpm", "latent", "all")


load("testResults/clpm.observed.results.RData")
results.c.o.a <- clpm.observed.results[2][[1]]
c.o.a <- extractParameterEstimates(results.c.o.a, "clpm", "observed", "all")


load("testResults/riclpm.observed.results.RData")
results.r.o.a <- riclpm.observed.results[2][[1]]
r.o.a <- extractParameterEstimates(results.r.o.a, "riclpm", "observed", "all")


plotData <- rbind(
    c.l.a,
    r.l.a,
    c.o.a,
    r.o.a
)


plotData %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = ci.lower,
            ymax = ci.upper,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()





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

extractParameterEstimatesSingle(
    single.clpm.latent.results,
    "clpm",
    "latent",
    "single"
)


load("testResults/single.clpm.latent.results.RData")
c.l.s <- extractParameterEstimatesSingle(
    single.clpm.latent.results,
    "clpm",
    "latent",
    "single"
)
c.l.s


load("testResults/single.riclpm.latent.results.RData")
r.l.s <- extractParameterEstimatesSingle(
    single.riclpm.latent.results,
    "riclpm",
    "latent",
    "single"
)
r.l.s

load("testResults/single.clpm.obs.results.RData")
c.o.s <- extractParameterEstimatesSingle(
    single.clpm.obs.results,
    "clpm",
    "observed",
    "single"
)
c.o.s


load("testResults/single.riclpm.obs.results.RData")
r.o.s <- extractParameterEstimatesSingle(
    single.riclpm.obs.results,
    "riclpm",
    "observed",
    "single"
)
r.o.s



plotData <- rbind(
    c.l.a,
    r.l.a,
    c.o.a,
    r.o.a,
    c.l.s,
    r.l.s,
    c.o.s,
    r.o.s
)


plotData %>%
    ggplot(
        aes(
            x = trait,
            y = est,
            ymin = ci.lower,
            ymax = ci.upper,
            color = model,
            linetype = type,
            shape = variables
        )
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(width = .05, position = position_dodge(width = 0.5)) +
    coord_flip()

################################################################################
## Full Sample Fit Indices
################################################################################

location <- "testResults"

fit <- data.frame(
    chisq = numeric(),
    df = numeric(),
    pvalue = numeric(),
    cfi = numeric(),
    rmsea = numeric(),
    srmr = numeric(),
    trait = character(),
    model = character(),
    type = character(),
    variables = character()
)

                  

## Load results for each set of models
load(paste0(location, "/clpm.latent.results.RData"))
fit[1, ] <- c(
    clpm.latent.results[[1]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
    )],
    NA,
    "clpm",
    "latent",
    "all"
)

load(paste0(location, "/riclpm.latent.results.RData"))
fit[2, ] <- c(
    riclpm.latent.results[[1]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
    )],
    NA,
    "riclpm",
    "latent",
    "all"
)


load(paste0(location, "/clpm.observed.results.RData"))
fit[3, ] <- c(
    clpm.observed.results[[1]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
    )],
    NA,
    "clpm",
    "observed",
    "all"
)

load(paste0(location, "/riclpm.observed.results.RData"))
fit[4, ] <- c(
    riclpm.observed.results[[1]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
    )],
    NA,
    "riclpm",
    "observed",
    "all"
)


traits <- c("agr", "cns", "ext", "neu", "opn")

load(paste0(location, "/single.clpm.latent.results.RData"))
c.l.s <- data.frame(
    chisq = numeric(),
    df = numeric(),
    pvalue = numeric(),
    cfi = numeric(),
    rmsea = numeric(),
    srmr = numeric(),
    trait = character(),
    model = character(),
    type = character(),
    variables = character()
)

for (i in 1:5) {
    c.l.s[i, 1:6] <- single.clpm.latent.results[[i]][[2]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
        )]
    c.l.s[i, 7] <- traits[i]
    c.l.s[i, 8] <- "clpm"
    c.l.s[i, 9] <- "latent"
    c.l.s[i, 10] <- "single"
}
    

load(paste0(location, "/single.riclpm.latent.results.RData"))
r.l.s <- data.frame(
    chisq = numeric(),
    df = numeric(),
    pvalue = numeric(),
    cfi = numeric(),
    rmsea = numeric(),
    srmr = numeric(),
    trait = character(),
    model = character(),
    type = character(),
    variables = character()
)

for (i in 1:5) {
    r.l.s[i, 1:6] <- single.riclpm.latent.results[[i]][[2]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
        )]
    r.l.s[i, 7] <- traits[i]
    r.l.s[i, 8] <- "riclpm"
    r.l.s[i, 9] <- "latent"
    r.l.s[i, 10] <- "single"
}


load(paste0(location, "/single.clpm.obs.results.RData"))
c.o.s <- data.frame(
    chisq = numeric(),
    df = numeric(),
    pvalue = numeric(),
    cfi = numeric(),
    rmsea = numeric(),
    srmr = numeric(),
    trait = character(),
    model = character(),
    type = character(),
    variables = character()
)

for (i in 1:5) {
    c.o.s[i, 1:6] <- single.clpm.obs.results[[i]][[2]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
        )]
    c.o.s[i, 7] <- traits[i]
    c.o.s[i, 8] <- "clpm"
    c.o.s[i, 9] <- "observed"
    c.o.s[i, 10] <- "single"
}


load(paste0(location, "/single.riclpm.obs.results.RData"))
r.o.s <- data.frame(
    chisq = numeric(),
    df = numeric(),
    pvalue = numeric(),
    cfi = numeric(),
    rmsea = numeric(),
    srmr = numeric(),
    trait = character(),
    model = character(),
    type = character(),
    variables = character()
)

for (i in 1:5) {
    r.o.s[i, 1:6] <- single.riclpm.obs.results[[i]][[2]][c(
        "chisq",
        "df",
        "pvalue",
        "cfi",
        "rmsea",
        "srmr"
        )]
    r.o.s[i, 7] <- traits[i]
    r.o.s[i, 8] <- "riclpm"
    r.o.s[i, 9] <- "observed"
    r.o.s[i, 10] <- "single"
}




r.o.s <- extractParameterEstimatesSingle(
    single.riclpm.obs.results,
    "riclpm",
    "observed",
    "single"
)


finalFit <- rbind(fit, c.l.s, r.l.s, c.o.s, r.o.s)


################################################################################
## Meta-analysis full results
################################################################################

library(metafor)
library(tidyverse)
library(ggplot2)



           


print(results %>%
    filter(
        Model == "RICLPM",
        Type == "Latent",
        Traits == "All",
        stateName == "Bavaria"
    ), n = 90)

#### Table for supplement

library(tidyverse)
problemTable <- read_csv("results/metaCombo.csv")


problemTable$Agreeableness <- paste0(
    formatC(problemTable$agr_tr.cl, format = "f", digits = 2),
    " (",
    formatC(problemTable$agr_tr.cl.lb, format = "f", digits = 2),
    ", ",
    formatC(problemTable$agr_tr.cl.ub, format = "f", digits = 2),
    ")"
)

problemTable$Conscientiousness <- paste0(
    formatC(problemTable$cns_tr.cl, format = "f", digits = 2),
    " (",
    formatC(problemTable$cns_tr.cl.lb, format = "f", digits = 2),
    ", ",
    formatC(problemTable$cns_tr.cl.ub, format = "f", digits = 2),
    ")"
)

problemTable$Extraversion <- paste0(
    formatC(problemTable$ext_tr.cl, format = "f", digits = 2),
    " (",
    formatC(problemTable$ext_tr.cl.lb, format = "f", digits = 2),
    ", ",
    formatC(problemTable$ext_tr.cl.ub, format = "f", digits = 2),
    ")"
)

problemTable$Neuroticism <- paste0(
    formatC(problemTable$neu_tr.cl, format = "f", digits = 2),
    " (",
    formatC(problemTable$neu_tr.cl.lb, format = "f", digits = 2),
    ", ",
    formatC(problemTable$neu_tr.cl.ub, format = "f", digits = 2),
    ")"
)

problemTable$Openness <- paste0(
    formatC(problemTable$opn_tr.cl, format = "f", digits = 2),
    " (",
    formatC(problemTable$opn_tr.cl.lb, format = "f", digits = 2),
    ", ",
    formatC(problemTable$opn_tr.cl.ub, format = "f", digits = 2),
    ")"
)

problemTable <- problemTable[,c(
    "stateName",
    "Model",
    "Type",
    "Traits",
    "agr",
    "Agreeableness",
    "cns",
    "Conscientiousness",
    "ext",
    "Extraversion",
    "neu",
    "Neuroticism",
    "opn",
    "Openness"
)]

    
################################################################################
## Regression Test for Discussion
################################################################################

source("scripts/latentRegression.R")
source("scripts/aRegression.R")
source("scripts/cRegression.R")

regModel <- sem(big5Regression, data)
summary(regModel, rsquare = TRUE)

cModel <- sem(cRegression, data)
summary(cModel, rsquare = TRUE)


################################################################################
## starts
################################################################################

source("~/Projects/code-generator/buildMplus.R")


################################################################################
## RICLPM, Observed, Single Trait
################################################################################

traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)


## Agreeableness

agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- traitModelNames

agrTrait <- agr %>%
    select(contains("Mean"), contains("relig"))
names(agrTrait) <- c(
    paste0("x", c(1:4)),
    paste0("oldRelig", c(1:4)),
    paste0("y", c(1:4))
)

test <- run_starts_mplus(
    agrTrait,
    4,
    1:4,
    title="agrStarts"
)

test <- run_arts_mplus(
    agrTrait,
    4,
    1:4,
    title="agrArts"
)


## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- traitModelNames


cnsTrait <- cns %>%
    select(contains("Mean"), contains("relig"))
names(cnsTrait) <- c(
    paste0("x", c(1:4)),
    paste0("oldRelig", c(1:4)),
    paste0("y", c(1:4))
)

test <- run_starts_mplus(
    cnsTrait,
    4,
    1:4
)

test <- run_arts_mplus(
    agrTrait,
    4,
    1:4
)



## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- traitModelNames

extTrait <- ext %>%
    select(contains("Mean"), contains("relig"))
names(extTrait) <- c(
    paste0("x", c(1:4)),
    paste0("oldRelig", c(1:4)),
    paste0("y", c(1:4))
)

test <- run_starts_mplus(
    extTrait,
    4,
    1:4,
    title="extStarts"
)

test <- run_arts_mplus(
    extTrait,
    4,
    1:4
)


## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- traitModelNames
neuFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = neu,
    em.h1.iter.max = 20000
)
est.neu.riclpm.obs <- standardizedSolution(neuFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.neu.riclpm.obs <- fitMeasures(neuFit)
neu.riclpm.obs.results <- list(
    est.neu.riclpm.obs,
    fit.neu.riclpm.obs
)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- traitModelNames
opnFit <- sem(riclpmUniObserved,
    missing = "FIML",
    estimator = "MLR",
    data = opn,
    em.h1.iter.max = 20000
)
est.opn.riclpm.obs <- standardizedSolution(opnFit,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
fit.opn.riclpm.obs <- fitMeasures(opnFit)
opn.riclpm.obs.results <- list(
    est.opn.riclpm.obs,
    fit.opn.riclpm.obs
)

single.riclpm.obs.results <- list(
    agr.riclpm.obs.results,
    cns.riclpm.obs.results,
    ext.riclpm.obs.results,
    neu.riclpm.obs.results,
    opn.riclpm.obs.results
)
save(single.riclpm.obs.results,
    file = paste0(location, "/single.riclpm.obs.results.RData")
)


library(MplusAutomation)
temp <- readModels("mplus/startsx.out")

load("results/clpm.latent.results.RData")

oldFit <- read_csv("results/clpm.latent.states.aggregated.fit.csv")
oldFitR <- read_csv("results/riclpm.latent.states.aggregated.fit.csv")

oldEst <- read_csv("results/clpm.latent.states.aggregated.estimates.csv")


source("scripts/originalModelNoTraitIndicators.R") ## Lavaan model name: model1_mod2


################################################################################
## 
################################################################################

library(tidyverse)

clpm <- read_csv("results/clpm.latent.states.aggregated.estimates.rev.csv")
riclpm <- read_csv("results/riclpm.latent.states.aggregated.estimates.rev.csv")

clpmFit <- read_csv("results/clpm.latent.states.aggregated.fit.rev.csv")
riclpmFit <- read_csv("results/riclpm.latent.states.aggregated.estimates.rev.csv")



load("results/clpm.errors.rev.RData")
load("results/riclpm.errors.rev.RData")

load("results/clpm.warnings.rev.RData")
load("results/riclpm.warnings.rev.RData")

