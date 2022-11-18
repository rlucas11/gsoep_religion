## Load libraries
library(lavaan)
library(tidyverse)

## Source model files
source("scripts/originalModel.R")
source("scripts/clpmUni.R")
source("scripts/riclpmUni.R")
source("scripts/gclpm.R")
source("scripts/fullRiclpm.R")
source("scripts/riclpmObserved.R")
source("scripts/clpmObserved.R")

## Read Data
data <- read_csv("data/final.csv")

## Rename religion variales so recoded versions are used in model
names(data)[65:72] <- c(paste0(rep("relig", 4),
                                c("05", "09", "13", "17"),
                                rep("_orig", 4)),
                         paste0(rep("relig", 4),
                                c("05", "09", "13", "17")))


################################################################################
## Reproduce original with full sample
################################################################################
## This takes a long time to run

model1 <- sem(model1_main, missing = "FIML", estimator = "MLR", data = data)
## Save to file because it takes so long to run
sink(file = "info/originalModel.txt", append = FALSE)
summary(model1)
standardizedResults <- standardizedSolution(model1,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
    )
standardizedResults
fitMeasures(model1)
sink()

## Temporarily save model as file
save(model1, file = "info/model1.RData")
load("info/model1.RData")

## List estimate labels for estimate-extraction function
estimateLabels <- data.frame(matrix(c(
    "agr", "c_ra", "c_ar", "sa", "r1_ra", "r2_ra",
    "cns", "c_rc", "c_cr", "sc", "r1_rc", "r2_rc",
    "ext", "c_re", "c_er", "se", "r1_re", "r2_re",
    "neu", "c_rn", "c_nr", "sn", "r1_rn", "r2_rn",
    "opn", "c_ro", "c_or", "so", "r1_ro", "r2_ro"
),
nrow = 5,
ncol = 6,
byrow = TRUE
))
names(estimateLabels) <- c("trait", "cl_rOnt", "cl_tOnr", "stability", "r1", "r2")


## Function to extract standardized estimates for meta-analysis
## Extracts estimate of r, along with 95% CIs
## Function works on one trait at a time (e.g., a line of estimateLabels)

extractEstimates <- function(resultsLabels, results) {
    clpmResults <- data.frame(
        trait = character(),
        r1 = numeric(),
        r1.lb = numeric(),
        r1.ub = numeric(),
        r2 = numeric(),
        r2.lb = numeric(),
        r2.ub = numeric(),
        rt.cl = numeric(),
        rt.cl.lb = numeric(),
        rt.cl.ub = numeric(),
        tr.cl = numeric(),
        tr.cl.lb = numeric(),
        tr.cl.ub = numeric(),
        st = numeric(),
        st.lb = numeric(),
        st.ub = numeric()
    )
    clpmResults[1, "trait"] <- resultsLabels$trait
    clpmResults[1, "r1"] <- results[results$label == resultsLabels[[1, "r1"]], "est.std"]
    clpmResults[1, "r1.lb"] <- results[results$label == resultsLabels[[1, "r1"]], "ci.lower"]
    clpmResults[1, "r1.ub"] <- results[results$label == resultsLabels[[1, "r1"]], "ci.upper"]
    clpmResults[1, "r2"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "est.std"])
    clpmResults[1, "r2.lb"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "ci.lower"])
    clpmResults[1, "r2.ub"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "ci.upper"])
    clpmResults[1, "rt.cl"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "est.std"])
    clpmResults[1, "rt.cl.lb"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "ci.lower"])
    clpmResults[1, "rt.cl.ub"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "ci.upper"])
    clpmResults[1, "tr.cl"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "est.std"])
    clpmResults[1, "tr.cl.lb"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "ci.lower"])
    clpmResults[1, "tr.cl.ub"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "ci.upper"])
    clpmResults[1, "st"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "est.std"])
    clpmResults[1, "st.lb"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "ci.lower"])
    clpmResults[1, "st.ub"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "ci.upper"])
    return(clpmResults)
}

## Extract all the estimates in a loop
rm(combinedClpmResults)
combinedClpmResults <- data.frame(
        trait = character(),
        r1 = numeric(),
        r1.lb = numeric(),
        r1.ub = numeric(),
        r2 = numeric(),
        r2.lb = numeric(),
        r2.ub = numeric(),
        rt.cl = numeric(),
        rt.cl.lb = numeric(),
        rt.cl.ub = numeric(),
        tr.cl = numeric(),
        tr.cl.lb = numeric(),
        tr.cl.ub = numeric(),
        st = numeric(),
        st.lb = numeric(),
        st.ub = numeric()
)
for (i in 1:nrow(estimateLabels)) {
    tempResults <- extractEstimates(estimateLabels[i, ], standardizedResults)
    combinedClpmResults[i, ] <- tempResults
}
combinedClpmResults

    


## Ri-CLPM

modelRiclpm <- sem(model1_riclpm, missing = "FIML", estimator = "MLR", data = data)
sink(file = "info/fullRiclpmModel.txt", append = FALSE)
summary(modelRiclpm)
riclpmStandardizedResults <- standardizedSolution(modelRiclpm,
    type = "std.all", se = TRUE, zstat = TRUE,
    pvalue = TRUE, ci = TRUE, level = .95, output = "text"
)
riclpmStandardizedResults
fitMeasures(modelRiclpm)
sink()

clpmStandardizedResults

## Temporarily save model.extract
save(modelRiclpm, file = "info/riclpmModel.RData")
load("info/riclpmModel.RData")

## List estimate labels for estimate-extraction function
riclpmEstimateLabels <- data.frame(matrix(c(
    "agr", "c_ra", "c_ar", "sa", "r1_ra", "r2_ra", "ri_ar",
    "cns", "c_rc", "c_cr", "sc", "r1_rc", "r2_rc", "ri_cr",
    "ext", "c_re", "c_er", "se", "r1_re", "r2_re", "ri_er",
    "neu", "c_rn", "c_nr", "sn", "r1_rn", "r2_rn", "ri_nr",
    "opn", "c_ro", "c_or", "so", "r1_ro", "r2_ro", "ri_or"
),
nrow = 5,
ncol = 7,
byrow = TRUE
))

names(riclpmEstimateLabels) <- c("trait", "cl_rOnt", "cl_tOnr", "stability", "r1", "r2", "ri_r")


## Function to extract standardized estimates for meta-analysis
## Extracts estimate of r, along with 95% CIs
## Function works on one trait at a time (e.g., a line of estimateLabels)

riclpmExtractEstimates <- function(resultsLabels, results) {
    riclpmResults <- data.frame(
        trait = character(),
        r1 = numeric(),
        r1.lb = numeric(),
        r1.ub = numeric(),
        r2 = numeric(),
        r2.lb = numeric(),
        r2.ub = numeric(),
        rt.cl = numeric(),
        rt.cl.lb = numeric(),
        rt.cl.ub = numeric(),
        tr.cl = numeric(),
        tr.cl.lb = numeric(),
        tr.cl.ub = numeric(),
        st = numeric(),
        st.lb = numeric(),
        st.ub = numeric(),
        ri_r = numeric(),
        ri_r.lb = numeric(),
        ri_r.ub = numeric()
    )
    riclpmResults[1, "trait"] <- resultsLabels$trait
    riclpmResults[1, "r1"] <- results[results$label == resultsLabels[[1, "r1"]], "est.std"]
    riclpmResults[1, "r1.lb"] <- results[results$label == resultsLabels[[1, "r1"]], "ci.lower"]
    riclpmResults[1, "r1.ub"] <- results[results$label == resultsLabels[[1, "r1"]], "ci.upper"]
    riclpmResults[1, "r2"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "est.std"])
    riclpmResults[1, "r2.lb"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "ci.lower"])
    riclpmResults[1, "r2.ub"] <- mean(results[results$label == resultsLabels[[1, "r2"]], "ci.upper"])
    riclpmResults[1, "rt.cl"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "est.std"])
    riclpmResults[1, "rt.cl.lb"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "ci.lower"])
    riclpmResults[1, "rt.cl.ub"] <- mean(results[results$label == resultsLabels[[1, "cl_rOnt"]], "ci.upper"])
    riclpmResults[1, "tr.cl"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "est.std"])
    riclpmResults[1, "tr.cl.lb"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "ci.lower"])
    riclpmResults[1, "tr.cl.ub"] <- mean(results[results$label == resultsLabels[[1, "cl_tOnr"]], "ci.upper"])
    riclpmResults[1, "st"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "est.std"])
    riclpmResults[1, "st.lb"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "ci.lower"])
    riclpmResults[1, "st.ub"] <- mean(results[results$label == resultsLabels[[1, "stability"]], "ci.upper"])
    riclpmResults[1, "ri_r"] <- mean(results[results$label == resultsLabels[[1, "ri_r"]], "est.std"])
    riclpmResults[1, "ri_r.lb"] <- mean(results[results$label == resultsLabels[[1, "ri_r"]], "ci.lower"])
    riclpmResults[1, "ri_r.ub"] <- mean(results[results$label == resultsLabels[[1, "ri_r"]], "ci.upper"])
    return(riclpmResults)
}

## Extract all the estimates in a loop
rm(combinedRiclpmResults)
combinedRiclpmResults <- data.frame(
    trait = character(),
    r1 = numeric(),
    r1.lb = numeric(),
    r1.ub = numeric(),
    r2 = numeric(),
    r2.lb = numeric(),
    r2.ub = numeric(),
    rt.cl = numeric(),
    rt.cl.lb = numeric(),
    rt.cl.ub = numeric(),
    tr.cl = numeric(),
    tr.cl.lb = numeric(),
    tr.cl.ub = numeric(),
    st = numeric(),
    st.lb = numeric(),
    st.ub = numeric(),
    ri_r = numeric(),
    ri_r.lb = numeric(),
    ri_r.ub = numeric()
)
for (i in 1:nrow(riclpmEstimateLabels)) {
    tempResults <- riclpmExtractEstimates(riclpmEstimateLabels[i, ], riclpmStandardizedResults)
    combinedRiclpmResults[i, ] <- tempResults
}
combinedRiclpmResults


################################################################################
## All Traits, Observed Variables, full sample
################################################################################

model.all.observed <- sem(riclpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
)
summary(model.all.observed)
fitMeasures(model.all.observed)
standardizedSolution(model.all.observed,
                     type = "std.all", se = TRUE, zstat = TRUE,
                     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
                     )


model.all.observed.clpm <- sem(clpm_observed,
    missing = "FIML",
    estimator = "MLR",
    data = data
    )
summary(model.all.observed.clpm)

fitMeasures(model.all.observed.clpm)
standardizedSolution(model.all.observed.clpm,
                     type = "std.all", se = TRUE, zstat = TRUE,
                     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
                     )
cov2cor(fitted(model.all.observed.clpm)$cov)



################################################################################
## Single Trait Models
################################################################################

## Each block pulls trait-specific variables and renames to work with the
## generic syntax in the model code.

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"))
names(agr) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

agrClpm <- sem(clpmUni, data = agr, missing = "FIML")
summary(agrClpm)
standardizedSolution(agrClpm,
                     type = "std.all",
                     se = TRUE,
                     zstat = TRUE,
                     pvalue = TRUE,
                     ci = TRUE,
                     level = .95)

agrRiclpm <- sem(riclpmUni, data = agr, missing = "FIML", estimator = "MLR")
summary(agrRiclpm)
standardizedSolution(agrRiclpm,type = "std.all",
                     se = TRUE,
                     zstat = TRUE,
                     pvalue = TRUE,
                     ci = TRUE,
                     level = .95)

agrGclpm <- sem(gclpmUni, data = agr, missing = "FIML", estimator = "MLR")
summary(agrGclpm)
standardizedSolution(agrGclpm,
                     type = "std.all",
                     se = TRUE,
                     zstat = TRUE,
                     pvalue = TRUE,
                     ci = TRUE,
                     level = .95)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"))
names(cns) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

cnsClpm <- sem(clpmUni, data = cns, missing = "FIML")
summary(cnsClpm)
standardizedSolution(cnsClpm)

cnsRiclpm <- sem(riclpmUni, data = cns, missing = "FIML")
summary(cnsRiclpm)


## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"))
names(ext) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

extClpm <- sem(clpmUni, data = ext, missing = "FIML")
summary(extClpm)
standardizedSolution(extClpm)

extRiclpm <- sem(riclpmUni, data = ext, missing = "FIML")
summary(extRiclpm)


## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"))
names(neu) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

neuClpm <- sem(clpmUni, data = neu, missing = "FIML")
summary(neuClpm)
standardizedSolution(neuClpm)

neuRiclpm <- sem(riclpmUni, data = neu, missing = "FIML")
summary(neuRiclpm)


## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"))
names(opn) <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17"
)

opnClpm <- sem(clpmUni, data = opn, missing = "FIML")
summary(opnClpm)
standardizedSolution(opnClpm)

opnRiclpm <- sem(riclpmUni, data = opn, missing = "FIML")
summary(opnRiclpm)



















