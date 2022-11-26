################################################################################
## Setup
################################################################################

## Read appropriate lavaan model
source("scripts/clpmUni.R")   ## Lavaan model name: clpmUni

## List of estimate labels for estimate-extraction function
## Single-trait version
## st = stability
## r1_r = initial wave correlations
## r2_r = subsequent wave correlations
## c_t = cross-lag, religion regressed on trait
## c_r = cross-lag, trait regressed on religion
clpm.labels <- data.frame(matrix(c(
    "agr", "cl_t", "cl_r", "st", "r1_r", "r2_r",
    "cns", "cl_t", "cl_r", "st", "r1_r", "r2_r",
    "ext", "cl_t", "cl_r", "st", "r1_r", "r2_r",
    "neu", "cl_t", "cl_r", "st", "r1_r", "r2_r",
    "opn", "cl_t", "cl_r", "st", "r1_r", "r2_r"
),
nrow = 5,
ncol = 6,
byrow = TRUE
))
names(clpm.labels) <- c("trait", "cl_rOnt", "cl_tOnr", "stability", "r1", "r2")

## Function to extract standardized estimates for meta-analysis
## Extracts estimate of r, along with 95% CIs
## Function works on one trait at a time (e.g., a line of clpm.labels)
extract.est.clpm <- function(clpm.labels, results) {
    clpm.results <- data.frame(
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
    clpm.results[1, "trait"] <- clpm.labels$trait
    clpm.results[1, "r1"] <- results[results$label == clpm.labels[[1, "r1"]], "est.std"]
    clpm.results[1, "r1.lb"] <- results[results$label == clpm.labels[[1, "r1"]], "ci.lower"]
    clpm.results[1, "r1.ub"] <- results[results$label == clpm.labels[[1, "r1"]], "ci.upper"]
    clpm.results[1, "r2"] <- mean(results[results$label == clpm.labels[[1, "r2"]], "est.std"])
    clpm.results[1, "r2.lb"] <- mean(results[results$label == clpm.labels[[1, "r2"]], "ci.lower"])
    clpm.results[1, "r2.ub"] <- mean(results[results$label == clpm.labels[[1, "r2"]], "ci.upper"])
    clpm.results[1, "rt.cl"] <- mean(results[results$label == clpm.labels[[1, "cl_rOnt"]], "est.std"])
    clpm.results[1, "rt.cl.lb"] <- mean(results[results$label == clpm.labels[[1, "cl_rOnt"]], "ci.lower"])
    clpm.results[1, "rt.cl.ub"] <- mean(results[results$label == clpm.labels[[1, "cl_rOnt"]], "ci.upper"])
    clpm.results[1, "tr.cl"] <- mean(results[results$label == clpm.labels[[1, "cl_tOnr"]], "est.std"])
    clpm.results[1, "tr.cl.lb"] <- mean(results[results$label == clpm.labels[[1, "cl_tOnr"]], "ci.lower"])
    clpm.results[1, "tr.cl.ub"] <- mean(results[results$label == clpm.labels[[1, "cl_tOnr"]], "ci.upper"])
    clpm.results[1, "st"] <- mean(results[results$label == clpm.labels[[1, "stability"]], "est.std"])
    clpm.results[1, "st.lb"] <- mean(results[results$label == clpm.labels[[1, "stability"]], "ci.lower"])
    clpm.results[1, "st.ub"] <- mean(results[results$label == clpm.labels[[1, "stability"]], "ci.upper"])
    return(clpm.results)
}


## Create function for running models in one selected state
## "data" is the full dataset; "bula" is the number of the state to be selected
runModels <- function(bula, data) {
    temp <- subset(data, first.state == bula)
    fit_bula <- sem(clpmUni,
                    missing="FIML",
                    estimator="MLR",
                    data=temp,
                    em.h1.iter.max=20000)
    return(fit_bula)
}


################################################################################
## Run separately in each state
################################################################################

## ## Temporary for testing
## data$first.state <- sample(1:100, nrow(data), replace=TRUE)
## bula_neu <- sort(unique(data$first.state))[1:3]

## Use purrr to run through states, saving output and errors
## Do this separately for each trait
traitModelNames <- c(
    "tr051", "tr091", "tr131", "tr171",
    "tr052", "tr092", "tr132", "tr172",
    "tr053", "tr093", "tr133", "tr173",
    "trMean05", "trMean09", "trMean13", "trMean17", "trMiss",
    "relig05_orig", "relig09_orig", "relig13_orig", "relig17_orig",
    "relig05", "relig09", "relig13", "relig17",
    "first.state"
)

## Agreeableness
agr <- data %>%
    select(contains("agr"), contains("relig"), first.state)
names(agr) <- traitModelNames
agrOutput <- map(bula_neu, quietly(safely(runModels)), agr)

## Conscientiousness
cns <- data %>%
    select(contains("cns"), contains("relig"), first.state)
names(cns) <- traitModelNames
cnsOutput <- map(bula_neu, quietly(safely(runModels)), cns)

## Extraversion
ext <- data %>%
    select(contains("ext"), contains("relig"), first.state)
names(ext) <- traitModelNames
extOutput <- map(bula_neu, quietly(safely(runModels)), ext)

## Neuroticism
neu <- data %>%
    select(contains("neu"), contains("relig"), first.state)
names(neu) <- traitModelNames
neuOutput <- map(bula_neu, quietly(safely(runModels)), neu)

## Openness
opn <- data %>%
    select(contains("opn"), contains("relig"), first.state)
names(opn) <- traitModelNames
opnOutput <- map(bula_neu, quietly(safely(runModels)), opn)

################################################################################
## Extract estimates
################################################################################

stateOutput <- list(agrOutput,
               cnsOutput,
               extOutput,
               neuOutput,
               opnOutput)

stateWarnings <- vector(mode="list",
                        length(stateOutput))
stateErrors <- vector(mode="list",
                      length(stateOutput))
stateFit <- vector(mode="list",
                   length(stateOutput))

                   
                       
if(exists("singleTraitOutput")) rm(singleTraitOutput)

for (k in 1:length(stateOutput)) {
    traitOutput <- stateOutput[[k]]
    clpm.warnings <- vector(mode="list", length=length(traitOutput))
    clpm.errors <- vector(mode="list",length=length(traitOutput))
    clpm.fit <- vector(mode="list", length=length(traitOutput))
    ## Initialize df for results
    clpm.estimates <- data.frame(
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
        state = numeric(),
        samplesize = numeric()
    )
    for (j in 1:length(traitOutput)) {
        bula <- j
        ## Extract results for one state
        fit_bula <- traitOutput[[j]]$result$result
        ## Extract warnings
        clpm.warnings[j] <- list(traitOutput[[j]]$warnings)
        ## Extract errors
        clpm.errors[j] <- list(traitOutput[[j]]$result$error)
        ## Skip results which had an error
        if(!is.null(fit_bula)) {
            estimate <- standardizedSolution(fit_bula)
            ## Extract Fit Measures
            clpm.fit[j] <- list(fitMeasures(fit_bula))
            ## Extract results
            tempResults <- extract.est.clpm(clpm.labels[k, ], estimate)
            ## Add additional info (state and samplesize)
            tempResults$state <- bula
            tempResults$samplesize <- as.numeric(summary(fit_bula)$data$nobs)
            clpm.estimates[j,] <- tempResults
        }
    }
    ## Restructure clpm.estimates
    tempTrait <- clpm.estimates %>%
        pivot_longer(cols = c(r1:st.ub, samplesize))
    ifelse(!exists("singleTraitOutput"),
           singleTraitOutput <- tempTrait,
           singleTraitOutput <- rbind(singleTraitOutput, tempTrait))
    stateWarnings[k] <- list(clpm.warnings)
    stateErrors[k] <- list(clpm.errors)
    stateFit[k] <- list(clpm.fit)
}

names(stateWarnings) <- clpm.labels[[1]]
names(stateErrors) <- clpm.labels[[1]]
names(stateFit) <- clpm.labels[[1]]

## Create wide version of singleTraitOutput
singleTraitOutput.w <- singleTraitOutput %>%
    pivot_wider(id_cols=state,
                names_from = c(trait, name),
                names_sep = ".")


write_csv(singleTraitOutput.w, file="results/clpm.states.single.estimates.csv")
save(stateWarnings, file="results/clpm.single.warnings.RData")
save(stateErrors, file="results/clpm.single.errors.RData")
save(stateFit, file="results/clpm.single.fit.RData")
