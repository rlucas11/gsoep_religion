################################################################################
## Setup
################################################################################

library(tidyverse)
library(lavaan)

## Read cleaned data
data <- read_csv("data/filteredData.csv")


## Source model files
source("scripts/originalModelMod.R") ## Lavaan model name: model1_mod


################################################################################
## Functions to extract estimates
################################################################################

## List of estimate labels for estimate-extraction function
## s_(var) = stability
## r1_(v1v2) = initial wave correlations
## r2_(v1v2) = subsequent wave correlations
## c_r(t) = cross-lag, religion regressed on trait
## c_(t)r = cross-lag, trait regressed on religion
clpm.labels <- data.frame(matrix(c(
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


## ################################################################################
## ## Reproduce original with full sample
## ################################################################################
## ## This takes a long time to run

## clpm.latent.all <- sem(model1_main, missing = "FIML", estimator = "MLR", data = data)
## ## Save to file because it takes so long to run
## sink(file = "info/clpm.latent.all.txt", append = FALSE)
## summary(clpm.latent.all)
## z.clpm.latent.all <- standardizedSolution(clpm.latent.all,
##     type = "std.all", se = TRUE, zstat = TRUE,
##     pvalue = TRUE, ci = TRUE, level = .95, output = "text"
##     )
## z.clpm.latent.all
## fitMeasures(clpm.latent.all)
## sink()

## ## Temporarily save model as file
## save(clpm.latent.all, file = "info/clpm.latent.all.RData")
## load("info/clpm.latent.all.RData")

## ## Extract all the estimates in a loop
## rm(clpm.estimates)
## clpm.estimates <- data.frame(
##         trait = character(),
##         r1 = numeric(),
##         r1.lb = numeric(),
##         r1.ub = numeric(),
##         r2 = numeric(),
##         r2.lb = numeric(),
##         r2.ub = numeric(),
##         rt.cl = numeric(),
##         rt.cl.lb = numeric(),
##         rt.cl.ub = numeric(),
##         tr.cl = numeric(),
##         tr.cl.lb = numeric(),
##         tr.cl.ub = numeric(),
##         st = numeric(),
##         st.lb = numeric(),
##         st.ub = numeric()
## )
## for (i in 1:nrow(clpm.labels)) {
##     tempResults <- extract.est.clpm(clpm.labels[i, ], z.clpm.latent.all)
##     clpm.estimates[i, ] <- tempResults
## }
## clpm.estimates

## clpm.estimates.w <- clpm.estimates %>%
##     pivot_longer(cols=-trait) %>%
##     pivot_wider(names_from = c(trait, name),
##                 names_sep = ".")


################################################################################
## Analysis by state
################################################################################

## Create function for running models in one selected state
## "data" is the full dataset; "bula" is the number of the state to be selected
runModels <- function(bula, data) {
    temp <- subset(data, first.state == bula)
    fit_bula <- sem(model1_mod,
                    missing="FIML",
                    estimator="MLR",
                    data=temp,
                    em.h1.iter.max=20000)
    return(fit_bula)
}

## Get list of states
bula_neu <- sort(unique(data$first.state))

## ## Temporary for testing
## data$first.state <- sample(1:100, nrow(data), replace=TRUE)
## bula_neu <- sort(unique(data$first.state))
## stateOutput <- map(bula_neu[1:3], quietly(safely(runModels)), data)

## Use purrr to run through states, saving output and errors
stateOutput <- map(bula_neu, quietly(safely(runModels)), data)

## Extract estimates for each state
clpm.warnings <- vector(mode="list", length=length(stateOutput))
clpm.errors <- vector(mode="list",length=length(stateOutput))
for (j in 1:length(stateOutput)) {
    bula <- j
    ## Extract results for one state
    fit_bula <- stateOutput[[j]]$result$result
    ## Extract warnings
    clpm.warnings[j] <- list(stateOutput[[j]]$warnings)
    ## Extract errors
    clpm.errors[j] <- list(stateOutput[[j]]$result$error)
    ## Skip results which had an error
    if(!is.null(fit_bula)) {
        estimate <- standardizedSolution(fit_bula)

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
            st.ub = numeric()
        )
        ## Extract results
        for (i in 1:nrow(clpm.labels)) {
            tempResults <- extract.est.clpm(clpm.labels[i, ], estimate)
            clpm.estimates[i, ] <- tempResults
        }
        ## Make wide file with one line per state
        clpm.estimates.w <- clpm.estimates %>%
            pivot_longer(cols=-trait) %>%
            pivot_wider(names_from = c(trait, name),
                        names_sep = ".")

        ## Add additional info (state and samplesize)
        clpm.estimates.w$state <- bula
        clpm.estimates.w$samplesize <- as.numeric(summary(fit_bula)$data$nobs)

        ## Aggregate accross states
        ifelse(j == 1,
               clpm.aggregated.estimates <- clpm.estimates.w,
               clpm.aggregated.estimates <- rbind(clpm.aggregated.estimates,
                                                  clpm.estimates.w)
               )
    }
}

## Save matrix of results plus errors and warnings
write_csv(clpm.aggregated.estimates, file="results/clpm.latent.states.aggregated.estimates.rev.csv")
save(clpm.warnings, file="results/clpm.warnings.rev.RData")
save(clpm.errors, file="results/clpm.errors.rev.RData")

## Get fit info

for (j in 1:length(stateOutput)) {
    bula <- j
    ## Extract one state
    fit_bula <- stateOutput[[j]]$result$result
    ## Skip if error in running original model
    if(!is.null(fit_bula)) {
        stateFit <- fitMeasures(fit_bula)
        cfi <- stateFit["cfi.robust"]
        rmsea <- stateFit["rmsea.robust"]
        srmr <- stateFit["srmr"]
    }
    ## Aggregate results
    ifelse(j == 1,
           clpm.aggregated.fit <- data.frame(bula, cfi, rmsea, srmr),
           clpm.aggregated.fit <- rbind(clpm.aggregated.fit,
                                        data.frame(bula, cfi, rmsea, srmr)))
}

## Save matrix of results
write_csv(clpm.aggregated.fit, file="results/clpm.latent.states.aggregated.fit.rev.csv")           



