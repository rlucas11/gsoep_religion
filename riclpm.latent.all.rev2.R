################################################################################
## Setup
################################################################################

## Read cleaned data
data <- read_csv("data/filteredData.csv")

## Source model files
source("scripts/fullRiclpmMod2.R")      ## Lavaan model name is 'model1_riclpmMod


################################################################################
## Functions to extract estimates
################################################################################

## List estimate labels for estimate-extraction function
## s_(var) = stability
## r1_(v1v2) = initial wave correlations
## r2_(v1v2) = subsequent wave correlations
## c_r(t) = cross-lag, religion regressed on trait
## c_(t)r = cross-lag, trait regressed on religion
## r1_(t)r = correlation between randome intercepts
riclpm.labels <- data.frame(matrix(c(
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
names(riclpm.labels) <- c("trait", "cl_rOnt", "cl_tOnr", "stability", "r1", "r2", "ri_r")


## Function to extract standardized estimates for meta-analysis
## Extracts estimate of r, along with 95% CIs
## Function works on one trait at a time (e.g., a line of estimateLabels)

extract.est.riclpm <- function(riclpm.labels, results) {
    riclpm.results <- data.frame(
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
    riclpm.results[1, "trait"] <- riclpm.labels$trait
    riclpm.results[1, "r1"] <- results[results$label == riclpm.labels[[1, "r1"]],
                                       "est.std"]
    riclpm.results[1, "r1.lb"] <- results[results$label == riclpm.labels[[1, "r1"]],
                                          "ci.lower"]
    riclpm.results[1, "r1.ub"] <- results[results$label == riclpm.labels[[1, "r1"]],
                                          "ci.upper"]
    riclpm.results[1, "r2"] <- mean(results[results$label == riclpm.labels[[1, "r2"]],
                                            "est.std"])
    riclpm.results[1, "r2.lb"] <- mean(results[results$label == riclpm.labels[[1, "r2"]],
                                               "ci.lower"])
    riclpm.results[1, "r2.ub"] <- mean(results[results$label == riclpm.labels[[1, "r2"]],
                                               "ci.upper"])
    riclpm.results[1, "rt.cl"] <- mean(results[results$label == riclpm.labels[[1, "cl_rOnt"]],
                                               "est.std"])
    riclpm.results[1, "rt.cl.lb"] <- mean(results[results$label == riclpm.labels[[1, "cl_rOnt"]],
                                                  "ci.lower"])
    riclpm.results[1, "rt.cl.ub"] <- mean(results[results$label == riclpm.labels[[1, "cl_rOnt"]],
                                                  "ci.upper"])
    riclpm.results[1, "tr.cl"] <- mean(results[results$label == riclpm.labels[[1, "cl_tOnr"]],
                                               "est.std"])
    riclpm.results[1, "tr.cl.lb"] <- mean(results[results$label == riclpm.labels[[1, "cl_tOnr"]],
                                                  "ci.lower"])
    riclpm.results[1, "tr.cl.ub"] <- mean(results[results$label == riclpm.labels[[1, "cl_tOnr"]],
                                                  "ci.upper"])
    riclpm.results[1, "st"] <- mean(results[results$label == riclpm.labels[[1, "stability"]],
                                            "est.std"])
    riclpm.results[1, "st.lb"] <- mean(results[results$label == riclpm.labels[[1, "stability"]],
                                               "ci.lower"])
    riclpm.results[1, "st.ub"] <- mean(results[results$label == riclpm.labels[[1, "stability"]],
                                               "ci.upper"])
    riclpm.results[1, "ri_r"] <- mean(results[results$label == riclpm.labels[[1, "ri_r"]],
                                              "est.std"])
    riclpm.results[1, "ri_r.lb"] <- mean(results[results$label == riclpm.labels[[1, "ri_r"]],
                                                 "ci.lower"])
    riclpm.results[1, "ri_r.ub"] <- mean(results[results$label == riclpm.labels[[1, "ri_r"]],
                                                 "ci.upper"])
    return(riclpm.results)
}


################################################################################
## Analysis by state
################################################################################

## Create function for running models in one selected state
## "data" is the full dataset; "bula" is the number of the state to be selected
runModelsRiclpm <- function(bula, data) {
    temp <- subset(data, first.state == bula)
    fit_bula <- sem(model1_riclpmMod,
                    missing="FIML",
                    estimator="MLR",
                    data=temp,
                    em.h1.iter.max=20000)
    return(fit_bula)
}

## Get list of states
bula_neu <- sort(unique(data$first.state))

## ## Temporary for testing
## data$first.state <- sample(1:50, nrow(data), replace=TRUE)
## bula_neu <- sort(unique(data$first.state))
## stateOutput <- map(bula_neu[1:3], quietly(safely(runModelsRiclpm)), data)

## Use purrr to run through states, saving output and errors (using "safely")
plan(multisession)
stateOutput <- future_map(bula_neu, quietly(safely(runModelsRiclpm)), data)

## Extract estimates for each state
riclpm.warnings <- vector(mode="list", length=length(stateOutput))
riclpm.errors <- vector(mode="list",length=length(stateOutput))
for (j in 1:length(stateOutput)) {
    bula <- j
    ## Extract results for one state
    fit_bula <- stateOutput[[j]]$result$result
    ## Extract warnings
    riclpm.warnings[j] <- list(stateOutput[[j]]$warnings)
    ## Extract errors
    riclpm.errors[j] <- list(stateOutput[[j]]$result$error)
    ## Skip results which had an error
    if(!is.null(fit_bula)) {
        estimate <- standardizedSolution(fit_bula)

        ## Initialize df for results
        riclpm.estimates <- data.frame(
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
            ri.r = numeric(),
            ri.r.lb = numeric(),
            ri.r.ub = numeric()
        )
        ## Extract results
        for (i in 1:nrow(riclpm.labels)) {
            tempResults <- extract.est.riclpm(riclpm.labels[i, ], estimate)
            riclpm.estimates[i, ] <- tempResults
        }
        ## Make wide file with one line per state
        riclpm.estimates.w <- riclpm.estimates %>%
            pivot_longer(cols=-trait) %>%
            pivot_wider(names_from = c(trait, name),
                        names_sep = ".")

        ## Add additional info (state and samplesize)
        riclpm.estimates.w$state <- bula
        riclpm.estimates.w$samplesize <- as.numeric(summary(fit_bula)$data$nobs)

        ## Aggregate accross states
        ifelse(j == 1,
               riclpm.aggregated.estimates <- riclpm.estimates.w,
               riclpm.aggregated.estimates <- rbind(riclpm.aggregated.estimates,
                                                  riclpm.estimates.w)
               )
    }
}

## Save matrix of results
write_csv(riclpm.aggregated.estimates,
          file=paste0(location,"/riclpm.latent.states.aggregated.estimates.rev2.csv"))
save(riclpm.warnings,
     file=paste0(location,"/riclpm.warnings.rev2.RData"))
save(riclpm.errors,
     file=paste0(location,"/riclpm.errors.rev2.RData"))


## Get fit info

## Parallel Version

getFitMeasures <- function(bula, stateOutput) {
    fitOutput <- fitMeasures(stateOutput[[bula]]$result$result)
}

stateFitRiclpm <- future_map(bula_neu, safely(getFitMeasures), stateOutput)

## Save matrix of results
save(stateFitRiclpm,
     file=paste0(location,"/riclpm.latent.states.aggregated.fit.rev2.RData"))

