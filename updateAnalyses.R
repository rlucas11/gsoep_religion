## Sink messages for later examination
fileName <- file(paste0("results/", format(Sys.time(), "%m-%d-%Y"), ".out.txt"), open="wt")
sink(fileName, type="message")

## Run once
## source('cleaning.R')

## Setup (load data and packages); run each time you run analyses
source('analysisSetup.R')

## Restrict sample to those with at least one item per construct
## source('restrictSample.R')

## Testing (comment out when running actual analysis)
## This creates random state data and will only run through the first 3
## data$first.state <- sample(1:50, nrow(data), replace=TRUE)
## bula_neu <- sort(unique(data$first.state))[1:3]

## ## Get correlations
## source('correlations.R')

##### The code below runs all models
##### Some of these take a *VERY* long time to run

## ## Run original model (all traits, latent, state-by-state)
## source('clpm.latent.all.R')

## ## Run RI-CLPM equivalent of original (all traits, latent, state-by-state)
## source('riclpm.latent.all.R')

## ## Run original model with observed traits (all traits, observed, state-by-state)
## source('clpm.observed.all.R')

## ## Run RI-CLPM with observed traits (all traits, observed, state-by-state)
## source('riclpm.observed.all.R')

## Run CLPM with latent variables, trait-by-trait (trait-by-trait, latent, state-by-state)
## source('clpm.single.R')

## Run RI-CLPM with latent variables, trait-by-trait (trait-by-trait, latent, state-by-state)
## source('riclpm.single.R')

## Run CLPM with observed variables, trait-by-trait (trait-by-trait, observed, state-by-state)
## source('clpm.single.obs.R')

## Run RI-CLPM with observed variables, trait-by-trait (trait-by-trait, observed, state-by-state)
## source('riclpm.single.obs.R')

## Run all models in full sample
## source("fullSample.R")

## Run meta-analysis
## source("metaAnalysisSetup.R")
## source("metaAnalysis.R")

## Revision Analyses
source("revisionAnalyses.R")

## Rerun manuscript file
## rmarkdown::render('religion.Rmd')
## rmarkdown::render('supplement.Rmd')

## Turn off sink
sink()
