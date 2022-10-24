## Setup (load data and packages)
source('analysisSetup.R')

## Restrict sample to those with at least one item per construct
source('restrictSample.R')

## ## Testing (comment out when running actual analysis)
## ## This creates random state data and will only run through the first 3
## data$first.state <- sample(1:50, nrow(data), replace=TRUE)
## bula_neu <- sort(unique(data$first.state))[1:3]

## Get correlations
source('correlations.R')

## Run original model (all traits, latent, state-by-state)
source('clpm.latent.all.R')

## Run RI-CLPM equivalent of original (all traits, latent, state-by-state)
source('riclpm.latent.all.R')

## Run original model with observed traits (all traits, observed, state-by-state)
source('clpm.observed.all.R')

## Run RI-CLPM with observed traits (all traits, observed, state-by-state)
source('riclpm.observed.all.R')

## Run CLPM with latent variables, trait-by-trait (trait-by-trait, latent, state-by-state)
source('clpm.single.R')

## Run RI-CLPM with latent variables, trait-by-trait (trait-by-trait, latent, state-by-state)
source('riclpm.single.R')
