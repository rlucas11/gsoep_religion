# Investigating The Lagged Associations Between Personality and Religion

Entringer et al. (in press) examined the reciprocal associations between the Big Five personality traits and a measure of religiosity in the German SocioEconomic Panel Study (GSOEP). They used the cross-lagged panel model (CLPM), but did not examine alternative models that incorporate a random intercept (such as the random intercept cross-lagged panel model; RI-CLPM). This project reanalyzes the data, focusing both on the size of the cross-sectional and trait-level associations and on the robustness of the lagged effects to alternative specifications. 

- Entringer, T. M., Gebauer, J. E., & Kroeger, H. (in press). Big Five Personality and Religiosity: Bidirectional Cross-Lagged Effects and their Moderation by Culture. Journal of Personality. http://dx.doi.org/10.1111/jopy.12770

## Directory structure

To run the following analyses, you need to set up the correct directory structure. The cleaning and analysis scripts are in the root directory. Within the root directory, there should be four additional directories for "data", "results", "info", and "scripts". Because of the confidential nature of the data, I keep the original data in a separate directory that is specified in the cleaning script. I do keep derived dataframes in the "data" folder, but I include this directory in .gitignore so it is not uploaded to github. 

A list of variables needed for the cleaning script to run is included in the "info" directory. That directory is also used to store some R objects and results. Some final results are saved to the "results" directory. Separate files with the lavaan models are saved in "scripts".

The code is set up to use the STATA files provided by the SOEP. 

## Analysis plan

### Calculate Zero-Order Correlations

- Within each waves
- Aggregating across waves
- For full sample and separately by state

### Replicate original analysis

- Full sample CLPM with all traits entered and modeled using latent traits
- State by state CLPM with all traits entered and modeled using latent traits

### Extend original analysis

#### All traits; latent

- Full sample RI-CLPM with all traits entered and modeled using latent traits
- State by state RI-CLPM with all traits entered and modeled using latent traits

#### All traits, observed

- Full sample CLPM and RI-CLPM with all traits entered and modeled using observed measures
- State by state CLPM and RI-CLPM with all traits entered and modeled using observed measures

#### Single traits; latent

- Full sample CLPM and RI-CLPM with separate models for each trait and modeled using latent variables
- State by state CLPM and RI-CLPM with separate models for each trait and modeled using latent variables

#### Single traits; observed

- Full sample CLPM and RI-CLPM with separate models for each trait and modeled using observed measures
- State by state CLPM and RI-CLPM with separate models for each trait and modeled using observed measures


## Notes

- OSF Site With Code: 
[https://osf.io/gn8xa/?view_only=27038964954d4485a2d418dd3a469838](https://osf.io/gn8xa/?view_only=27038964954d4485a2d418dd3a469838)

- Main Model:
[https://osf.io/3gyur?view_only=27038964954d4485a2d418dd3a469838](https://osf.io/3gyur?view_only=27038964954d4485a2d418dd3a469838)

- Main Analysis:
[https://osf.io/zrp6h?view_only=27038964954d4485a2d418dd3a469838](https://osf.io/zrp6h?view_only=27038964954d4485a2d418dd3a469838)

- Meta-Analysis: 
[https://osf.io/5ht8n?view_only=27038964954d4485a2d418dd3a469838](https://osf.io/5ht8n?view_only=27038964954d4485a2d418dd3a469838)

### Variables

- State variable in hbrutt file, variable name: bula_h

#### Religion variable: 
- [https://paneldata.org/soep-core/datasets/pl/pli0098_v2](https://paneldata.org/soep-core/datasets/pl/pli0098_v2)
- pli0098_v2
- pli0098_h

This variable needs to be recoded, both because the response options changed and to reverse score.

#### Personality variables: 
- [https://paneldata.org/soep-core/instruments/soep-core-2019-pe2/164](https://paneldata.org/soep-core/instruments/soep-core-2019-pe2/164)

- Items (see code for reverse scoring and calculation of scale scores):

plh0212
plh0213
plh0214
plh0215
plh0216
plh0217
plh0218
plh0219
plh0220
plh0221
plh0222
plh0223
plh0224
plh0225
plh0226


