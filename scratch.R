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

