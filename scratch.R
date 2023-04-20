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
