## Names of all variables
varNames <- c(paste0(
        c(rep("cns", 4), rep("ext", 4), rep("agr", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("01", 15),
        c(rep("", 4), rep("", 4), rep("r", 4), rep("", 4), rep("", 4))
    ),
    paste0(
        c(rep("agr", 4), rep("cns", 4), rep("ext", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("02", 15),
        c(rep("", 4), rep("r", 4), rep("", 4), rep("", 4), rep("", 4))
    ),
    paste0(
        c(rep("cns", 4), rep("ext", 4), rep("agr", 4), rep("opn", 4), rep("neu", 4)),
        rep(c("05", "09", "13", "17"), 4),
        rep("03", 15),
        c(rep("", 4), rep("r", 4), rep("", 4), rep("", 4), rep("r", 4))
    ),
    paste0("relig", c("05", "09", "13", "17")),
    paste0("relig", c("05", "09", "13", "17"), "r"))

grep("ext", varNames)

## Select variable specific sets of names
agrNames <- varNames[grep("agr", varNames)]
cnsNames <- varNames[grep("cns", varNames)]
extNames <- varNames[grep("ext", varNames)]
neuNames <- varNames[grep("neu", varNames)]
opnNames <- varNames[grep("opn", varNames)]
relNames <- varNames[grep("rel", varNames)][1:4] ## Only select reverse-scored


## Calculate number of missing for each trait
data <- data %>%
    rowwise() %>%
    mutate(agrMiss = sum(is.na(c_across(all_of(agrNames)))),
           cnsMiss = sum(is.na(c_across(all_of(cnsNames)))),
           extMiss = sum(is.na(c_across(all_of(extNames)))),
           neuMiss = sum(is.na(c_across(all_of(neuNames)))),
           opnMiss = sum(is.na(c_across(all_of(opnNames)))),
           relMiss = sum(is.na(c_across(all_of(relNames)))))

## Create selection variable
## Selected if they have at least one item per construct
## As per original study
data[which(data$agrMiss < 12 &
    data$cnsMiss < 12 &
    data$extMiss < 12 &
    data$neuMiss < 12 &
    data$opnMiss < 12 &
    data$relMiss < 4), "missingSelect"] <- 1

data <- filter(data, missingSelect == 1)
