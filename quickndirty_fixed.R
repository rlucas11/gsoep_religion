################################################################################
# Quick and dirty fixed effects analyses
################################################################################

################################################################################
# Prepare data
################################################################################

# Read data
dat <- read.csv("data/final.csv")


# Keep the following variables
vars <- c("pid", "sex", "gebjahr",
          paste0("relig", c("05", "09", "13", "17")),
          paste0("cns", c("05", "09", "13", "17")),
          paste0("ext", c("05", "09", "13", "17")),
          paste0("agr", c("05", "09", "13", "17")),
          paste0("neu", c("05", "09", "13", "17")),
          paste0("opn", c("05", "09", "13", "17")))

dat <- dat[, vars]

# Manual reshape wide to long

dat5 <- dat[, c("pid", paste0(c("relig", "cns", "ext", "agr", "neu", "opn"), "05"))]
names(dat5) <- c("pid", "relig", "cns", "ext", "agr", "neu", "opn")
dat5$year <- 5

dat9 <- dat[, c("pid", paste0(c("relig", "cns", "ext", "agr", "neu", "opn"), "09"))]
names(dat9) <- c("pid", "relig", "cns", "ext", "agr", "neu", "opn")
dat9$year <- 9

dat13 <- dat[, c("pid", paste0(c("relig", "cns", "ext", "agr", "neu", "opn"), "13"))]
names(dat13) <- c("pid", "relig", "cns", "ext", "agr", "neu", "opn")
dat13$year <- 13

dat17 <- dat[, c("pid", paste0(c("relig", "cns", "ext", "agr", "neu", "opn"), "17"))]
names(dat17) <- c("pid", "relig", "cns", "ext", "agr", "neu", "opn")
dat17$year <- 17

dat_long <- rbind(dat5, dat9, dat13, dat17)

# Pooled cross-sectional correlations as a sanity check
cor.test(dat_long$relig, dat_long$cns) # -.05
cor.test(dat_long$relig, dat_long$ext) # .01
cor.test(dat_long$relig, dat_long$agr) # -.08
cor.test(dat_long$relig, dat_long$neu) # -.00
cor.test(dat_long$relig, dat_long$opn) # -.03

# I assume this is coded in a manner so that higher number = fewer church visits
# I will recode this so I do not get confused
dat_long$relig_r <- 6 - dat_long$relig

################################################################################
# Fixed effects models
################################################################################
library(plm)

# All in one
combined <- plm(relig_r ~ cns + ext + agr + neu + opn, data = dat_long,
             index = c("pid", "year"), model = "within")
summary(combined)
# positive effect of openness on church attendance


# Individual associations
fix_cns <- plm(relig_r ~ cns, data = dat_long,
               index = c("pid", "year"), model = "within")
summary(fix_cns)

fix_ext <- plm(relig_r ~ ext, data = dat_long,
             index = c("pid", "year"), model = "within")
summary(fix_ext)

fix_agr <- plm(relig_r ~ agr, data = dat_long,
               index = c("pid", "year"), model = "within")
summary(fix_agr)

fix_neu <- plm(relig_r ~ neu, data = dat_long,
               index = c("pid", "year"), model = "within")
summary(fix_neu)

fix_opn <- plm(relig_r ~ opn, data = dat_long,
               index = c("pid", "year"), model = "within")
summary(fix_opn)

