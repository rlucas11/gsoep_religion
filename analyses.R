library(lavaan)
source("model.R")

data <- read_csv("data/final.csv")


model1 <- sem(model1_main, missing="FIML", estimator="MLR", data=data)
