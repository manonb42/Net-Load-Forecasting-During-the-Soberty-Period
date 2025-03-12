library(opera)
library(mgcv)
library(caret)
library(quadprog)
library(xgboost)
library(ranger)

source("init.R")


tau = 0.8

solar <- list()

# generalized additive model

gam.fit <- gam(Solar_power ~
        te(Temp, Nebulosity, k = c(12,12)) +
        s(Solar_power.1) +
        s(Solar_power.7) +
        as.factor(WeekOfYear) +
        WeekDays + DLS + Summer_break +
        Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

solar$long <- predict(gam.fit, newdata = test)





# medium term model

medium.fit <- gam(Solar_power ~
      te(Temp, Wind, k=c(12,12))
    + s(Solar_power.1)
    + s(Solar_power.7)
    + s(WeekOfYear, bs="cc")
    + WeekDays + DLS + Summer_break
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

solar$medium <- predict(medium.fit, newdata=data)
data$SolarMedium <- solar$medium
data$SolarResiduals <- data$Solar_power - data$SolarMedium

label <- data$SolarResiduals


# Residuals correction


features <- c("NumDate", "Temp", "Temp_s99_min", "Temp_s99_max",
              "Wind", "Wind_weighted", "Nebulosity", "Nebulosity_weighted",
              "Load.1", "Load.7", "Net_demand.1", "Net_demand.7",
              "Solar_power.1", "Solar_power.7", "Wind_power.1", "Wind_power.7",
              "DayOfYear")

categorical_vars <- c("DayOfWeek", "DLS", "Holiday", "BH")

all_vars <- c(features, categorical_vars)


# Gradient boosting model

## LightGBM
solar$lgb <- data$SolarMedium[test_idx] + lightbm(train, label[train_idx], test, all_vars)

solar$lgb_ol <- data$SolarMedium[test_idx] + semi_online(
  function (train, label, test) { lightbm(train, label, test, all_vars) },
  data, label, nrow(train)+1)



# QRF on residuals


qrf <- function(data, label_, test) {

  model <- ranger(SolarResiduals ~
            Wind
          + Temp
          + Solar_power.1 + Solar_power.7
          + DayOfWeek,
    data = data[train_idx, ],
    importance = "permutation",
    quantreg = TRUE,
    num.trees = 500)

  predict(model,
          data = data[test_idx,],
          quantiles = tau,
          type = "quantiles")$predictions[,]

}

solar$qrf_ol <- data$SolarMedium[test_idx] + semi_online(qrf, data, label, nrow(train)+1)



# XGBoost semi-online et normal

xgb_rsme <- function(train, label, test){ xgboost_rsme(train, label, test, all_vars) }
xgb_quant <- function(train, label, test){ xgboost_quant(train, label, test, all_vars) }

solar$xgb <- data$SolarMedium[test_idx] + xgb_rsme(train, label[train_idx], test)
solar$xgb_ol <- data$SolarMedium[test_idx] + semi_online(xgb_rsme, data, label, nrow(train)+1)

solar$qxgb <- data$SolarMedium[test_idx] + xgb_quant(train, label[train_idx], test)

# Aggregation

experts <- cbind(
  xgb    = solar$xgb,
  xgb_ol = solar$xgb_ol,
  qxgb   = solar$qxgb,
  lgb    = solar$lgb,
  lgb_ol = solar$lgb_ol,
  qrf_ol = solar$qrf_ol,
  long   = solar$long
)

available <- 1:nrow(test)-1
mix <- mixture(model = "MLpol")
mix <- predict(mix, newexpert = experts[available,], newY = test$Solar_power[available], online = TRUE)
last_pred <- predict(mix, newexperts = experts[-available, ], online = FALSE, type = 'response')

solar$final <- rbind(mix$prediction, last_pred)
predictions.solar <- solar$final

# Oracle
plot(mix)

plot_predictions(solar$final, test[available,], "Solar_power")

