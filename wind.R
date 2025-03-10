library(opera)
library(mgcv)
library(caret)
library(quadprog)
library(xgboost)
library(ranger)
library(lightgbm)

source("init.R")


wind <- list()

# generalized additive model

gam.fit <- gam(Wind_power ~
        te(Temp, Nebulosity, k = c(12,12)) +
        s(Wind_power.1) +
        s(Wind_power.7) +
        as.factor(WeekOfYear) +
        WeekDays + DLS + Summer_break +
        Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

wind$long <- predict(gam.fit, newdata = test)


# medium term model

medium.fit <- gam(Wind_power ~
      te(Temp, Wind, k=c(12,12))
    + s(Wind_power.1)
    + s(Wind_power.7)
    + s(WeekOfYear, bs="cc")
    + WeekDays + DLS + Summer_break
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

wind$medium <- predict(medium.fit, newdata=data)
data$WindMedium <- solar$medium
data$WindResiduals <- data$Wind_power - data$WindMedium

label <- data$WindResiduals


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
wind$lgb <- data$WindMedium[test_idx] + lightbm(train, label[train_idx], test, all_vars)

wind$lgb_ol <- data$WindMedium[test_idx] + semi_online(
  function (train, label, test) { lightbm(train, label, test, all_vars) },
  data, label, nrow(train)+1)


# GBM

gbm.fit <- train(Load ~
      Temp
    + Load.1 + Load.7
    + WeekDays + WeekOfYear
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train,
    method = "gbm")

wind$gbm <- predict(gbm.fit, newdata = test)




# autoregressive correction

ar.forecast <- numeric(length(test_idx))
for (i in seq(test_idx)) {
  ar.fit <- ar(data$WindResiduals[1:(test_idx[i] - 1)])
  ar.forecast[i] <- as.numeric(predict(ar.fit)$pred) + data$WindMedium[test_idx[i]]
}


wind$ar <- ar.forecast

# Gradient boosting model

gbm.fit <- train(Wind_power ~
      Temp
    + Wind_power.1 + Wind_power.7
    + WeekDays + WeekOfYear
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train,
    method = "gbm")

gbm.forecast <- predict(gbm.fit, newdata = test)






# QRF on residuals



qrf <- function(data, label_, test) {

  model <- ranger(WindResiduals ~
            Wind
          + Temp
          + Wind_power.1 + Wind_power.7
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

wind$qrf_ol <- data$WindMedium[test_idx] + semi_online(qrf, data, label, nrow(train)+1)



# XGBoost semi-online et normal

xgb_rsme <- function(train, label, test){ xgboost_rsme(train, label, test, all_vars) }
xgb_quant <- function(train, label, test){ xgboost_quant(train, label, test, all_vars) }

wind$xgb <- data$WindMedium[test_idx] + xgb_rsme(train, label[train_idx], test)
wind$xgb_ol <- data$WindMedium[test_idx] + semi_online(xgb_rsme, data, label, nrow(train)+1)

wind$qxgb <- data$WindMedium[test_idx] + xgb_rsme(train, label[train_idx], test)

# Aggregation

experts <- cbind(
  xgb_ol = wind$xgb_ol,
  qxgb   = wind$qxgb,
  lgb_ol = wind$lgb_ol,
  qrf_ol = wind$qrf_ol,
  long   = wind$long
)

available <- 1:nrow(test)-1
mix <- mixture(model = "MLpol")
mix <- predict(mix, newexpert = experts[available,], newY = test$Wind_power[available], online = TRUE)
last_pred <- predict(mix, newexperts = experts[-available, ], online = FALSE, type = 'response')

wind$final <- rbind(mix$prediction, last_pred)


# Oracle
plot(mix)

plot_predictions(wind$final, test[available,], "Wind_power")

