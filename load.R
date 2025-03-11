library(opera)
library(mgcv)
library(caret)
library(quadprog)
library(xgboost)
library(ranger)
library(lightgbm)

source("init.R")
source("experts.R")

tau = 0.8

load <- list()

# generalized additive model

gam.fit <- gam(Load ~
      te(Temp, Wind, k=c(12,12))
    + s(Load.1)
    + s(Load.7)
    + as.factor(WeekOfYear)
    + WeekDays + DLS + Summer_break
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

load$long <- predict(gam.fit, newdata = test)


# medium term model

medium.fit <- gam(Load ~
      te(Temp, Wind, k=c(12,12))
    + s(Load.1)
    + s(Load.7)
    + s(WeekOfYear, bs="cc")
    + WeekDays + DLS + Summer_break
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train)

load$medium <- predict(medium.fit, newdata=data)
data$LoadMedium <- load$medium
data$LoadResiduals <- data$Load - data$LoadMedium

label <- data$LoadResiduals


# Residuals correction

features <- c("NumDate", "Temp", "Temp_s99_min", "Temp_s99_max",
              "Wind", "Wind_weighted", "Nebulosity", "Nebulosity_weighted",
              "Load.1", "Load.7", "Net_demand.1", "Net_demand.7",
              "Wind_power.1", "Wind_power.7", "Solar_power.1", "Solar_power.7",
              "DayOfYear")

categorical_vars <- c("DayOfWeek", "DLS", "Holiday", "BH")

all_vars <- c(features, categorical_vars)


## LightGBM
load$lgb <- data$LoadMedium[test_idx] + lightbm(
  train, data$LoadResiduals[train_idx], test, all_vars)

load$lgb_ol <- data$LoadMedium[test_idx] + semi_online(
  function (train, label, test) { lightbm(train, label, test, all_vars) },
  data, label, nrow(train)+1)


# Gradient boosting model

gbm.fit <- train(Load ~
      Temp
    + Load.1 + Load.7
    + WeekDays + WeekOfYear
    + Holiday + HolidayA + HolidayB + HolidayC,
    data = train,
    method = "gbm")

load$gbm <- predict(gbm.fit, newdata = test)


# QRF on residuals

qrf <- function(data, label_, test) {
  
  model <- ranger(LoadResiduals ~
         Wind
       + Temp
       + Load.1 + Load.7
       + DayOfWeek,
       data = data,
       importance = "permutation",
       quantreg = TRUE,
       num.trees = 500)

  predict(model,
          data = test,
          quantiles = tau,
          type = "quantiles")$predictions[,]


} 

load$qrf_ol <- data$LoadMedium[test_idx] + semi_online(qrf, data, label, nrow(train)+1)


# XGBoost semi-online et normal

xgb_rsme <- function(train, label, test){ xgboost_rsme(train, label, test, all_vars) }
xgb_quant <- function(train, label, test){ xgboost_quant(train, label, test, all_vars) }

load$xgb <- data$LoadMedium[test_idx] + xgb_rsme(train, label[train_idx], test)
load$xgb_ol <- data$LoadMedium[test_idx] + semi_online(xgb_rsme, data, label, nrow(train)+1)


load$qxgb <- data$LoadMedium[test_idx] + xgb_rsme(train, label[train_idx], test)


# Aggregation

experts <- cbind(
  xgb    = load$xgb,
  xgb_ol = load$xgb_ol,
  qxgb   = load$qxgb,
  lgb    = load$lgb,
  lgb_ol = load$lgb_ol,
  qrf_ol = load$qrf_ol,
  long   = load$long
  
)

available <- 1:nrow(test)-1
mix <- mixture(model = "MLpol", loss.type = list(name = "pinball", tau = 0.8))
mix <- predict(mix, newexpert = experts[available,], newY = test$Load[available], online = TRUE)
last_pred <- predict(mix, newexperts = experts[-available, ], online = FALSE, type = 'response')

load$final <- rbind(mix$prediction, last_pred)
predictions.load <- load$final

# Oracle
plot(mix)


plot_predictions(load$final, test[available,], "Load")
