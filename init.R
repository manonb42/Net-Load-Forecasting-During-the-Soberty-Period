library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)


set.seed(123)



prepare_data <- function(data, ipc) {
  ipc$Date <- as.Date(ipc$Date)
  data %>%
    mutate(
      Date      = as.Date(Date),
      NumDate   = as.numeric(Date),
      DayOfWeek = as.factor(WeekDays),
      DayOfYear = as.numeric(format(Date, "%j")),
      WeekOfYear = as.numeric(format(Date, "%V")),

      DLS       = as.factor(DLS),
      Summer_break = as.factor(Summer_break),
      BH        = as.factor(BH),

      Holiday   = as.factor(Holiday),
      HolidayA = as.factor(Holiday_zone_a),
      HolidayB = as.factor(Holiday_zone_b),
      HolidayC = as.factor(Holiday_zone_c),
      
      GlobalIPC = approx(x = as.numeric(ipc$Date), 
                      y = ipc$Global, 
                      xout = as.numeric(Date), 
                      method = "linear", 
                      rule = 2)$y,
      ElecIPC = approx(x = as.numeric(ipc$Date), 
                           y = ipc$Electricite, 
                           xout = as.numeric(Date), 
                           method = "linear", 
                           rule = 2)$y,
    ) %>% mutate(
        LogElecIPC = log(ElecIPC))
}



load_data <- function() {

  ipc   <- read.csv("ipc.csv", stringsAsFactors = FALSE)
  train <- read.csv("train.csv", stringsAsFactors = FALSE)
  test  <- read.csv("test.csv", stringsAsFactors = FALSE)

  test_public_idx  <<- which(test$Usage == "Public")
  test_private_idx <<- which(test$Usage== "Private")

  data <- bind_rows(train, test)

  train_idx <<- 1:nrow(train)
  test_idx  <<- (nrow(train) + 1):nrow(data)

  data <- data %>% select(-Id, -Usage)

  for (col in c("Net_demand", "Wind_power", "Solar_power", "Load")){
    lag_col <- paste0(col, ".1")
    data[[col]][test_idx[-length(test_idx)]] <- data[[lag_col]][test_idx[-1]]
  }


  data <- prepare_data(data, ipc)
  data
}

data <- load_data()
train <- data[train_idx, ]
test <- data[test_idx, ]

available <- 1:(nrow(test) - 1)


compute_loss <- function(prediction, expected, tau = 0.8) {
  error <- expected - prediction[1:length(expected)] 
  mean(ifelse(error > 0, tau * error, (tau - 1) * error))
}


compute_scores <- function(prediction) {
  
  public <- compute_loss(prediction[test_public_idx][-118], test$Net_demand[test_public_idx][-118])
  private <- compute_loss(prediction[test_private_idx], test$Net_demand[test_private_idx])
  cat("Public score: ", public, "\n")
  cat("Private score: ", private, "\n")

}





plot_predictions <- function(predictions, observations, expected_col, tau = 0.8) {
  dates <- observations$Date
  expected <- observations[[expected_col]]

  preds_df <- as.data.frame(predictions)[1:length(dates), , drop = FALSE]


  loss_values <- sapply(colnames(preds_df), function(col) {
    compute_loss(preds_df[[col]], expected, tau)
  })

  if (is.null(colnames(preds_df))) {
    colnames(preds_df) <- paste0("Prediction", seq_len(ncol(preds_df)))
  }

  colnames(preds_df) <- sapply(names(loss_values), function(col) {
    paste0(col, " (loss: ", round(loss_values[col], 2), ")")
  })


  preds_df$Date <- dates

  preds_long <- preds_df %>%
    pivot_longer(cols = -Date, names_to = "Type", values_to = "Value")

  df_expected <- data.frame(Date = dates, Value = expected, Type = "Expected")
  df_combined <- bind_rows(df_expected, preds_long)

  p <- ggplot(df_combined, aes(x = Date, y = Value, color = Type)) +
    geom_line() +
    labs(title = "Expected vs Predictions",  x = "Date", y = "Value")

  ggplotly(p)
}
