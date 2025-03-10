source("init.R")



predictions.demand <- predictions.load - predictions.solar - predictions.wind


plot_predictions(predictions.demand, test[available,], "Net_demand")

submission <- data.frame(
  Id = seq_len(nrow(predictions.demand)),
  Net_demand = predictions.demand
)
write.table(submission, file = "submission.csv", quote = FALSE, sep = ",", dec = ".", row.names = FALSE)

compute_scores(predictions.demand)