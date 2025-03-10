#'
semi_online <- function(algorithm, data, label, test_start, chunk_size = 10){
  n <- nrow(data) - test_start + 1 
  out <- numeric(n)

  for (i in seq(0, n-1, by = chunk_size)) {
    train_range <- 1:(test_start+i-1)
    test_range <- (i+1):min(i + chunk_size, n)
    test_range_2 <- (test_start+i):min(test_start +i + chunk_size - 1, nrow(data))

    cat("Iteration", i/10, i, min(i + chunk_size - 1, n), "\n")
    out[test_range] <- algorithm(data[train_range,], label[train_range], data[test_range_2,])
  }
  out
}



lightbm <- function(train, label, test, all_vars) {
  
    train <- model.matrix(~ . -1, data = train[, all_vars])
    test  <- model.matrix(~ . -1, data = test[, all_vars])
    
    train_indices <- sample(seq_len(nrow(train)), size = floor(0.8 * nrow(train)))

    valid <- lgb.Dataset(data = train[-train_indices,], label = label[-train_indices])
    train <- lgb.Dataset(data = train[train_indices,], label = label[train_indices])

    params <- list(
        objective = "regression",
        metric = "rmse",
        learning_rate = 0.05,
        num_leaves = 31,
        max_depth = -1,
        min_data_in_leaf = 20,
        feature_fraction = 0.9,
        bagging_fraction = 0.8,
        bagging_freq = 5,
        lambda_l1 = 0.1,
        lambda_l2 = 0.1
    )

    cv_results <- lgb.cv(
        params = params,
        data = train,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 50,
        verbose = 0
    )

    model <- lgb.train(
        params = params,
        data = train,
        nrounds = cv_results$best_iter,
        valids = list(validation = valid),
        verbose = 0
    )

    predict(model, newdata = test)
}


xgboost_rsme <- function(train, label, test, all_vars){

    train <- model.matrix(~ . -1, data = train[, all_vars])
    test  <- model.matrix(~ . -1, data = test[, all_vars])
    model <- xgboost(
        data = train, label = label,
        nrounds = 500, verbose = 0,
        params = list(
                objective = "reg:squarederror",
                eta = 0.05,
                max_depth = 6,
                subsample = 0.8)
    )

    predict(model, newdata = test)
}

xgboost_quant <- function(train, label, test, all_vars, tau = 0.8){

    train <- model.matrix(~ . -1, data = train[, all_vars], label=label)
    test  <- model.matrix(~ . -1, data = test[, all_vars])


    quantile_obj <- function(preds, dtrain) {
        labels <- getinfo(dtrain, "label")
        errors <- labels - preds
        grad <- ifelse(errors > 0, -tau, 1 - tau)
        hess <- rep(1, length(labels))  # approximation constante de la hessienne
        return(list(grad = grad, hess = hess))
    }
    quantile_eval <- function(preds, dtrain) {
        labels <- getinfo(dtrain, "label")
        errors <- labels - preds
        loss <- mean(ifelse(errors > 0, tau * errors, (tau - 1) * errors))
        return(list(metric = "quantile_loss", value = loss))
    }


    model <- xgboost(
        data = train, label = label,
        nrounds = 500, verbose = 0,
        params = list(
            objective = quantile_obj,
            eval_metric = quantile_eval,
            eta = 0.05,
            max_depth = 6,
            subsample = 0.8
        )
    )

    predict(model, newdata = test)
}
