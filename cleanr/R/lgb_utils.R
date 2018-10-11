fit_lgbm <- function(df, label = NULL, ...){
  
  # check that lightgbm is available
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("Package lightgbm needed for this function to work. Please install it by following instructions at https://github.com/Microsoft/LightGBM/tree/master/R-package", call. = FALSE)
  }
  
  # drop label from df
  label_ind <- which(colnames(df) == label)
  label_vec <- df[[label]]
  dftrain <- df[, -label_ind]
  
  # prepare df and extract label
  dftrain <- lightgbm::lgb.prepare_rules(dftrain)

  # convert to matrix then to lgb
  rules <- dftrain$rules
  dftrain <- as.matrix(dftrain$data)
  
  # perform cross validation
  dftrain <- lightgbm::lgb.Dataset(data = dftrain, label = label_vec)
  modelcv <- lightgbm::lgb.cv(data = dftrain, early_stopping_rounds = 50, ...)# objective = "binary", nfold = 10, nrounds = 100)
  niter <- length(modelcv$record_evals$valid$binary_logloss$eval)
  nrounds <- ifelse(modelcv$best_iter == -1, niter, modelcv$best_iter)
  
  # train the model for the best number of its
  a     <- list(...)
  a$nrounds <- nrounds
  a$data <- dftrain
  model <- do.call(lgb.train, a)

  # output
  out_list <- list(model = model, rules = rules, label = label)
}


predict_lgbm <- function(fit_object, newdata){
  
  # check that lightgbm is available
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("Package lightgbm needed for this function to work. Please install it by following instructions at https://github.com/Microsoft/LightGBM/tree/master/R-package", call. = FALSE)
  }
  
  # drop lable from df
  label_ind <- which(colnames(newdata) == fit_object$label)
  label_vec <- newdata[[fit_object$label]]
  newdata <- newdata[, -label_ind]
  
  # prepare df and extract label
  newdata <- lightgbm::lgb.prepare_rules(newdata, rules = fit_object$rules)
  
  # convert to matrix then to lgb
  newdata <- as.matrix(newdata$data)


  predict(fit_object$model, newdata)
  
}