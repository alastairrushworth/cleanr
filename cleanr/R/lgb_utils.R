fit_lgbm <- function(df, label = NULL, valid_prop = NULL, ...){
  
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
  
  # split into validation set and train 
  if(!is.null(valid_prop)){
    train_inds <- sample(1:nrow(dftrain), round(valid_prop * nrow(dftrain), 0))
    valids  <- list(test = lgb.Dataset(data = dftrain[train_inds,  ], label = label_vec[train_inds]))
    dftrain <- lightgbm::lgb.Dataset(data = dftrain[-train_inds, ], label = label_vec[-train_inds])
    model <- lightgbm::lgb.train(data = dftrain, valids = valids,...)
  } else {
    dftrain <- lightgbm::lgb.Dataset(data = dftrain, label = label_vec)
    model <- lightgbm::lgb.train(data = dftrain,...)
  }
  
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