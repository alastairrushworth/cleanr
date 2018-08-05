# show how much space the columns and data take up
report_space <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  # input object name
  # data_name     <- deparse(substitute(df))
  # get column size
  col_space     <- sapply(df, pryr::object_size)
  col_max       <- which.max(col_space)
  col_max_size  <- col_space[col_max]
  col_max_names <- names(col_space)[col_max]
  # get column sparsity of numeric features
  val_numeric   <- df %>% select_if(is.numeric) %>% unlist %>% as.numeric
  val_numeric[is.na(val_numeric)] <- 1
  prop_sparse   <- round(mean(val_numeric == 0) * 100, 1)
  # get number of columns
  sz <- format(object.size(df), standard = "auto", unit = "auto", digits = 2L)
  console_title(c(paste("Data has ", ncol(df), " cols, totalling ", sz, sep = ""),
                  paste("Numeric features are ", prop_sparse, "% sparse", sep = ""),
                  "Top columns listed by storage:"))
  vec_to_tibble(col_space) %>% 
    mutate(prop = n / sum(n)) %>%
    arrange(desc(n)) %>%
    slice(1:10) %>%
    dot_bars_space
  invisible(df)
}