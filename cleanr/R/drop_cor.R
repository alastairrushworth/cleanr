drop_cor<- function(df, abs_cor = 1, verbose = T){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # filter to only the numeric variables
  df_numeric <- df %>% select_if(is.numeric)
  
  # remove anything that is constant
  df_numeric <- df_numeric %>% select(-which(sapply(df_numeric, sd) == 0))
  
  # calculate correlation coefficients
  if(ncol(df_numeric) > 0){
    cor_num_mat <- suppressWarnings(cor(df_numeric, use = "pairwise.complete.obs"))
    cor_num_mat[lower.tri(cor_num_mat, diag = T)] <- NA
    cor_df <- tibble::as.tibble(cor_num_mat)
    cor_df$X1 <- colnames(cor_df)
    cor_df    <- tidyr::gather(cor_df, key = "X2", value = "cor", -X1)
    cor_df <- cor_df %>% dplyr::filter(!is.na(cor)) %>%
      dplyr::arrange(desc(abs(cor))) %>%
      dplyr::filter(abs(cor) >= abs_cor)
    if(nrow(cor_df) >= 1){
      names_to_drop <- sort(unique(cor_df$X1))
      cat("Constant columns dropped:\n ")
      cat(paste("\U2022 ", names_to_drop, " \n", sep = ""))
      # actually drop these columns
      df <- df %>% select(-names_to_drop)
    } else {
      cat("No highly correlated columns to remove. \n")
    }
  } else {
    cat("No highly correlated columns to remove. \n")
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}