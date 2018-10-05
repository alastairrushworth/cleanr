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
      column_drop_console(names_to_drop = names_to_drop, type = "Correlated columns dropped")
      # actually drop these columns
      df <- df %>% select(-names_to_drop)
    } else {
      column_drop_console(type = "Correlated columns dropped")
    }
  } else {
    column_drop_console(type = "Correlated columns dropped")
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}