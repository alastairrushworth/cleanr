report_imbalance <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  not_list <- function(v) !is.list(v)  
  gdf      <- df %>% select_if(not_list)
  # function to find the percentage of the most common value in a vector
  imb_cols       <- do.call("rbind", lapply(gdf, get_most_common_value))
  imb_cols$names <- colnames(gdf)
  # sort and slice
  console_title("Top most imbalanced features")
  imb_cols %>% 
    arrange(desc(prop)) %>% 
    slice(1:10) %>% 
    dot_bars_imbalance
  invisible(df)
}