report_imbalance <- function(df, top_n = 10){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # exclude columns containing lists
  not_list <- function(v) !is.list(v)  
  gdf      <- df %>% select_if(not_list)
  
  # function to find the percentage of the most common value in a vector
  imb_cols       <- do.call("rbind", lapply(gdf, get_most_common_value))
  imb_cols$names <- colnames(gdf)
  
  # print console title text
  console_title("Top most imbalanced features")
  
  # get top ten most imbalance by common class and pass to histogrammer
  imb_cols %>% 
    arrange(desc(prop)) %>% 
    slice(1:top_n) %>% 
    dot_bars_imbalance
  
  # invisibly return the df for further summaries
  invisible(df)
}