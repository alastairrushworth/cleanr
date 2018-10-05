drop_imbalance <- function(df, max_imbalance = 0.99, include_numeric = F, verbose = T){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # exclude columns containing lists (or numeric if not specified)
  col_exclude <- ifelse(include_numeric, 
                        function(v) !is.list(v), 
                        function(v) !(is.list(v) | is.numeric(v))) 
  gdf         <- df %>% select_if(col_exclude)
  if(ncol(gdf) > 0){
    cnames      <- colnames(gdf)
    # function to find the percentage of the most common value in a vector
    imb_cols       <- do.call("rbind", lapply(gdf, fast_table))
    imb_cols$names <- colnames(gdf)
    
    # get top ten most imbalance by common class and pass to histogrammer
    imb_df <- imb_cols %>% arrange(desc(prop)) %>% filter(prop >= max_imbalance)
    if(nrow(imb_df) > 0){
      names_to_drop <- sort(unique(imb_df$names))
      column_drop_console(names_to_drop = names_to_drop, type = "Imbalanced columns dropped")
      # actually drop these columns
      df <- df %>% select(-names_to_drop)
    } else {
      cat("No highly imbalance features found. \n")
    }
  } else {
    cat("No highly imbalance features found. \n")
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}