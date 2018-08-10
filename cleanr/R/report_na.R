# find columns with missing values
report_na <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # print title text
  console_title("Columns sorted by % missing")
  
  # check if there are any NA columns
  na_df <- df %>% select_if(anyNA)
  if(ncol(na_df) > 0){
    vec_to_tibble(apply(df, 2, function(v) sum(is.na(v)))) %>%
      mutate(prop = n / nrow(df)) %>%
      filter(prop > 0) %>%  
      arrange(desc(prop)) %>%
      slice(1:10) %>%
      dot_bars_na
  } else {
    cat(silver("    << Not applicable >>\n"))
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}