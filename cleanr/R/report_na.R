# columns with NA columns
report_na <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  # check if there are any NA columns
  na_df <- df %>% select_if(anyNA)
  if(ncol(na_df) > 0){
    console_title("Columns sorted by % missing")
    vec_to_tibble(apply(df, 2, function(v) sum(is.na(v)))) %>%
      mutate(prop = n / nrow(df)) %>%
      dot_bars_na
  } else {
    # print("No missing values detected")
  }
  invisible(df)
}