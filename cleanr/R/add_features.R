add_features <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # time based features
  is_date <- function(v) any(class(v) %in% c("Date", "datetime", "POSIXct", "POSIXt"))
  df_time <- df %>% select_if(is_date)
  if(ncol(df_time) > 1){
    df_new  <- tibble(x = character(length = nrow(df_time)))[, -1]
    for(i in 1:ncol(df_time)){
      col_i <- colnames(df_time)[i]
      try(df_new[paste(col_i, "second", sep = "_")] <- lubridate::second(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "minute", sep = "_")] <- lubridate::minute(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "hour_of_day", sep = "_")] <- lubridate::hour(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "day_of_week", sep = "_")] <- lubridate::wday(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "day_of_month", sep = "_")] <- lubridate::mday(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "day_of_year", sep = "_")] <- lubridate::yday(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "week_of_year", sep = "_")] <- lubridate::week(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "month_number", sep = "_")] <- lubridate::month(df_time[, i, drop = T]), silent = T)
      try(df_new[paste(col_i, "year", sep = "_")] <- lubridate::year(df_week[, i]), silent = T)
    }
    # remove anything that is redundant
    df_new %<>% drop_columns %>% drop_cor(abs_cor = 0.995, verbose = F)
    
    # append to original data
    df <- bind_cols(df, df_new)
  }

  # invisibly return the df
  invisible(df)
}