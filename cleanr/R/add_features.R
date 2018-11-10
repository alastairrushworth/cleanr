
library(timeDate)
library(chron)


add_time_features <- function(df, holidays = "US", lag_stuff = NULL){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # time based features
  is_date <- function(v) any(class(v) %in% c("Date", "datetime", "POSIXct", "POSIXt"))
  df_time <- df %>% select_if(is_date)
  if(ncol(df_time) > 1){
    # define holiday list
    hlist <- c("USChristmasDay", "USGoodFriday", "USIndependenceDay", "USLaborDay", "USNewYearsDay", "USThanksgivingDay")        
    myholidays  <- chron::dates(as.character(timeDate::holiday(1950:2049, hlist)), format = "Y-M-D")
    
    
    df_new  <- tibble(x = character(length = nrow(df_time)))[, -1]
    for(i in 1:ncol(df_time)){
      col_i <- colnames(df_time)[i]
      dt_i <- df_time[, i, drop = T]
      # time and date subfeatures
      try(df_new[paste(col_i, "second", sep = "_")] <- lubridate::second(dt_i), silent = T)
      try(df_new[paste(col_i, "minute", sep = "_")] <- lubridate::minute(dt_i), silent = T)
      try(df_new[paste(col_i, "hour_of_day", sep = "_")] <- lubridate::hour(dt_i), silent = T)
      try(df_new[paste(col_i, "day_of_week", sep = "_")] <- lubridate::wday(dt_i), silent = T)
      try(df_new[paste(col_i, "day_of_month", sep = "_")] <- lubridate::mday(dt_i), silent = T)
      try(df_new[paste(col_i, "day_of_year", sep = "_")] <- lubridate::yday(dt_i), silent = T)
      try(df_new[paste(col_i, "week_of_year", sep = "_")] <- lubridate::week(dt_i), silent = T)
      try(df_new[paste(col_i, "month_number", sep = "_")] <- lubridate::month(dt_i), silent = T)
      try(df_new[paste(col_i, "year", sep = "_")] <- lubridate::year(dt_i), silent = T)
      # holiday features
      try(df_new[paste(col_i, "is_holiday", sep = "_")] <- chron::is.holiday(dt_i, myholidays), silent = T)
      }
    # remove anything that is redundant
    df_new %<>% drop_columns %>% drop_cor(abs_cor = 0.995, verbose = F)
    
    # append to original data
    df <- bind_cols(df, df_new)
    
    
  } else {
    cat(paste("\U2022 ", silver("<no time features detected>"), sep = ""))
  }

  # invisibly return the df
  invisible(df)
}