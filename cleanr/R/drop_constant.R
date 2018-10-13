drop_constant <- function(df, verbose = T){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  unique_1 <- function(v) length(unique(v)) == 1
  
  # cols for removal
  names_to_drop <- c()
  
  # integer constant columns to remove
  df_integer <- df %>% select_if(is.integer)
  if(ncol(df_integer) > 0){
    rm_inds <- which(sapply(df_integer, sd) == 0)
    names_to_drop <- c(names_to_drop, colnames(df_integer)[rm_inds])
  }
  # numeric constant colums to remove
  df_numeric <- df %>% select_if(is.numeric)
  if(ncol(df_numeric) > 0){
    rm_inds <- which(sapply(df_numeric, sd) == 0)
    names_to_drop <- c(names_to_drop, colnames(df_numeric)[rm_inds])
  }
  
  # character constant columns to remove
  df_character <- df %>% select_if(is.character)
  if(ncol(df_character) > 0){
    rm_inds <- which(sapply(df_character, unique_1))
    names_to_drop <- c(names_to_drop, colnames(df_character)[rm_inds])
  }
  
  # factor constant columns to remove
  df_factor <- df %>% select_if(is.factor)
  if(ncol(df_factor) > 0){
    rm_inds <- which(sapply(df_factor, unique_1))
    names_to_drop <- c(names_to_drop, colnames(df_factor)[rm_inds])
  }
  
  # return a message, if requested
  if(verbose){
    if(length(names_to_drop) > 1){
      names_to_drop <- sort(unique(names_to_drop))
      column_drop_console(names_to_drop = names_to_drop, type = "Constant columns dropped")
      df <- df %>% select(-names_to_drop)
    } else {
      column_drop_console(type = "Constant columns dropped")
    }
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}
