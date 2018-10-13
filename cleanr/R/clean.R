# todo
# paste messages for each action
# shrink the column names - max_col_char

# ACTION FUNCTIONS  
# na_impute, na_level
# cat_bundle



clean <- function(df, factors_convert = T, column_names = T,
                  rownames_column = T, column_alpha = T,
                  fix_numeric = T, na_impute = NULL){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # convert any rownames
  if(rownames_column){
    # check first whether rownames aren't just an integer index
    if(!identical(rownames(df), as.character(1:nrow(df)))){
      df <- tibble::rownames_to_column(df)
    }
  }
  
  # factors to strings
  if(factors_convert){
    df %<>% mutate_if(is.factor, as.character)
  } 
  
  # colnames to lower, remove dots and space, ensure uniqueness
  if(column_names){
    names_orig <- colnames(df)
    names_new <- tolower(names_orig)
    names_new <- gsub(" |\\.", "_", names_new)
    colnames(df) <- make.names(names_new, unique = T)
  }
  
  # arrange columns alphabetically
  if(column_alpha){
    df %<>% select(sort(colnames(.)))
  }
  
  # convert to a tibble
  df %<>% as.tibble
  
  # invisibly return the df for further summaries
  invisible(df)
}

