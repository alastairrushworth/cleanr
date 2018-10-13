drop_columns <- function(df, top_n = 10, verbose = T){
  df %>%
    drop_constant(verbose = verbose) %>%
    drop_cor(abs_cor = 1, verbose = verbose) %>% 
    drop_imbalance(verbose = verbose)
}