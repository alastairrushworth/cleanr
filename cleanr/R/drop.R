drop <- function(df, top_n = 10){
  df %>%
    drop_constant(verbose = T) %>%
    drop_cor(abs_cor = 1, verbose = T) %>% 
    drop_imbalance(verbose = T)
}