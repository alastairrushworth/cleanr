report <- function(df){
  df %>%
    report_space %>%
    report_types %>%
    report_na    %>%
    report_cor   %>% 
    report_imbalance
}