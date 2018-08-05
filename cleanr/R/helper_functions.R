get_most_common_value <- function(v){
  x   <- rev(sort(table(v)))[1] / length(v)
  xdf <- vec_to_tibble(x)
  colnames(xdf) <- c("value", "prop")
  return(xdf)
}