# convert 
vec_to_tibble <- function(v) tibble(names = names(v), n = as.vector(v))
