sumna_slow <- function(v) sum(is.na(v))

sumna <- function(v){
  if(class(v) %in% c("numeric", "character", "integer", "logical", "factor")){
    if(class(v) == "numeric")   nasum <- na_numeric(v)
    if(class(v) == "character") nasum <- na_character(v)
    if(class(v) == "integer")   nasum <- na_integer(v)
    if(class(v) == "factor")    nasum <- na_integer(v)
    if(class(v) == "logical")   nasum <- na_logical(v)
  } else {
    nasum <- sumna_slow(v)
  }
  return(nasum)
}

