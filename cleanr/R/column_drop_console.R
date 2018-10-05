column_drop_console <- function(type, names_to_drop){
  cat(paste(type, " ", sep = ""))
  cat(paste("\U2022 ", names_to_drop, sep = ""))
  cat("\n")
}