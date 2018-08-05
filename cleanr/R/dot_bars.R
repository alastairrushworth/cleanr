dot_bars_na <- function(sdf, text = ""){
  sdf %<>% filter(prop > 0) %>%  arrange(desc(prop))
  perc <- str_pad(paste(round(sdf$prop * 100, 0), "% ", sep = ""), width = 4, side = "right", pad = " ")
  total_bars <- 30 
  nbars      <- round(sdf$prop * total_bars, 0)
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  for(i in 1:length(sdf$prop)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", perc[i], sdf$names[i], sep = "")))
    cat("\n")
  }
}

dot_bars_cor <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$cor) * total_bars, 0)
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  cor_vals   <- str_pad(gsub("[+]-", "-", paste("+", as.character(round(sdf$cor, 3)), sep = "")), 
                        width = 6, side = "right", pad = "0")
  for(i in 1:length(cor_vals)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", cor_vals[i], " ", sdf$pair[i], sep = "")))
    cat("\n")
  }
}

dot_bars_space <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$prop) * total_bars, 0)
  perc       <- str_pad(paste(round(sdf$prop * 100, 0), "% ", sep = ""), width = 4, side = "right", pad = " ")
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  for(i in 1:length(sdf$prop)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", perc[i], sdf$names[i], sep = "")))
    cat("\n")
  }
}

dot_bars_imbalance <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$prop) * total_bars, 0)
  perc       <- str_pad(paste(round(sdf$prop * 100, 0), "% ", sep = ""), width = 4, side = "right", pad = " ")
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  for(i in 1:length(sdf$prop)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", perc[i], sdf$names[i], 
                    " (", sdf$value[i], ")", sep = "")))
    cat("\n")
  }
}

dot_bars_composition <- function(sdf, text = ""){
  total_bars <- 30
  nbars      <- round(sdf$prop * total_bars, 0)
  predash    <- cumsum(c(0, nbars[-length(nbars)]))
  postdash   <- 30 - nbars - predash
  rep_bar    <- function(n, chr) paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(predash, chr = "\U00B7")
  bar_mid    <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(postdash, chr = "\U00B7")
  bar_text   <- str_pad(" % of columns ", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(%)", width = 4, pad = " ", side = "left")
  num_text   <- str_pad("(#)", width = 1, pad = " ", side = "left")
  cat(paste("     ", bar_text, " "), perc_text, 
      num_text, "type \n", "  ",
       paste(rep("-", 56), collapse = ""), "\n")
  for(i in 1:length(sdf$prop)){
    cat(paste("    \U2022 "))
    cat(silver(bar_left[i]))
    cat(red(bar_mid[i]))
    cat(silver(bar_right[i]))
    cat(paste(" \U2022"))
    perc <- str_pad(paste(round(sdf$prop[i] * 100, 0), "% ", sep = ""), 
                    width = 4, side = "left", pad = " ")
    cat(green(paste("  ", perc, "(", sdf$n[i], ") ", text, sdf$type[i], sep = "")))
    cat("\n")
  }
}


