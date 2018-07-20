
df.complexity <- function(x, scaleMin, scaleMax, width) {
  x <- x %>% purrr::keep(is.numeric)
  df.comp <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  df.comp <- as.data.frame(df.comp)
  colnames(df.comp) <- colnames(x)
  for(i in 1:ncol(x)){
      df.comp[,i] <- complexity(x[,i], scaleMin, scaleMax, width)
  }
df.comp$time <- c(1:nrow(df.comp))
return(df.comp)
}

