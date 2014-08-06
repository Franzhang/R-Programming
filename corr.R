corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE)
  id <- 1:332
  dat <- data.frame()
  cor_vec <- c()
  nobs <-c()
  x <- integer()
  y <- numeric()
  for(i in id){
    dat <- read.csv(files_list[i])
    x <- sum(complete.cases(dat))
    nobs <- c(nobs, x)
    y <- cor(dat$nitrate, dat$sulfate, use = "na.or.complete")
    cor_vec <- c(cor_vec, y)
  }
  
  outcome <- c()
  for(i in id){
    if(nobs[i] > threshold) {
      z <- cor_vec[i]
      outcome <- c(outcome, z)
    }
  }
  return(outcome)
}