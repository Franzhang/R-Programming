complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  nobs <- integer()
  for (i in id){
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  j <- 1
  for (i in id){
    dat_subset <- dat[dat[, "ID"] == i,] 
    nobs[j] <- sum(complete.cases(dat_subset))
    j <- j + 1

  }
  
  data.frame(id, nobs)
}

