rankall <- function(outcome, num = "best") {
  ## Read outcome data
  RawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  StateNames <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                  "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS",
                  "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",
                  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY",
                  "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  if( match(outcome,c("heart attack", "heart failure", "pneumonia" ), nomatch = -1) == -1 ) 
    stop("invalid outcome")

  if (outcome == "heart attack"){
    clean <- RawData[, c(2,7,11)]
    ## For each state, find the hospital of the given rank
    StateRank <- data.frame()
    if (num == "best") {
      for(i in StateNames){
      Temp <- clean[clean[,"State"] == i, ]
      Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
      Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
      StateRank <- rbind(StateRank, Temp[1,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
    
    if (num == "worst") {
      for(i in StateNames){
        Temp <- clean[clean[,"State"] == i, ]
        Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
        Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
        StateRank <- rbind(StateRank, tail(Temp, n = 1)[1,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
    
    if (is.numeric(num)) {
      for(i in StateNames){
        Temp <- clean[clean[,"State"] == i, ]
        Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
        Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
        StateRank <- rbind(StateRank, Temp[num,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
  }
    
    if (outcome == "heart failure"){
      clean <- RawData[, c(2,7,17)]
      StateRank <- data.frame()
      if (num == "best") {
        for(i in StateNames){
          Temp <- clean[clean[,"State"] == i, ]
          Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
          Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
          StateRank <- rbind(StateRank, Temp[1,1:2])
        }
        colnames(StateRank) <- c("hospital", "state")
        StateRank$state <- rownames(StateRank) <- StateNames
        return(StateRank)
      }
      
      if (num == "worst") {
        for(i in StateNames){
          Temp <- clean[clean[,"State"] == i, ]
          Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
          Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
          StateRank <- rbind(StateRank, tail(Temp, n = 1)[1,1:2])
        }
        colnames(StateRank) <- c("hospital", "state")
        StateRank$state <- rownames(StateRank) <- StateNames
        return(StateRank)
      }
      
      if (is.numeric(num)) {
        for(i in StateNames){
          Temp <- clean[clean[,"State"] == i, ]
          Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
          Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
          StateRank <- rbind(StateRank, Temp[num,1:2])
        }
        colnames(StateRank) <- c("hospital", "state")
        StateRank$state <- rownames(StateRank) <- StateNames
        return(StateRank)
      }
    }
  if (outcome == "pneumonia"){
    clean <- RawData[, c(2,7,23)]
    StateRank <- data.frame()
    if (num == "best") {
      for(i in StateNames){
        Temp <- clean[clean[,"State"] == i, ]
        Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
        Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
        StateRank <- rbind(StateRank, Temp[1,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
    
    if (num == "worst") {
      for(i in StateNames){
        Temp <- clean[clean[,"State"] == i, ]
        Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
        Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
        StateRank <- rbind(StateRank, tail(Temp, n = 1)[1,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
    
    if (is.numeric(num)) {
      for(i in StateNames){
        Temp <- clean[clean[,"State"] == i, ]
        Temp[,3] <- suppressWarnings(as.numeric(Temp[,3]))
        Temp <- Temp[order(Temp[,3], Temp[,1], na.last = NA), ]
        StateRank <- rbind(StateRank, Temp[num,1:2])
      }
      colnames(StateRank) <- c("hospital", "state")
      StateRank$state <- rownames(StateRank) <- StateNames
      return(StateRank)
    }
  }
}
