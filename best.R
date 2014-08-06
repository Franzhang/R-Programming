best <- function(state, outcome) {
  ## Read outcome data
  RawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  StateNames <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                  "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS",
                  "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",
                  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY",
                  "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  if(!(state %in% StateNames)) stop("invalid state") 
  if( match(outcome,c("heart attack", "heart failure", "pneumonia" ), nomatch = -1) == -1 ) 
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  if (outcome == "heart attack"){
    sub <- RawData[RawData[,"State"] == state, c(2,11)]
    sub[,2] <- suppressWarnings(as.numeric(sub[,2]))
    sub <- sub[order(sub[,2]),]
    return(sub[1,1])
  }
  if (outcome == "heart failure"){
    sub <- RawData[RawData[,"State"] == state, c(2,17)]
    sub[,2] <- suppressWarnings(as.numeric(sub[,2]))
    sub <- sub[order(sub[,2]),]
    return(sub[1,1])
  }
  if (outcome == "pneumonia"){
    sub <- RawData[RawData[,"State"] == state, c(2,23)]
    sub[,2] <- suppressWarnings(as.numeric(sub[,2]))
    sub <- sub[order(sub[,2]),]
    return(sub[1,1])
  }
}