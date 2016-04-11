## File         : rankAll.R
## System       : Assignment 3 - RProgramming Course (Coursera) - Part 3
## Date         : 11/04/2016
## Author       : Ala Halaseh

##data: the data frame containing the hospital data
##state: the 2-character abbreviated name of a state and an outcome name.
##outcome: "heart attack", "heart failure", or "pneumonia"
##returns: hospital name in Hospital.Name
getRankPerState <- function(data, state, outcome, num=1){
  
  ##check if state exists
  if(is.na(match(state, data[,"State"])))
    stop("invalid state")
  ##Check if outcome exists
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia"))))
    stop("invalid outcome")
  
  ##convert state to factor
  data$State <- as.factor(data$State)
  
  ##split data to get selected State data
  stateData <- split(data, data$State)[[state]]
  
  ##get col index of the outcome
  index <- getOutcomeIndex(outcome)
  
  ##change variable to numeric from character
  stateData[,index] <- suppressWarnings(as.numeric(stateData[,index]))
  
  ##Order state data by factor and hospital Name
  stateData <- stateData[order(stateData[,index], stateData$Hospital.Name),]
  
  if(num == "worst")
    num = sum(!is.na(stateData[,index]))
  else if (num == "best")
    num = 1
  
  ##get the hospital name
  stateData[num,"Hospital.Name"]
}

getOutcomeIndex <- function (outcome){
  if (outcome == "heart attack")
    11
  else if (outcome == "heart failure")
    17
  else if (outcome == "pneumonia")
    23
}

##outcome: the area we are examining can: "heart attack", "heart failure" or "pneumonia"
##num: the rank we want to get, can accept best/worst
##returns data frame that has the hospitals in the the rank passed in all the states.
rankAll <- function(outcome, num="best") {
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  ## convert states to factor
  data$State <- as.factor(data$State)
  
  ## create empty dataframe
  df <- data.frame(State=character(),Hospital=character())
  
  ## Loop on all states
  for (x in levels(data$State)){
    
    df <- rbind.data.frame(df, data.frame(State=x, Hospital=getRankPerState(data, x, outcome, num)), stringsAsFactors=FALSE)
    ##df <- rbind (df, c(as.character(x), "2"), )
    
  }
  df
  
}