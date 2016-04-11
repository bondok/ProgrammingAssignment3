## File         : rankHospital.R
## System       : Assignment 3 - RProgramming Course (Coursera) - Part 2
## Date         : 11/04/2016
## Author       : Ala Halaseh

##state: the 2-character abbreviated name of a state and an outcome name.
##outcome: "heart attack", "heart failure", or "pneumonia"
##returns: hospital name in Hospital.Name
rankHospital <- function(state, outcome, num=1){

  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
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
  stateData[,index] <- as.numeric(stateData[,index])

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