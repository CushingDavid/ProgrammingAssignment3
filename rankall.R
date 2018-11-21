## R programming Assignment 3 -Part 4
##
## Write a function called rankall that takes two arguments: 
## an outcome name (outcome) and a hospital ranking (num).
## The function reads the outcome-of-care-measures.csv 
## It returns a 2-column data frame containing 
## 1. the hospital in each state that has the ranking specified in num. 
## For example the function call
## rankall("heart attack", "best") would return a data frame containing 
## the names of the hospitals that are the best in their respective states 
## or 30-day heart attack death rates. 
## The function should return a value for every state (some may be NA). 
## The first column in the data frame is named hospital, which contains the hospital name
## and the second column is named state, which contains the 2-character abbreviation for
## the state name. 
## Hospitals that do not have data on a particular outcome should be excluded from the 
## set of hospitals when deciding the rankings.



rankall <- function(illness , num = "best") {
    if (num == "best") num <- 1
    
    ## Read outcome data
    # Read in the data from the .csv file:
    outcomef <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Reduce the data frame to the relevant data:
    outcome <- as.data.frame(cbind(outcomef[ ,2], outcomef[ ,7], outcomef[ ,11], outcomef[ ,17], outcomef[ ,23]))
    
    # Name the columns of outcome with the relevant titles: 
    colnames(outcome) <- c("hospital_name", "state", "heart_attack", "heart_failure", "pneumonia")
    
    # Convert factors to numeric values:
    outcome$heart_attack <- as.numeric(as.character(outcome$heart_attack))
    outcome$heart_failure <- as.numeric(as.character(outcome$heart_failure))
    outcome$pneumonia <- as.numeric(as.character( outcome$pneumonia ))
    
    
    if (illness == "heart_attack") { 
        ## order the outcome dataset by state - heart_attack - hospital_name   
        outcome_sort <- outcome[with(outcome, order(state, heart_attack, hospital_name, na.last = NA)),]
        outcome_sort$rank <- NA
        outcome_sort$rank=unlist(with(outcome_sort,tapply(heart_attack,state,order)))
        
    }   else if (illness == "heart_failure") {
        outcome_sort <- outcome[with(outcome, order(state, heart_failure, hospital_name, na.last = NA)),]
        outcome_sort$rank <- NA
        outcome_sort$rank=unlist(with(outcome_sort,tapply(heart_failure,state,order)))
        
    }    else if (illness == "pneumonia") {
        outcome_sort <- outcome[with(outcome, order(state, pneumonia, hospital_name, na.last = NA)),]
        outcome_sort$rank <- NA
        outcome_sort$rank=unlist(with(outcome_sort,tapply(pneumonia,state,order)))
        
    }    else {
        Print("Try again with a different illness!") 
    } 
    
    ## Return hospital name in that state with the given rank. 
    ## state_row <- which(outcome_sort$state == state, outcome_sort$rank == num)
    
    rank_row <- subset(outcome_sort, outcome_sort$rank == num)
    hospital_list <- rank_row [ , c("hospital_name", "state")]
    print(rank_row)
    ## 30 day death rate
    ## print(outcome_min[state_row, 2])
}
