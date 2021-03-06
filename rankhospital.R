# Take the state name (state), outcome (outcome), and ranking of the hospital for that outcome (num).
# From out-of-care measures.csv file
# Returns a vector with the name of the hospital - ranking by 'num'
# 



rankhospital <- function(state, illness , num = "best") {
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
    outcome$state <- as.character(outcome$state, length(2))
    state <- as.character(state, length(2))
    
    ## Check that state and outcome are valid:
    
    state_list <- unique(outcome$state)
    as.vector(state_list)
    if(!state %in% (state_list)){
        
        stop("invalid state code!!!")
        
    }
    
    
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
    
    outcome_sort$state <- as.character(outcome_sort$state, length(2))
    
    ## Return hospital name in that state with the given rank. 
    
    rank_row <- outcome_sort[ which(outcome_sort$state == state & outcome_sort$rank == num), ]
    
    print(rank_row)
    
    ## 30 day death rate  
    print(rank_row[, 1])
}
