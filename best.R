# R-Programming Assignment 3 - Part 2
# Finding the best hospital using the "outcome-of-care-measures.csv" file
# Create a function that returns the lowest 30-day mortality for:
#   a. Heart attack (column 11)
#   b. heart failure (column 17)
#   c. pneumonia (column 23)
# for a given state (column 7)
# 
## Remove the warnings - then return the option to original value
oldw <- getOption("warn")
options(warn = -1)

best <- function(state, illness) {

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
        outcome_sort <- outcome[with(outcome, order(state, heart_attack, hospital_name)),] 
        
}   else if (illness == "heart_failure") {
        outcome_sort <- outcome[with(outcome, order(state, heart_failure, hospital_name)),] 
        
}    else if (illness == "pneumonia") {
        outcome_sort <- outcome[with(outcome, order(state, pneumonia, hospital_name)),] 
            
}    else {
        Print("Try again with a different illness!") 
}
## Use head to take the top/minimum value for each state: 
outcome_min <- aggregate(outcome_sort, list(outcome_sort$state), FUN = head, 1)


state_row <- which(outcome_min$state == state)
## print(outcome_sort)
## print(state_row)
## print(outcome_min)
print(outcome_min[state_row, 2])


options(warn = oldw)
}


    
