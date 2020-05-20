#Part 4: Create a function that ranks hospitals on their best mortality rates within their 
#respective states for a given outcome (Ex. Mortality cause)

rankall <- function(outcome, num = "best") {
        
        #read the desired data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #Check that the outcome input is valid
        if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        #identify which column needs to be read from the given outcome value
        if (outcome == "heart attack") {
                outcome_column <- 11
        }
        else if (outcome == "heart failure") {
                outcome_column <- 17
        }
        else {
                outcome_column <- 23
        }
        
        #if num is greater than the number of hospitals in the desired state, return NA
        if (is.numeric(num) == TRUE) {
                if (length(data[,2]) < num) {
                        return(NA)
                }
        }
        
        #Pull out NA values
        data[, outcome_column] <- suppressWarnings(as.numeric(levels(data[, outcome_column])[data[, outcome_column]]))
        data[, 2] <- as.character(data[, 2])
        
        
        #create a list of states and initialize a character vector to hold the req. hospital names
        state <- levels(factor(data[, 7]))
        hospital <- vector(mode = "character")
        
        for (i in seq(state)) {
                hospital[i] <- suppressWarnings(rankhospital(state[i], outcome, num))
        }
        data.frame(hospital, state)
        
}