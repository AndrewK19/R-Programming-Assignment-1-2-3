#Part 3: Create a function that outputs a hospital name for the required state, mortality outcome, and 
#inputted rank (ex.)


rankhospital <- function (state, outcome, num = "best") {
        
        #read the desired data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #Check that the outcome and state are valid
        states <- data[,7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% states) == FALSE) {
                stop(print("invalid state"))
        }
        else if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        #get the subset of the data with the desired state (use only data for defined state)
        new_data <- subset(data, State == state)
        
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
        
        #read the new subsetted data based on state and outcome value, and pull out NA values
        new_data[,outcome_column] <- suppressWarnings(as.numeric(new_data[,outcome_column]))
        bad <- is.na(new_data[, outcome_column])
        desired_data <- new_data[!bad,]
        
        #arrange the modified data frame in ascending order based on the outcome value
        #names() for columns, row.names() for row names
        outcome_col_name <- names(desired_data)[outcome_column] 
        hospital_col_name <- names(desired_data)[2]
        index <- with(desired_data, order(desired_data[outcome_col_name], desired_data[hospital_col_name]))
        ordered_desired_data <- desired_data[index, ]
        
        #if num is either "best" or "worst", then return numerical rank based on num input
        if (is.character(num) == TRUE) {
                if(num == "best"){
                        num <- 1
                }
                else if (num == "worst") {
                        num <- length(ordered_desired_data[, outcome_column])
                }
        }
        
        #return the hospital name with the outcome ranking of the input num
        ordered_desired_data[num,2]
}