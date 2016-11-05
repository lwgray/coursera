best <- function(state, outcome){
        # place outcome data into 'input' variable.  Set all column classes to charcter type
        input <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
        conditions <- c('heart attack', 'pneumonia', 'heart failure')
        # check that state and outcome are valid
        # Verify input by user is correct
        if (state %in% outcome$State) {
                
        } else {
                stop('invalid state')
        }
        
        if (outcome %in% conditions){
                
        } else{
                stop('invalid outcome')
        }
        
        
        # Look up all hospitals in a state with a valid outcome
        
}