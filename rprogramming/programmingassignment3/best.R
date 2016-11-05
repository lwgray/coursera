best <- function(state, outcome){
        # place outcome data into 'input' variable.  Set all column classes to charcter type
        input <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
        index <- NULL
        # check that state and outcome are valid
        # Verify input by user is correct
        # conditions <- c('heart attack', 'pneumonia', 'heart failure')
        if (outcome == 'heart attack') { index <- 11}
        if (outcome == 'heart failure') { index <- 17}
        if (outcome == 'pneumonia') { index <- 23}
        
        
        
        if (state %in% input$State) {
                y <- input[which(input$State == state),]
        } else {
                stop('invalid state')
        }
        
        if (!is.null(index)){
                z <- y[which(y[index] > 0),]
        } else{
                stop('invalid outcome')
        }
        
        
        # Look up all hospitals in a state with a valid outcome
        # x <- outcome[which(outcome$State == state & outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack > 0),]
        # hospitals <- input$State
        z$Hospital.Name[which(z[index] == min(z[index]))]
        
}