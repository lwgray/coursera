best <- function(state, outcome){
        
        # place outcome data into 'input' variable.  Set all column classes to charcter type
        input <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  
        # assign column index based on outcome
        index <- NULL
        if (outcome == 'heart attack') { index <- 11}
        if (outcome == 'heart failure') { index <- 17}
        if (outcome == 'pneumonia') { index <- 23}
        
        
        # check that state and outcome are valid
        # If state is valid then pull out all data with designated state
        if (state %in% input$State) {
                y <- input[which(input$State == state),]
        } else {
                stop('invalid state')
        }
        
        # If index is not null throw error
        # otherwise coerce outcome column to be numeric
        # and extract only rows that hae values that are not NAs
        if (!is.null(index)){
                suppressWarnings(
                y[index] <- lapply(y[index], function(x) { if (is.character(x)){ as.numeric(x)}})
                )
                # z <- y[which(y[index] > 0 & !is.na(y[index])),]
                z <- y[which(!is.na(y[index])),]
        } else{
                stop('invalid outcome')
        }
        
        
        # Look up all hospitals in a state with a valid outcome
        # select hospitals with min value then return a hospital name
        # based on which name is first in alphabetical order
        min(matrix(z$Hospital.Name[which(z[index] == min(z[index], na.rm=TRUE))]))
        
        
        
}