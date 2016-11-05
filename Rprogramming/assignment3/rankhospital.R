rankhospital <- function(state, outcome, num){
        
        # place outcome data into 'input' variable.  Set all column classes to charcter type
        input <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
        
        #Variable used to check if num is set to 'best', or 'worst'
        numcheck <- FALSE
        
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
                print(state)
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
        
        # set 'num' variable which specifies rank
        if (num == 'best'){
          num <- 1
          numcheck <- TRUE
        }
        if (num == 'worst'){
          numcheck <- TRUE
          num <- nrow(z)
        }
        if (numcheck == FALSE) {
            suppressWarnings(
            if (num <= nrow(z) & num != 0 & !is.na(as.numeric(num))){
              num <- num
            } else {
                return(NA)
            })
        }

        
        # Look up all hospitals in a state with a valid outcome
        # Order the data.frame by rates of outcome
        z <- data.frame(z$Hospital.Name, z[index])
        z <- z[order(z[,2], z[,1]),]
        
        # add rank column
        z$rank <- c(1:length(z$z.Hospital.Name))
        
        # convert Hospital.Name to character vector
        z$z.Hospital.Name <- as.character(z$z.Hospital.Name)
        
        # select hospital name that corresponds to rank input by user
        answer <- z$z.Hospital.Name[which(z$rank == num)]
        print(answer)
}