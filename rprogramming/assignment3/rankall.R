rankall <- function(outcome, num){
        
        # Read outcome data
        # place outcome data into 'input' variable.  Set all column classes to charcter type
        input <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
        
        # create vector containing unique states
        states <- as.character(unique(input$State))
        
        #Variable used to check if num is set to 'best', or 'worst'
        numcheck <- FALSE
        
        original.num <- num
        
        # assign column index based on outcome
        index <- NULL
        if (outcome == 'heart attack') { index <- 11}
        if (outcome == 'heart failure') { index <- 17}
        if (outcome == 'pneumonia') { index <- 23}
        
        # used as a global variable to store final data.frame 
        # that will be returned to the user
        final <- data.frame(hospital = character(0), state = character(0))
        
        # loop throught all states and gather rank according to input by user
        for (x in states) {
                xnum <- original.num
                # check that state and outcome are valid
                # If state is valid then pull out all data with designated state
                if (x %in% states) {
                        y <- input[which(input$State == x),]
                } else {
                        stop('invalid state')
                }
                
                # If index is not null throw error
                # otherwise coerce outcome column to be numeric
                # and extract only rows that have values that are not NAs
                if (!is.null(index)){
                        suppressWarnings(
                                y[index] <- lapply(y[index], function(x) { 
                                        if (is.character(x)){ as.numeric(x)}
                                        })
                        )
                        # z <- y[which(y[index] > 0 & !is.na(y[index])),]
                        z <- y[which(!is.na(y[index])),]
                } else{
                        stop('invalid outcome')
                }
                
                # set 'num' variable which specifies rank
                if (xnum == 'best'){
                        xnum <- 1
                        numcheck <- TRUE
                }
                if (xnum == 'worst'){
                        numcheck <- TRUE
                        xnum <- nrow(z)
                }
                if (numcheck == FALSE) {
                        suppressWarnings(
                                if (xnum <= nrow(z) & xnum != 0 & !is.na(as.numeric(xnum))){
                                        xnum <- xnum
                                } else {
                                        xnum <- NA
                                }
                        )
                }
                
                
                # Look up all hospitals in a state with a valid outcome
                # Order the data.frame by rates of outcome
                z <- data.frame(z$Hospital.Name, z[index], z$State)
                z <- z[order(z[,2], z[,1]),]
                
                # add rank column
                z$rank <- c(1:length(z$z.Hospital.Name))
                
                # convert Hospital.Name to character vector
                z$z.Hospital.Name <- as.character(z$z.Hospital.Name)
                
                # select hospital name that corresponds to rank input by user
                # create data.frame containing hospital name and state
                # if num is NA then add NA for Hospital
                if (is.na(xnum)){
                        z <- data.frame(hospital = NA, state=x)
                } else {
                
                z <- data.frame(hospital = z$z.Hospital.Name[which(z$rank == xnum)], state = z$z.State[which(z$rank == xnum)])
                }
                
                # concatenate hospital to global data.frame
                final <- rbind(final, z)

        }
        final$state <- as.character(final$state)
        final <- final[order(final$state),]
        final
        
}