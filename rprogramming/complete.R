complete <- function(directory, ids) {
        "Returns number of complete rows"
        a <- c() ## Contains IDs
        b <- c() ## Contains NumRows
        for (x in ids) {
                file_list <- get_data_file(x)
                files <- file_paths(directory, file_list)
                num <- count_rows(files)
                a <- c(a, x)
                b <- c(b, num)
        }
        
        answer <- data.frame(id = a, nobs = b)
        answer
}


count_rows <- function(ids) {
        "Collect number of complete rows"
                
        total <- c()
        nitrate <- c()
        sulfate <- c()
        air <- read.csv(ids)
        
        # count number with values in nitrate rows
        for (x in air$nitrate) {
                if (is.na(x)) {
                        next()
                } else{
                        nitrate <- c(nitrate, 1)
                }
        }
        nitrate <- sum(nitrate)
        
        # count number with values in sulfate rows
        for (x in air$sulfate) {
                if (is.na(x)) {
                        next()
                } else{
                        sulfate <- c(sulfate, 1)
                }
        }
        sulfate <- sum(sulfate)
        
        # The min value represents the number of rows that have at least one value
        # in either nitrate of sulfate rows
        answer <- min(nitrate, sulfate)
}


file_paths <- function(directory, ids){
        "Return a vector of full paths for each directory and file"
        paths <- c()
        for (x in ids) {
                x <- c(directory, '/', x)
                x <- paste(x, collapse = '')
                paths <- c(paths, x)
        }
        paths
}


get_data_file <- function(id) {
        "Return a vector of file names for requested files"
        myids <- c()
        for (i in id) {
                i <- as.character(i)
                if (nchar(i) == 2) {
                        i <- c('0', i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
                else if (nchar(i) == 1) {
                        i <- c('00', i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
                else {
                        i <- c(i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
        }
        myids
}
