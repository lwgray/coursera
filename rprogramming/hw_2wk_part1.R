pollutantmean <- function(directory, pollutant, id = 1:332) {
        ids <- get_data_file(directory, id)
        data <- get_pollunant_data(ids, pollutant)
        answer <- get_mean(data)
        answer
}

get_mean <- function(data) {
        find <- mean(data, na.rm=TRUE)
}

get_pollunant_data <- function(data, pollutant) {
        mypol <- c()
        for (i in data) {
                air <- read.csv(i)
                b <- eval(parse(text=paste('air$', pollutant, collapse='')))
                mypol <- c(b, mypol)
        }
        mypol
}


get_data_file <- function(directory='', id, sep='/') {
        myids <- c()
        for (i in id) {
                i <- as.character(i)
                if (nchar(i) == 2) {
                        i <- c(directory, sep, '0', i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
                else if (nchar(i) == 1) {
                        i <- c(directory, sep, '00', i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
                else {
                        i <- c(directory, sep, i, '.csv')
                        i <- paste(i, collapse = '')
                        ## print(i)
                        myids <- c(myids, i)
                }
        }
        myids
}

