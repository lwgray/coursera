pollutantmean <- function(directory, pollutant, id = 1:332) {
        "Obtain mean of all polluant values from all data provided"
        ids <- get_data_file(id)
        files <- file_paths(directory, ids)
        data <- get_pollunant_data(files, pollutant)
        # options(scipen = 3)
        answer <- signif(get_mean(data), 4)
        # answer <- get_mean(data)
        answer
}

get_mean <- function(data) {
        "Return Mean of all collect polluant values"
        find <- mean(data, na.rm=TRUE)
}

get_pollunant_data <- function(data, pollutant) {
        "Retrun a vector of all pollunat values from the data provided"
        mypol <- c()
        for (i in data) {
                air <- read.csv(i)
                b <- eval(parse(text=paste('air$', pollutant, collapse='')))
                mypol <- c(b, mypol)
        }
        mypol
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


