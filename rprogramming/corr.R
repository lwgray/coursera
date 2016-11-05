# air <- read.csv('specdata/001.csv')
# a <- air[, 2:3]


# correlations <- c()


corr <- function(directory, threshold=1){
        correlations <- c()
        dir_name <- directory # c('/', directory)
        # combine_dir_name <- paste(dir_name, collapse = )
        ls_dir_files <- list.files(dir_name) # paste(dir_name, collapse = ''))
        ids <- length(ls_dir_files)
        # file_names <- c(271:280) # Used to test various segments of files in dir
        for (x in 1:ids) {
                file_list <- get_data_file(x)
                files <- file_paths(directory, file_list)
                cor_list <- cal_cor(threshold, files)
                # if(is.data.frame(cor_list) && nrow(df) == 0){
                # 
                # }
                
                # # works
                # if (is.null(cor_list)){
                #         next
                # }
                correlations <- c(correlations, cor_list)
        }
        
        if (is.null(correlations)){
                correlations <- rep(0,0)
                return(correlations)
        } else{
                options(digits=4)
                correlations
        }
        options(digits=4)
        correlations
}


cal_cor <- function(threshold, fn){
        correlations <- c()
        air <- read.csv(fn)
        a <- air[, 2:3]
        sulfate <- c()
        nitrate <- c()
        for(row_value in 1:nrow(a)){
                b <- !is.na(a['sulfate'][row_value,])
                c <- !is.na(a['nitrate'][row_value,])
                d <- a['sulfate'][row_value,]
                e <- a['nitrate'][row_value,]
                if (b && c){
                        sulfate <- c(sulfate, d)
                        nitrate <- c(nitrate, e)
                } 
        }
        
        df <- data.frame(sulfate, nitrate)

        rows <- nrow(df)
        columns <- ncol(df)
        
        
        if(is.data.frame(df) && nrow(df) == 0){
                # happy <- nrow(a)
                # sad <- ncol(a)
                # m <- matrix(0, ncol = sad, nrow = happy)
                # n <- data.frame(m)
                # return(df)
                # 
                return(NULL)
        }
        
        answer <- cor(df$sulfate, df$nitrate)
        answer <- round(answer, digits=5)
       
        if (rows > threshold){
                 correlations <- c(correlations, answer)
        }
        
        correlations
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