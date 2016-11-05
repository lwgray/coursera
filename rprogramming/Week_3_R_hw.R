MakeCacheMatrix <- function(x = matrix()){
        mx <- NULL
        set <- function(y){
                x <<- y
                mx <<- NULL
        }
        get <- function() x
        setInv <- function(solve) mx <<- solve
        getInv <- function() mx
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) {
        mx <- x$getInv()
        if(!is.null(mx)){
                message("Getting cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$setInv(mx)
        mx
}


makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

test = function(mat){
        ## @mat: an invertible matrix
        
        temp <- MakeCacheMatrix(mat)
        
        start.time <- Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time <- Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time <- Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time <- Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
