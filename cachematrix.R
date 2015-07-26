makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## clears
    if(!identical(length(x[1,]), length(x[, 1]))) {   ## checks for square matrix
        message("not a square matrix")   ## notifies user if not square matrix
    }
    get <- function() x   ## 1. get function stores original matrix
    setinv <- function(solve) m <<- solve   ## 2. setinv creates inverse
    getinv <- function() m   ## 3. getinv stores inverse
    list(get = get,    ## creates a list of the three functions for recall
         setinv = setinv, getinv = getinv)  
}

cacheSolve <- function(x, ...) {
    m <- x$getinv()   ## assigns m to cached inv matrix
    if(!is.null(m)) {  ## checks if m has a value
        message("getting cached data")   ## prints message to notify user
        return(m)  ## returns cache value
    }
    data <- x$get()   ## recalls original matrix
    m <- solve(data, ...)  ## inverses matrix
    x$setinv(m)   ## sets inverse matrix value to cache
    m   ## returns inverse
}