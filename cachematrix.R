## The function makeCacheMatrix will calculate the inverse of a square matrix, then
## caching the result in cache for later retrieving, if required, and assuming that the
## original matrix we passed has not changed.
## Sample matrix:
## x <- matrix(1:4, nrow = 2, ncol = 2)

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The function below will retrieve the cached value of the inverse of the original
## matrix x (which means, such matrix has not changed) so we don't need to rerun
## the calculation of the inverse using solve().

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
