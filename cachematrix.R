## The first function provides the inverse of a desired matrix and the second one 
## computes the inverse if it is not found in the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set0 <- function(y) {
                x <<- y
                m <<- NULL
        }
        get0 <- function() x
        set1 <- function(inverted_matrix) m <<- inverted_matrix
        get1 <- function() m
        list(set0 = set0, get0 = get0,
             set1 = set1,
             get1 = get1)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) { 
        mat <- x$get1(mat)
        if (!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get0()
        mat <- solve(data, ...)
        x$set1(mat)
        mat
