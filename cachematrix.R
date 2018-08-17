## This collection of functions creates a matrix object (with functions for accessing and
## mutating). It then calculates the inverse of the matrix, or if the inverse has already
## been calculated, the inverse is retrieved from the cache.

## makeCacheMatrix creates special matrix object that can cache its inverse.
## Returns list of functions for accessing and mutating matrix object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <-- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve computes the inverse of the special 'matrix' object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.
## Returns inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message('Getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}