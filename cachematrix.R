## Matrix inversion can be a computationally intensive operation. Where possible, it can be helpful to cache
## the inverse of a matrix, rather than computing it repeatedly. These functions employ the <<- operator,
## which can be used to assign a value to an object in an environment that is different from the current
## environment, enabling caching of a previously computed inverse matrix, which can be retrieved in lieu of
## recomputing.

## The makeCacheMatrix function creates a special "matrix" object which sets the values of a matrix,
## gets the values of this matrix, sets the values of the inverse matrix (produced via solve()), and
## gets the values of this inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The cacheSolve function does the calculation to produce the inverse matrix using solve(), but only
## after checking to see if the inverse matrix has already been calculated. If it has, it gets the
## values for the matrix from the cache, and skips re-doing the calculation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}