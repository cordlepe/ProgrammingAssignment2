## The pair of functions below will cache the inverse of a matrix
## Because calculating the inverse of a matrix is a costly computation,
## there may be some benefit to caching the inverse rather than computing it repeatedly.
## Assume the matrix supplied to these functions is always invertible

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes inverse of the special "matrix" that gets returned by makeCacheMatrix above
## if the inverse was already calculated (and the matrix hasn't changed)
## then this function retrieves the inverse from the cache
## the solve function returns the inverse of a square matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}