## Provides an makeCacheMatrix function that is able to cache the solution of a matrix
## via the lexical scope of a inner function and the cacheSolve func.

## Makes a special form of a matrix that is able to cache the computation of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Caches and returns the inverse of a matrix. 'x' is a list returned from makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
