## This file it's part of the Programming Assignment 2

## The makeCacheMatrix take a matrix as the argument and create a object that contains some methods,
## that will be used by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes the object created by makeCacheMatrix and tests if the matrix inverse of this
## matrix already has been calculated. If it so, show it. If not, calculate it, using the solve function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
