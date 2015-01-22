## Put comments here that give an overall description of what your
## functions do

## The functions in this file are for creating a matrix object
## that can cache its own inverse, and then solving/caching/getting
## the inverse of the matrix.

## Write a short comment describing this function

## When supplied a square invertible matrix as the argument,
## this function returns a list of functions get(), set(),
## getinverse(), and setinverse() used by cacheSolve to
## cache the inverse of the matrix for multiple uses.
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set = function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## When supplied a makeCacheMatrix object as
## the first argument, this function will return
## the cached inverse of the matrix. If the
## inverse has not been cached, it will solve
## the inverse and cache it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
