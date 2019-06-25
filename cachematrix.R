## Coursera: R Programming: Week 3: Programming Assignment 2: Lexical Scoping

## makeCacheMatrix takes in an invertable matrix and defines functions that can be used to set and recover
## inverse of the matrix when called from different environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve returns the inverse of matrix returned by makeCacheMatrix, and if the inverse has not been stored,
## calculates the inverse and stores it to the matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
