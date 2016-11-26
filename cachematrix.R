# Week 3 Assignment 
# Author : AnnaMala
# 
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinv <- function(inverse) inv <<- inverse

    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: 
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # Check to see if the matrix is cached
    if (!is.null(inv)) {
        message("Cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}