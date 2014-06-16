# usage:
# > x <- matrix(rnorm(9), nrow = 3)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create a matrix that can cache it's inverse
# > cacheSolve(cx)                            // Return the inverse of matrix by calculating first time and cache after
# > cacheSolve(cx)                            // Second call will return inverse cached value
#                                            


# makeCacheMatrix: return a matrix with list  of functions to:
# - Set the value of the matrix
# - Get the value of the matrix
# - Set the value of the inverse
# - Get the value of the inverse


makeCacheMatrix <- function(m = matrix()) {
        # inv will store the cached inverse matrix
        invOfM <- NULL
        
        # Set value  for the matrix. set is not reserved word in R !!
        set <- function(y) {
                m <<- y
                invOfM <<- NULL
        }
        # Get value of the matrix
        get <- function() m
        
        # Set value of the inverse of matrix
        setinv <- function(inverse) invOfM <<- inverse
        # Get Value of the inverse
        getinv <- function() invOfM
        
        # Return the matrix with  functions above
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix using cache if already calculated else calculate and store in cache for next call
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        invm <- x$getinv()
        
        # If the inverse is already calculated, return from cache
        if (!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
        # calculate inverse as not calculated before using solve function
        data <- x$get()
        invm <- solve(data, ...)
        
        # Cache the inverse of matrix
        x$setinv(invm)
        
        # Return inverse
        invm
}