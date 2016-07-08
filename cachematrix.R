## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following is a pair of functions that cache the inverse of a matrix.
##
## Assumption: the matrix supplied to makeCacheMatrix is always invertible.
##
## Example for using these two fuctions:
##
## testM <- matrix(1:4, 2, 2)
## tempM <- makeCacheMatrix(testM)
##
## First time, the inverse is calculated:
## invTestM <- cacheSolve(tempM)
##
## If called again, the cached inverse is returned: 
## (see message "getting cached data" displayed)
## invTestM <- cacheSolve(tempM)
##
## To verify the inverse is correct, 
## the following should return I2 (2x2 identity matrix):
## invTestM %*% testM

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseM) inv <<- inverseM
    getinverse <- function() inv
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    # return cached inverse if it's available
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # calculate inverse of the matrix
    data <- x$get()
    inv <- solve(data, ...)
    
    # cache the newly calculated inverse
    x$setinverse(inv)
    inv
}
