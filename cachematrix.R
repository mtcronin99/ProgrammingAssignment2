## A pair of functions that cache the inverse of a matrix to avoid this costly operation.

## The function makeCacheMatrix creates a special "matrix", that is really a list containing
##  4 functions: set, get, setinverse and getinverse along with environment variables
##  to store the actual matrix and its cached inverse.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
	inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
	getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated, it will
## retrieved from the cache.  Extra parameters (beyond the special "matrix" called 'x'
## will be only be used in the initial computation of the inverse.  Subsequent calls
## will not check if these parameters are different.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
	    message("getting cached data")
	    return(inv)
	}
	mat = x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv
}
