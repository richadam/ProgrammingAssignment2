## Put comments here that give an overall description of what your
## functions do

## create an inverse matrix and get, set, getinverse, setinverse functions

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
	        x <<- y
	        # set to NULL to clear the cached state
	        inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(mat) inverse <<- mat  # this is an evil command line function
	getinverse <- function() inverse
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## return the inverse of matrix, use the cached inverse if it exists

cacheSolve <- function(x, ...) {
	## check for a cached inverse matrix and return that if it exists
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached matrix.")
        return(inverse)
    }
    # otherwise compute the inverse, set, and return it
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
