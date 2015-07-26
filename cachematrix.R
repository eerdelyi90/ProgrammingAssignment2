## Put comments here that give an overall description of what your
## functions do
## creates a class makeCacheMatrix
## (essentially a list of functions necessary to cache a matrix and the appropriate getters and setters) and constructs it
## Write a short comment describing this function
##
makeCacheMatrix <- function(x = matrix()) {
	## init the inverse variale with null
	inverse <- NULL
	## set the value
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## get the value of the matrix
    get <- function() x

    # set inverse of matrix
    setinverse <- function(inv) inverse <<- inverse

    # get inverse of matrix
    getinverse <- function() inverse

    # return a list of all the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## solves the inverse for x quickly by checking for a cache
cacheSolve <- function(x, ...) {
    ## get the inverse of x (matrix)
    inverse <- x$getinverse()

   	## check if there is already an inverse if not continue
    if(!is.null(inverse)) {
        message("returning cached matrix")
         # if yes, return cached inverse
        return(inverse)
    }
    ## get the matrix
    data <- x$get()
    ## create inverse of the matrix
    inverse <- solve(data, ...)
    ## cache the matrix
    x$setinverse(inverse)

    ## return the inverse
    inverse
}
