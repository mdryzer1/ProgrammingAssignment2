## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix() function creates a special "matrix" object that is really
## just a list of functions to:
##  1) Set the value of the matrix;
##  2) Get the value of the matrix;
##  3) Set the value of the inverse;
##  2) Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The cacheSolve() function takes the special "matrix" object returned by
## makeCacheMatrix and computes its inverse. If this has already been done, then
## it just retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message('getting cached data')
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}