## Put comments here that give an overall description of what your
##  functions do

##  This function creates a special "matrix" object that can cache its inverse
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) invs
        getinverse <- function() invs
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

# This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("It is getting cached data.")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinverse(invs)
    invs
}
