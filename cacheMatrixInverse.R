

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

##  1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##      retrieve the inverse from the cache



makeCacheMatrix <- function(x = matrix()) {

	Minv <- NULL

	set <- function(y) {

		x <<- y

		Minv <<- NULL

	}


	get <- function() x

	setinverse <- function(inverse) Minv <<- inverse

	getinverse <- function() Minv

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}



cacheSolve <- function(x, ...) {

	Minv <- x$getinverse()

	if(!is.null(Minv)) {

		message("getting cached data")

		return(Minv)

	}

	Mdata <- x$get()

	Minv <- solve(Mdata)

	x$setinverse(Minv)

	Minv

}
