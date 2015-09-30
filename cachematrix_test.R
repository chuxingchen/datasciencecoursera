##  1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##      retrieve the inverse from the cache


## This function creates a special "matrix" object that can cache its inverse.

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




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
