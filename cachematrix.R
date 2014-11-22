## Author: Joan R.
## Date: 11/22/2014

## Function 1 makeCacheMatrix
##		This consists a list of functions that stores a "matrix" object. 

## Function 2 cacheSolve
##		This either gets the cached inverse of the cached matrix
##			or sets the inverse of the newly stored matrix.


## makeCacheMatrix creates a special "matrix" object with a set of functions.
## The functions are the following:
##		1. set - stores the given matrix
##  	2. get - retrieves the cached matrix
##		3. setinv - assigns the inverse of the given matrix
##		4. getinv - retrieves the cached inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
	print("starting makeCacheMatrix")
	print("matrix value: ")
	print(x)
	inv <- NULL
	
	set <- function(y) {
		print("setting cached matrix")
		print("y")
		print(y)
		x <<- y
		inv <<- NULL
	}
	
	get <- function () { 
		print("getting cached matrix")
		x 
	}
		
	setinv <- function(inverse) { 
		print("setting inverse")
		inv <<- inverse 
	}
		
	getinv <- function () { 
		print("getting cached inverse")
		inv
	}
			
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## cacheSolve returns the cached inverse of the given matrix 
##		or the computed inverse of 'x' and stores its value.

cacheSolve <- function(x, ...) {
	print("starting cacheSolve")
	
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


