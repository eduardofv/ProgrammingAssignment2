#eduardofv
#Creates a Matrix List Object than can cache its inverse 
# using the cacheSolve function

#makeCacheMatrix creates a new cacheable-inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	x.inverse <- NULL
	set <- function(y) {
		x <<- y
		x.inverse <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) x.inverse <<- inverse
	getinverse <- function() x.inverse
	list(
		set=set,
		get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


#cacheSolve Solves (calculates inverse) con a inverse-cacheable matrix
#  returns the cached inverse if it's already been calculated

cacheSolve <- function(x, ...) {
	if( !is.null( x$getinverse() ) ) {
		message("retrieving inverse from cache")
		return( x$getinverse() )
	}
	x$setinverse( solve(x$get()) )
	x$getinverse()
}
