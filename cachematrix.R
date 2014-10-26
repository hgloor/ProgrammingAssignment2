## functions to store and calculate the inverse of a matrix, storing 
## the result into a kind of cache

## makeCacheMatrix: function to store and show a matrix and your inverse 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: function to calculate the inverse of a matrix stored 
## in a "makeCacheMatrix" object

cacheSolve <- function(x, ...) {
	## Return the inverse matrix of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	i <- solve(x$get(), ...)
	x$setinverse(i)
	i
}