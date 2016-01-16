## This function cache the inverse of a matrix
## This function works by storing a matrix and returning a cached value of inverse of that matrix

## makeCacheMatrix function is for creating a list that:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse 
## get the value of the inverse
## the "<<-" (superassignment) operator is used in this function

makeCacheMatrix <- function(x = matrix()) {
	result <- NULL

	set <- function(y){
		x <<- y
		result <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inv){
		result <<- inv
	}

	getInverse <- function() result

	list(	set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve will compute the inverse of the matrix that has been created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	result <- x$getInverse()
	
	## if the result is already available to use
	if(!is.null(result)){
		message("getting cached data")
		return(result)
	}

	## if not, then solve
	m <- x$get()
	result <- solve(m,...)
	x$setInverse(result)

	result
}
