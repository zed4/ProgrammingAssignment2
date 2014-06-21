## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
	## set the local cache to null if we don't do this, the cache will never
	## change once we set it, which is a problem if we want to check multiple
	## matrices
	m <- NULL
	
	## define a function to set 
	set <- function(y) {
		## set the value of x in the parent scope to the passed value
		x <<- y
		## set the cache to null in the parent in the parent scope
		m <<- NULL
	}
	
	## getter function: returns the passed matrix
	get <- function() x
	
	## sets the cache to the passed value
	setinv <- function(inv) m <<- inv
	## gets the cache
	getinv <- function() m

	## create the functions
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	## get the stored inverse
	m <- x$getinv()
	if(!is.null(m)) {
		## we hit the cache! yay!
		message("getting cached data")
		return(m)
	}
	## the cache must have been empty... 
	
	## get the matrix
	data <- x$get()
	## solve it
	m <- solve(data, ...) 
	## set the cache
	x$setinv(m)
	## return the solution
	return(m)
}
