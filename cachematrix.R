## These two functions make the inverse of a given matrix and store
## this value.  If available cacheSolve will return the cached value
## or calculate and store it



## makeCacheMatrix creates a matrix 'wrapper' to store a cached inverse
## Set and get store and retrieve the data; setinverse and getinverse 
## set the inverse value and returns the cached inverse if available

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<-NULL
	}

	get <- function() x
	setinverse <- function(inverse) i <<-inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, 
		getinverse= getinverse)

}



## cacheSolve returns the cached value or calculates it. 
## Additional arguments for solve() can be passed in addition
## to the matrix.

cacheSolve <- function(x, ...) {
	i <-x$getinverse()
	if(!is.null(i)) {
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
