## Functions to cache the inverse of a matrix and return
## it from cache when populated.  If not populated yet
## inverse will be calculated, returned and written to cache

## Function to return a list of functions for manipulating cache 

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
			x <<- y
			i <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) i <<- solve
		getinverse <- function() i
		list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Function to actually calculate the inverse (if not cached)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}
