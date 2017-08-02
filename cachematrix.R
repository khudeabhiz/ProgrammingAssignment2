## makes cache matrix
## 

## this function creates a cached matrix that can be used again without calculating inverse again

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	get <- function(y) x
	
	setinverse <- function(inverse) m <<- inverse

	getinverse <- function() m

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## this method checks that for a given matrix if the inverse if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()

	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
