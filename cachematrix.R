# The first function is used to cache the inverse of a matrix
# and the second function uses that cached value (if the inverse
# has already been cached) to return the value of the inverse.

# makeCacheMatrix creates a list which sets and gets the value of a
# matrix ('set' and 'get'), as well as methods to set and get the
# value of the inverse of the matrix ('setinverse' and 'getinverse').
makeCacheMatrix <- function(x = matrix()) {
    #inv will store the cached inverse of the matrix
	inv <- NULL
    
	#sets the matrix
	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
	#gets the matrix
	get <- function() x
    
	#sets the inverse of the matrix
	setinverse <- function(inverse) inv <<- inverse
    
	#gets the inverse of the matrix
	getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of a matrix. If the inverse
# has already been computed, it returns the cached value from the first
# function (inv). If not, it computes the inverse.

cacheSolve <- function(x, ...) {
	# If inv is not NULL, return the cached value
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
	
	# If there is no cached value, compute the inverse using solve function	
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv		
}
