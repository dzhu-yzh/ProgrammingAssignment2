## This pair of functions can be used to calculate a
## matrix inverse and cache it

##  This function creates a special "matrix" object
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		## Set the matrix data
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
		## Get the matrix data
        get <- function() x
		## Set the inverse for cache
        setinverse <- function(inverse) inv <<- inverse
        ## Get the inverse for cache
		getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
		## Check if inverse has been cached and return the cache if
		## it is the case
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
		## Inverse is not cached, get the data, compute the inverse
		## and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
