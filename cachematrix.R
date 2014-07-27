## The following functions allow us to create a cached matrix and compute its inverse

##This function creates a cached matrix with its cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function computes the inverse of a matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) { #If the inverse is cached, it is retrieved and returned
            message("getting cached data")
            return(inv)
    }
    toInv <- x$get()
    inv <- solve(toInv, ...)
    x$setinverse(inv)
    inv
}