## To overcome repeated computation(Matrix inverse) which is costly operation.
## We will store the result of the calucated inverse of matrix for future reference instead of recomputing it.

## Generic function definitions for setter as well as getter for value and computed result.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve will return the required inverse of matrix value after computation for the first time or 
## if already computed it gets from cache. (With the help of CacheMatrix Function.)

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
