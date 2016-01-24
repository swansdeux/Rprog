## Caching the inverse of a matrix
## Only works with square matrices

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) s <<- solve
                getinverse <- function() s
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by the
## function makeCacheMatrix. If the inverse has already been calculated, 
## and the matrix hasn't changed, then the inverse is retrieved from the
## cache rather than recalculated.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
