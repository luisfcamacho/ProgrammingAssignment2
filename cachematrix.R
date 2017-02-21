## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that create a special object that stores a
## matrix and then caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv.m <- NULL
        set <- function(y) {
                x <<- y
                inv.m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv.m <<- inverse
        getInverse <- function() inv.m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the matrix created by makeCacheMatrix
## function from above. If the inverse has already been calculated (and the 
## matrix has not changed), it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv.m <- x$getInverse()
        if(!is.null(inv.m)) {
                message("getting cached data")
                return(inv.m)
        }
        mtx <- x$get()
        inv.m <- solve(mtx, ...)
        x$setInverse(inv.m)
        inv.m
}