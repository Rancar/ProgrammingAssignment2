## The two functions makeCacheMatrix and cacheSolve take an invertible matrix
## and return the inverse matrix from the cache if available or the calculated inverse matrix if not.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
          x <<- y
          invM <<- NULL
        }
        get <- function() x
        setinvM <- function(inverse) invM <<- inverse
        getinvM <- function() invM
        list(set = set, get = get,
             setinvM = setinvM,
             getinvM = getinvM)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated returns the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinvM()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvM(inv)
        inv
}