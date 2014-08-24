## Assignment: Caching the Inverse of a Matrix
## Functions makeCacheMatrix and cacheSolve are written to cache the inverse of ## a matrix. The function makeCacheMatrix creates a special matrix object which ## is given as input to the function cacheSolve.

## makeCacheMatrix function creates a special object to store the input matrix
## and inverse of it.
## The function contains a list of functions to set the value of matrix, get the## value of matrix, set the inverse of matrix, and get the inverse of matrix.

makeCacheMatrix <- function(x = matrix(numeric())) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates and returns the inverse of special
## matrix object created with makeCacheMatrix function.
## The function checks if the inverse already exits in cache, if Yes then
## cached inverse data is returned Else inverse is computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message("Calculating the inverse...")
        m <- solve(data, ...)
        message("Cached the inverse data")
        x$setinverse(m)
        m
}
