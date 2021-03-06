## a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        y <- x$getmatrix()
        x$setmatrix(y)
        m <- solve(y, ...)
        x$setinverse(m)
        m
}
