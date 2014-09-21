## CACHING THE INVERSE OF A MATRIX
##
## The following pair of functions returns the inverse of a matrix. It
## avoids recomputing of the inverse by first checking and retrieving it
## from the cache, provide it has already been computed and the matrix
## has not changed; or by computing and cashing it, otherwise.
##
## Args:
##   x: matrix() which inverse matrix is to be calculated.
##
## Call:
##   cm <- makeCacheMatrix(x)
##   cacheSolve(cm)
##
## NOTE: 
##   The input matrix x must be invertible (i.e. a square n by n
##   matrix with a non 0 determinant).


## Creates a special "matrix" object that can cache its inverse, i.e.
## the list a list containing functions to set/get the value of the
## input matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function(solve) m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" created by the above function. 
## Checks first if the inverse has already been calculated. If so, retrieves
## it from the cache and skips calculations; otherwise, calculates it and sets
## its value in the cache via the 'setinverse' function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }      
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

