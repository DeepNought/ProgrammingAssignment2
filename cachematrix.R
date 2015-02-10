## Together these two functions create a "special" type of matrix that is albe to compute and cache its inverse
## matrix.  The fist function, makeCacheMatrix, creates the matrix and the operations associated with it.  These
## operations are implemented in four functions (set, get, setinverse and getinverse).  These functions are
## returned in a list.  The second function, cacheSolve, is used to create or access the cached inverse of the
## matrix.  It is passed the "special" matrix creaed by makeCacheMatrix and, if a cached version exists, returns
## it.  If not, it uses solve() to calculate the inverse, sets it and returns it.

## makeCacheMatrix creates a "special" matrix that can cache its inverse.  Returns a list of functions to operate
## on the "special" matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {    # a new matrix, wipe out any previous inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) inv <<- inverseMatrix
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is passed a "special" matrix created by the above function, and either returns a cached
## version of the inverse, or calls solve() to compute the inverse, then sets it and returns it

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                         # a cached inverse exists; just return it,
        message("returning cached inverse")     # but first demonstrate it's the cached version
        return(inv)
    }
    # if we got here an inverse matrix is not cached, so get the
    # original matrix, compute its inverse, set it and return it
    orig <- x$get()
    inv <- solve(orig, ...)
    x$setinverse(inv)
    inv
}
