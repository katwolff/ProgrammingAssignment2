## This pair of functions can be used to check whether the inverse of a matrix
## has already been computed. If yes (and the matrix elements have not been
## changed in between), the inverse is retrieved from cache. Otherwise the
## inverse is computed and stored in a cache.


## This function is passed a matrix and returns a list of functions to set
## (the local copy of) the matrix, to get the matrix and to set and get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## If matrix elements are changed, the inverse is no longer up to date
        ## and therefore set back to NULL
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function tries to retrieve the matrix inverse from cache. If it is
## successful, that's what it returns and we're done. Otherwise it calculates
## and sets the matrix inverse before also returning it. The function is called 
## with the "matrix object" created by makecacheMatrix() which is to be inverted 
## and optional further arguments which are passed on to solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    matrix <- x$get()
    
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
