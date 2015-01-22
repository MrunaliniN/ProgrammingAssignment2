# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. The following program defines function to cache the inverse of a matrix.



# This function creates a special "matrix" object that can cache its inverse
# and defines functions for get/set operations on this object. Created and returns 
# this list of functions
makeCacheMatrix <- function(x = matrix()) {
    
    inv_x <- NULL;
    
    # set the variables x & inv_x, which hold the matrix and its inverse
    set <- function(y) {    
        x <<- y
        inv_x <<- NULL    
    }
    
    # function to return the matrix
    get <- function() {x}
    
    # function to store the value y (which is the inverse of x)
    setinv <- function(y) {
        inv_x <<- y
    }
    
    # function to get the inverse stored by setinv()
    getinv <- function(y) {inv_x}
    
    # create and return list of functions, which can be called by subsetting into the matrix object created
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached matrix")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinv(inv_x)
    inv_x
}

