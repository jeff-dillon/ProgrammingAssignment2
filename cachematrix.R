# Caching the Inverse of a Matrix
# Author: Jeff Dillon

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Args: 
    #   x: a matrix 
    #
    # Returns: 
    #   calculated inverse of matrix
    
    # Error Handling:
    # check that x is a matrix
    if(class(x) != "matrix") {
        stop("Argument x must be of class matrix")
    }

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) m <<- solved
    getsolved <- function() m
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.   
cacheSolve <- function(x, ...) {
    # Args: 
    #   x: the "cache matrix list" to be inverted
    #   ...
    #
    # Returns:
    #   a matrix that is the inverse of 'x'
    
    # Error Handling:
    # check that x is a list
    if(class(x) != "list") {
        stop("Argument x must be of class list")
    }
    
    m <- x$getsolved()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) 
    x$setsolved(m)
}
