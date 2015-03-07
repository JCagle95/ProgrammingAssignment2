## The Function will solve for the inverse of the Matrix give in argument and cache it.

## The makeCacheMatrix is basically the replica of the example makeVector function

makeCacheMatrix <- function(x = matrix()) {
    
    # Set cache to NULL
    Inverse <- NULL
    
    # Update Matrix with new Matrix y
    set <- function(y = matrix()) {
        x <<- y
        Inverse <<- NULL
    }
    
    # Display Matrix
    get <- function() x
    
    # Store cache
    setInverse <- function(computedInverse) Inverse <<- computedInverse
    
    # Obtain cached Inverse Matrix
    cachedInverse <- function() Inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         cachedInverse = cachedInverse)
}


## Return a matrix that is the inverse of 'x', the Matrix given by makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    # Look for cachedInverse
    Inverse <- x$cachedInverse()
    if (!is.null(Inverse)){
        message("getting cached data")
        return(Inverse)
    }
    
    # Obtain Matrix and solve for inverse
    Matrix <- x$get()
    Inverse <- solve(Matrix)
    x$setInverse(Inverse)
    Inverse
}
