## This is an implementation of special matrix that cache's its inverse.

## This function creates the special matrix object.
## Arguments: x (optional) - the "normal" matrix you want to set to the
## "special" matrix object.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #This is for setting desired "normal" matrix to "special" matrix object.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #This is for getting the actual matrix from the special matrix object.
        get <- function() x
        
        #This is for setting the matrix's inverse.
        setInverse <- function(inverse) inv <<- inverse
        
        #This is for getting the matrix's inverse.
        getInverse <- function() inv
        
        #Return the special matrix's operations list.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This functions returns either cached or on-line computed matrix inverse
## that the "special matrix" object holds.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("Getting cached matrix inverse.")
                return(inv)
        }
        
        mat <- x$get()
        
        inv <- solve(mat, ...)
        
        x$setInverse(inv)
        
        inv
}
