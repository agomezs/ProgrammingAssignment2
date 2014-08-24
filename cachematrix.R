## makeCacheMatrix would be special "matrix" that stored the inverse. 
## With cacheSolve you can cache the inverse of your matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   
   setInverse <- function(y){
      i <<- y
   }
   
   getInverse <- function() i
   
   getOriginal <- function() x
   
   ## Returns the "matrix" with the functions.
   list(setInverse = setInverse,
        getInverse = getInverse,
        getOriginal = getOriginal)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {    
    ## Checks if the inverse matrix is cached.
    i <- x$getInverse()
    
    if(!is.null(i)) {
        message("getting cached data") 
        return(i)
    }
    ## Gets the original matrix.
    m <- x$getOriginal()
    ## Stores the inverse matrix.
    i <- solve(m)
    x$setInverse(i)
    ## Returns a matrix that is the inverse of 'x'.
    i    
}


