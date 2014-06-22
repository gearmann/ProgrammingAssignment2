## This pair of functions provide the ability to store a matrix and
## its inverse.  The inverse is computed the first time that it is
## accessed and then cached for later use.
##
## Example Usage:
## > m <- matrix(rnorm(9), 3, 3)
## > cm <- makeCacheMatrix(m)
## > inv <- cacheSolve(cm)          [This will compute the inverse]
## > inv <- cacheSolve(cm)          [This will now retrieve the cached inverse]

## This function caches a matrix and its inverse.  It returns a list 
## of functions that the cacheSolve() function uses to get/set the 
## cached matrix as well as get/set the cached inverse.
makeCacheMatrix <- function(matrix = matrix()) 
{
    ## define the local cache
    inverse <- NULL
    
    ## functions for getting and setting the cached matrix
    set <- function(newMatrix) {
        matrix <<- newMatrix
        inverse <<- NULL
    }
    get <- function() matrix
    
    ## functions for getting and setting the cached inverse
    setinverse <- function(newInverse) inverse <<- newInverse
    getinverse <- function() inverse
    
    ## returns the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function takes the return from makeCacheMatrix, checks to see
## if the inverse has already been computed or not, and if so returns
## the cached inverse.  If not, then the inverse is calculated using
## the solve() function, placed in the cache, and then returned.
##
## Any additional args beyond the cacheMatrix are passed to the solve() 
## function.
cacheSolve <- function(cacheMatrix, ...) 
{
    ## check the cache
    inverse <- cacheMatrix$getinverse()
    if(!is.null(inverse)) 
    {
        ## we found the cached inverse, so return it
        message("getting cached data")
        return(inverse)
    }
    
    ## compute and cache the inverse
    matrix <- cacheMatrix$get()
    inverse <- solve(matrix, ...)
    cacheMatrix$setinverse(inverse)
    
    ## return the newly computed inverse
    inverse
}
