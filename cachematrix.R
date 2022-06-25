## makeCacheMatrix builds a set of functions
## and returns the functions to the parent 
## environment

## cacheSolve returns the inverse of a matrix
## from the cache if it is in there or by 
## calculating the inverse

## Create functions that will store inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize objects
 
    m <- NULL 

  ## Define behaviors for the objects

    set <- function(y) {
           x <<- y
           m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m

  ## Assign the functions to a list and return to 
  ## parent enviornment

    list(set = set, get =  get, ## gives names to the 
         setInverse = setInverse, ## above functions
         getInverse = getInverse)
}


## Checks for inverse of matrix in cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ## If !is.null(m.local) is false cacheSolve returns
        ## inverse matrix from parent environment
    
    m.local <- x$getInverse()

    if(!is.null(m.local)) {
        message("getting cached data")
        return(m.local)
    }

      ## If !is.null(m.local) is true cacheSolve calculates
      ## the inverse matrix

    data <- x$get()
    m.local.calculated <- solve(data, ...)
    x$setInverse(m.local.calculated)

    m.local.calculated

}
