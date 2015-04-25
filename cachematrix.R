## makeCachrMatrix creates a matrix that is able 
##to cache the inverse of matrix passed as a function
## if the value has not been recieved before, get is used,otherwise
##the inverse stored as cache is passed

## the following function returns a list of values which contain 
## the cached matrix(if applicable), or the newly generated inverse if 
##the passed matrix is not cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()
  {
    x
  }
  setinverse <- function(inv)
    {
    i <<- inv
  }
  getinverse <- function() {
    i
  }
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function first checks if the inverse of the matrix already exists in the cache
##if it does, it returns the previously calculated inverse
##else, it uses the solve function to calculate and return the inverse
##an inverse once calculated is cached and a message is printed if the inverse
##is being retrieved from cache

cacheSolve <- function(x, ...) {
        ## return inverse of x
  i <- x$getinverse()
  if(!is.null(i)) {
    message("retrieveing cached matrix")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
