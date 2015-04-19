## A cached matrix is a matrix where the result of some operations are cached.
## It avoids to redo computation intensive operations again and again.
##
## For now, only the `solve` operation (computing inverse matrix) is implemented.
##
## Example:
## my_matrix <- makeCacheMatrix(matrix(rnorm(8 * 8), c(8, 8)))
## cacheSolve(my_matrix)

## makeCacheMatrix creates a cache matrix, which store computation
## intensive results.
makeCacheMatrix <- function(x = matrix()) {
  # init the cached solve result to null
  s <- NULL
  
  # assign a matrix to a cached matrix (and reset the cached solve value)
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # get the matrix associated with the cached matrix
  get <- function() x
  
  # assign the result of the solve operation
  setsolve <- function(solve) s <<- solve
  
  # get the result of the solve operation (cached)
  getsolve <- function() s
  
  # return all functions available for the cached matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve computes the inverse matrix of a cached matrix
## and cache the result
cacheSolve <- function(x, ...) {
  
  # see if the solve operation result is cached
  s <- x$getsolve()
  
  # it is cached
  if(!is.null(s)) {
    message("getting cached data")
    
    # return the cached result
    return(s)
  }
  
  # retrieve the matrix associated with the cached matrix
  data <- x$get()
  
  # compute the inverse matrix
  s <- solve(data, ...)
  
  # cache the result of the solve operation
  x$setsolve(s)
  
  # return the result of the solve operation
  s
}
