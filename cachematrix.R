## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(b) inverse <<- b
  
  getinverse <- function() inverse
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if (!is.null(inverseMatrix)) {
    message("Getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
}
