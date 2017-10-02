## The following two functions will create a special matrix, and then solve it.

## The first function will create a special matrix able to set and get its contents, as well as set and get its inverse.

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


## The following function will solve the special matrix, or get the cached value of the inverse if present.

cacheSolve <- function(x, ...) {
        
  inverseMatrix <- x$getinverse()
  if (!is.null(inverseMatrix)) {
    message("Getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  return(x$getinverse())
}
