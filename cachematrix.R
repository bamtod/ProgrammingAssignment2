## set:Creates a matrix x
## get: stores the matrix x
## setinverse: assigns the inverse to m
## getinverse: stores the inverse m into cache
## matix has 4 functions: set, get, setinverse, getinverse

## Describe this function: Creates a matix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Describe this function:  Calculates the inverse of a matrix, checkes if the matrix is already its inverse
## Return from chache : calculates inverse and stores(sets) in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

