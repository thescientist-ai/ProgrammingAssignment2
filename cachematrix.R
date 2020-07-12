## makeCacheMatrix creates a "special" matrix and cacheSolve solves
## for the inverse of this "special" matrix, if it has not yet 
## been calculated. If it has been calculated already, it returns
## the cached value.

## makeCacheMatrix creates a "special" matrix that:
## 1. set the value of the matrix
## 2. get the value of the matrix inverse
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses the "special" matrix obtained from
## makeCacheMatrix.  It checks if the matrix inverse
## has already been calculated.  If it has, it returns
## the cached value.  If it has not, it calculates
## the matrix inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
