## makeCacheMatrix creates a special “matrix” that:
## sets the elements of the matrix
## gets the elements of the matrix
## sets the elements of the matrix inverse
## gets the elements of the matrix inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## checks to see if the inverse has already been calculated
## if "yes" gets invers from cache
## calculates the inverse of “makeCacheMatrix”

## Return a matrix that is the inverse of 'x'
cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
