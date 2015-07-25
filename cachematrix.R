## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object to cache its inverse
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


## Write a short comment describing this function
## This function returns the inverse of the matrix object

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  ## If the inverse is already calculated
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse is yet to be calculated
  data <- x$get()
  new_inv <- solve(data) %*% data
  x$setinverse(new_inv)
  new_inv  

}
