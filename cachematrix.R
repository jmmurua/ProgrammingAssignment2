## These functions allow for caching the inverse of a matrix, thus saving
## resources avoiding the repeated computation of the inverse matrix

## This function creates a special "Matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
  setinv = setinv, 
  getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}


## Testing

A <- matrix(c(1, 2, 4, 2, 1, 1, 3, 1, 2), nrow = 3)
my.matrix <- makeCacheMatrix(A)
cacheSolve(my.matrix)
cacheSolve(my.matrix)


