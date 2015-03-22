## makeCacheMatrix is to create a matrix Object and
##cacheSolve finds the inverse of the matrix
## The cache will have the results of the inverse and 
##the calculation will not be done again

makeCacheMatrix <- function(x = matrix()) {
  invrse_x <- NULL
  set <- function(y) {
    x <<- y
    invrse_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) invrse_x <<-inverse
  getinverse <- function() invrse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix created with the function above
## If the inverse is available
##the result is returned, else it is computed cached and returned
cacheSolve <- function(x, ...) {
  ## Return the inverse of X
  invrse_x <- x$getinverse()
  if (!is.null(invrse_x)) {
    message("getting cached inverse matrix")
    return(invrse_x)
  } else {
    invrse_x <- solve(x$get())
    x$setinverse(invrse_x)
    return(invrse_x)
  }
}

makeCacheMatrix(matrix(c(1:4), nrow = 2, ncol=2))
cacheSolve(makeCacheMatrix(matrix(c(1:4), nrow = 2, ncol=2)))