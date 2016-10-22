
## As part of the first function - makeCacheMatrix, i am trying to create matrix
## and store it. As input a square matrix is passed, or it can be inserted via a new function
## set_matrix. get_matrix will print the existing matrix. SetInverse will set the inverse of a matrix
## and getInverse will print the inverse of the matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function()inv
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##' cacheSolve function takes in the argument as a square matrix which is being created
##' as part of previous function.
##' Need to make use of the function solve for this purpiose which will do the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
