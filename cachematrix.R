## A set of functions to store the inverse of a matrix
## since inverting a matrix is costly operation
# Example of use
# bMatrix <- matrix(1:4, 2, 2)
# myMatrix <- makeCacheMatrix(bMatrix)
# cacheSolve(myMatrix)
# myMatrix$getinverse()

# Create a matrix object with 2 functions
# To store the inverse matrix
# To retrieve the inverse matrix
makeCacheMatrix <- function(x = numeric()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Returns the inverse matrix of x if it exists
# Stores the inverse matrix of x with x$setinverse
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  # If inverse matrix has already been computed
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    # Return this already inverted matrix
    return(inverse_matrix)
  }
  
  # Test if the argument 'x' is a matrix and if it is square
  if (is.matrix(x$get()) & (dim(x$get())[1] == dim(x$get())[2]))
  {
    data <- x$get()    
    # Test if the matrix can be inverted
    if (det(data) == 0)
    {
      # A matrix with determinant = 0 can not be inverted
      message("This is not an invertible matrix")
      return(NULL)
    }
    else
    {
      # Compute the inverse and 'store' it in x
      inverse_matrix <- solve(data, ...)
      x$setinverse(inverse_matrix)
      # Return a matrix that is the inverse of x
      inverse_matrix      
    }    
  }
  else
  {
    # A non-square matrix can not be inverted
    message("This is not a square matrix")
    return(NULL)
  }
}
