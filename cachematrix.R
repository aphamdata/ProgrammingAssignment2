## This function creates a matrix that caches its inverse
makeCacheMatrix <- function(x = numeric ()){
  ## Empty Inverse Property
  m <- NULL
  ## Method set the matrix y
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## Method to get the matrix
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  ## Returns the list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the matrix by the function makeCacheMatrix. It also checks to see if the
## inverse is calculated then returns the inverse
cacheSolve <- function(x, ...){
  ## Returns matrix inverse of x
  i <- x$getInverse()
  ## Condition to test if inverse is set
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## Gets matrix from the object
  data <-x$get()
  
  ## Calcuates the inverse matrix
  i <- solve(data)
  x$setInverse(i)
  ## Returns the matrix
  i
}
