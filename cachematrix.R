## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  inv = NULL   ## initialise inv as NULL; will hold the value of matrix inverse
  set <- function(y) {   ## define the set function to assign new value of matrix in parent environment
    x <<- y    
    inv <<- NULL     ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x  ## define the get function - returns the value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv  ## gets the value of inv where called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## needed in order to refer to the functions with the $ operator
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}
