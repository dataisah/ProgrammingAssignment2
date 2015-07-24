## The first functions creates a special matrix object that caches the 
## inverse of the matrix, the second calculates the inverse of the special 
## "matrix" created by the first function

## This function creates a special matrix object, which does the following:
## sets the value, gets the value, sets the value of the inverse, and gets
## the value of the inverse.

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


## This function calculates the inverse of the above matrix when it is not 
## calculated, and retrieves the inverse if there is a stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
