## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will return a list containing four functions. Calling this 
# function will setup space for another function (cacheSolve) to save computation time 
# by caching the inverse of a matrix. 
# 1. set() - will define a matrix and reset the cached value to NULL
# 2. get() - will print the original matrix
# 3. setinv() - sets the inverse of the matrix to object m
# 4. getinv() - prints out the cached inverted matrix from object m
#
# usage: 
#     create a matrix:                m <- matrix(c(1, 2, 3, 4), 2, 2)
#     set up the function list:       newm <- makeCacheMatrix(m)
#     get the inverse of the matrix:  inv <- cacheSolve(newm)
# 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

# When called this function will check to see if there is a cached 
# value for the inverse of the matrix. If there is that value will be returned
# if not the inverse will be computed using solve(). 
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if (!is.null(m)) {
    message("retruned cached inverse")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}