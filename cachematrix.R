## These function make a matrix, that can store it's reverse as a cache. 
## The first function makes the special matrix which allowes inversing.
## The second function computes the inverse of that matrix. 
## Added to the second function is the ability to check the cache.
## Calculating the mean could take a lot of time, therefore it is useful to use the mean in cache when available.

## makeCacheMatrix. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  #  setting the inverse to NULL as a placeholder for a future value. This also clears inv. 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # This step defines the function to get the value of the matrix. 
  # We are setting the vector "x" to a new vector, "y" and reset the inverse "inv" to NULL.
  
  get <- function() x
  # this returns the vector x
  
  setInverse <- function(inverse) inv <<- inverse
  # this sets the inverse (this is inv) 
  
  getInverse <- function() inv
  # this returns the inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
# this returns the special vector that contains all these functions.


## Cachesolve. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, called "x". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) { # if the cache is emppty, take the following steps:
    message("getting cached data")
    return(inv)
}
mat <- x$get() 
# get the value of the matrix

inv <- solve(mat, ...) 
# calculate the inverse and store this result in m

x$setInverse(inv)
# cache the result

inv
# return the inverse 
}