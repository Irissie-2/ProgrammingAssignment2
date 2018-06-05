## These function make a matrix, that can store it's reverse as a cache. 
## The first function makes the special matrix which allowes inversing.
## The second function computes the inverse of that matrix. 
## Added to the second function is the ability to check the cache.
## Calculating the mean could take a lot of time, therefore it is useful to use the mean in cache when available.

## makeCacheMatrix. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  #  setting the mean to NULL as a placeholder for a future value. This also clears m. 
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # This step defines the function to get the value of the matrix. 
  # We are setting the vector "x" to a new vector, "y" and reset the mean "m" to NULL.
  
  get <- function() x
  # this returns the vector x
  
  setmean <- function(mean) m <<- mean
  # this sets the mean (this is m) 
  
  getmean <- function() m
  # this returns the mean m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
# this returns the special vector that contains all these functions.


## Cachesolve. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, called "x". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) { # if the cache is emppty, take the following steps:
    message("getting cached data")
    return(m)
}
data <- x$get() 
# get the value of the matrix

m <- solve(data) 
# calculate the inverse and store this result in m

x$setInverse(m)
# cache the result

m
# return the inverse 
}