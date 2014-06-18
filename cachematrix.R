# Description: Creates special matrix with methods (set, get, setinverse, getinverse)
#              to get and set the matrix and its inverse
# Input: matrix
# Returns: object with getters and setters for matrix's value and inverse
makeCacheMatrix <- function(m = matrix()) 
{
  # set the value of matrix
  set <- function (y) 
  {
    x <<- y
    i <<- NULL
  }
  
  # get the value of matrix
  get <- function() x

  # set the inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  # get the inverse of matrix
  getinverse <- function() i

  # initialize object with function input m 
  set(m)
  
  # return getter/setter methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Description: Returns inverse of input cached matrix. 
#              If inverse is already cached, the function simply returns the cached value.
#              Otherwise, it computes the inverse, caches the result in input x, and returns the value. 
# Input: cached matrix, created with makeCachedMatrix()
# Returns: inverse of input matrix 
cacheSolve <- function(x, ...) 
{
  # return cached inverse of matrix if available
  i <- x$getinverse()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  # compute inverse of matrix
  data <- x$get()
  i <- solve(data, ...)
  
  # cache and return the inverse
  x$setinverse(i)
  i
}