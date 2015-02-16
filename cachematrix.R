# The first function, makeCacheMatrix creates "vector", 
# which is really a list containing functions to
# 
# sets the value of the matrix
# gets the value of the matrix
# sets the value of the matrix inverse
# gets the value of the matrix inverse


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


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x = matrix(), ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  inverse <- x$get()
  m <- solve(inverse, ...)
  x$setinv(m)
  m
  
}
