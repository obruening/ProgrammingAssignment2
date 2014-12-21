## Creates a cached matrix.
## The returned list provides methods to set and get a matrix and set and get the inverted matrix.
## The inverted Matrix is stored in a cached variable m.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Takes the cached matrix as a parameter ('x') and returns the inverted matrix.
## It only calls the time intensive function solve() if there is no cached value for the inverted matrix.
## If there is a cached value for the inverted matrix, it is just returned.
## if solve() must be called, it is called only once and the value is then stored for future use in the 
## cached matrix. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}