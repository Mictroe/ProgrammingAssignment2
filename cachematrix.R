# MicTroe@gmail.com - Assingment II R-Programming

# The following pair of functions is ment to read a square matrix, convert it
# into its invers and caches the inverse until the initial matrix is changed.
# Changing the initial matrix will lead to solve the matrix from scratch.


# makeCacheMatrix converts the initial matrix "x" into its invers and stores
# the result in "m"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) m <<- solve
  getinvers <- function() m
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}



# cacheSolve has a look, whether matrix "m" returnd from the previos
# function ist empty. If not, it prints "getting cached data" to the console
# and returns the inverse matrix "m" from the chache. Otherwise the solve 
# funktion provides a newly built inverse matrix from changed input matrix "x".

cacheSolve <- function(x, ...) {
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvers(m)
  ## Return a matrix that is the inverse of 'x'
  
  m
}