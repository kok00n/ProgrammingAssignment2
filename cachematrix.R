## Put comments here that give an overall description of what your
## functions do
## Functions to optimize matrix inversions. Operation of matrices invertion 
## is very costly in terms of computing power, to avoid non necessary
## computations functions below store computed inversion in the memory which 
## could be reused in the future

## Write a short comment describing this function
## makeCacheMatrix caches matrix (given by x argument) in the memory.
## The function has getters and setters to get/set matrix and matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) i <<- inversion
  getinversion <- function() i
  list(set = set, 
       get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## Write a short comment describing this function
## cacheSolve function resolves matrix inversion on the object 
## given by x argument.
## If inversion parameter is set, function returns matrix inversion stored in
## the memory without computing it once again.

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if(!is.null(i)){
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
