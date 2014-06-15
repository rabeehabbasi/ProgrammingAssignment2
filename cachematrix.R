## These functions are used to compute the inverse of a invertable 
## matrix. First time the inverse of the matrix is computed and 
## store in the cache. For later calls inverse from the cache is
## returned

## Example:
## m <- matrix(rnorm(9), nrow=3,ncol=3)    ## create a square matrix
## x <- makeCacheMatrix(m)                 ## create cache matrix
## cacheSolve(x)                           ## compute the inverse
## cacheSolve(x)                  ## result from cache is returned


##Function for creating the cache matrix
makeCacheMatrix <- function(x = numeric()) {
  
  ## Input: data matrix x
  ## Output: cached matrix
  
  inv <- NULL                   ##variable to cache the inverse of the matrix
  set <- function(y) {          ##function to initialize the matrix x
    x <<- y                     ##store the matrix
    inv <<- NULL                ##set inverse to null
  }
  get <- function() x           ##get the matrix
  setinv <- function(i) inv <<- i  ##set the inverse of the matrix
  getinv <- function() inv         ##get the inverse of the matrix
  list(set = set, get = get,       ##list of functions returned
       setinv = setinv,
       getinv = getinv)
}


##This function stores a matrix and its cache using the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Input: an invertable matrix and ... parameters for the solve function
  ## Output: inverse of the matrix
  
  inv <- x$getinv()     ## try to get the cached inverse
  
  if(!is.null(inv)) {   ## if cached inverse is found then return it
    message("getting cached data")
    return(inv)
  }
  
  ## other compute the inverse and cache it
  data <- x$get()           ##get the matrix
  inv <- solve(data, ...)   ##compute its inverse
  x$setinv(inv)             ##cache the inverse
  inv                       ##return the newly computed inverse
}
