## Takes a matrix as its one argument and then 
## returns a list consisting of functions to create a cached version
## of the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) { 
  
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL 
  } 
  get <- function() x
  setinv <- function(inv) i <<- inv 
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv) 
} 

## Takes a matrix and checks if a cached version of its inverse
## exists, if it doesn't, it creates a cached version and returns it 

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
  }
  data <- x$get()  
  i <- solve(data, ...)  
  x$setinv(i) 
  i 
} 
