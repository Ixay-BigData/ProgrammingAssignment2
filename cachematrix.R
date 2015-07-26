## Cache the inverse of a matrix

## Create a list that contains getters and setters 
## for the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of a matrix computing it
## or getting it from cache
cacheSolve <- function(x) {
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  
  inv
}
