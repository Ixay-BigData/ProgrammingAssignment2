## Cache the inverse of a matrix

## Create a list that contains getters and setters 
## for the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Set new data (without inverse)
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## Getter for the data
  get <- function() x
  
  ## Setter and getter for the cached inverse
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  
  ## List to return
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of a matrix computing it
## or getting it from cache
cacheSolve <- function(x) {
  inv <- x$getInv() ## Retrieves the cache value
  
  if(!is.null(inv)) { ## Test it
    message("getting cached data")
    return(inv)
  }
  
  ## If there is no cached value
  data <- x$get()
  inv <- solve(data)  ## Compute the inverse
  x$setInv(inv) ## Store it
  
  inv
}
