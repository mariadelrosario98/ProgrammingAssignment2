#your goal is to create a "smart" matrix that remembers its own inverse so 
#you don't have to waste computer power calculating it over and over again.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  # 1. Function to change the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL # If the matrix changes, clear the cache!
  }
  # 2. Function to see the matrix
  get <- function() x
  # 3. Function to save the inverse to the 'inv' variable
  setInverse <- function(solve) 
    inv <<- solve
  # 4. Function to look at the 'inv' variable
  getInverse <- function() inv
  # Return all these tools in a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse() # Ask the locker if the inverse is there
  # If the note isn't empty, return it and stop!
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise, get the matrix, calculate the inverse, and save it back
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv) # This "writes" the answer on the sticky note
  inv
}
