## Make Matrix will create a matrix, set, get , setInverse and getInverse matrix
## CacheInverse will get inverse in cache if not present then it will create an inverse matrix and then returns it

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##set function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##get function
  get <- function() x
  
  ##setinverse function to inverse matrix
  setInverse <- function(inverse) i <<- inverse
  
  ##getInverse to get the inverse of matrix
  getInverse <- function() i
  
  ##list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  ## if in cache retrieve it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##get matrix
  data <- x$get()
  
  ##inverse the matrix
  m <- solve(data)
  
  ##setInverse function is called to set
  x$setInverse(m)
  
  ##return the inverse matrix
  m
}
