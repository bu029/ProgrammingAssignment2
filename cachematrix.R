## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #hold the catch value if not it is NULL
  #initially NULL
  inver <- NULL
  #storing a matrix
  set <- function(y) {
    x <<- y
    #matrix assigned a new value
    inver <<- NULL
  }
  #stored matrix return
  get <- function() x
  #set given arguments
  setInverse <- function(inverse)
  {
      inver <<- inverse
  }
  #get cached value
  getInverse <- function(){
    inver
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  #get chache value
  inver <- x$getInverse()
  #return if catche value exists
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  #if not calculate inverse of matrix and store it 
  matrixx <- x$get()
  inver <- solve(matrixx, ...)
  x$setInverse(inver)
  #return the inverse matrix
  inver
}
