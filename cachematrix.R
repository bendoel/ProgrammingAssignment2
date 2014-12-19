## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function makeCacheMatrix for storing inverse matrix
# only accept input square matrix
makeCacheMatrix <- function(x = matrix()) {

#var im for storing inverse matrix
  im <- NULL
  
  # update matrix x	
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  
  # return value of input's x
  get <- function() x
  
  # set inverse matrix into variable im inside makeCacheMatrix's function	
  setInvMatrix <- function(invMatrix){
    im <<- invMatrix
  }
  
  # return inverse matrix
  getInvMatrix <- function()
    im
  
  # return list containing set, get, setInvMatrix, getInvMatrix
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)	
}


## Write a short comment describing this function

# function cacheSolve for doing inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

#   get inverse matrix from object x
  im <- x$getInvMatrix()
  
  if(!is.null(im)) {
#   if going this way that mean we already have the result of inverse matrix operation
    message("getting cached data")
    return(im)
  }
  
# if going this way that mean we don't have the result, so lets do inverse matrix
  data <- x$get()
  im <- solve(data)

# save inverse matrix into object x  
  x$setInvMatrix(im)
  im
}