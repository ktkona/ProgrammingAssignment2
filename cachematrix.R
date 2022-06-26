#create square invertible matrix object, x, that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  #initialize inverse matrix 
  inversematrix <- NULL
  
  #define set function for matrix
  set <- function(y) {
    #assigning values to x and inversematrix in environments different from current
    x <<- y
    inversematrix <<- NULL
  }
  
  #define return matrix object
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse 
  
  #define function for returning inverse matrix
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#x is output of makeCacheMatrix(),cacheSolve will return 
#inverse of matrix inputted to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inversematrix <- x$getinverse()
  
  #check if inverse matrix already calculated
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  
  #if inverse matrix not already calculated, calculate inverse matrix
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  
  #return inverse matrix
  inversematrix
}
