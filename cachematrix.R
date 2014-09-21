## These functions caches the inverse of a matrix

## Create a cache of a matrix
makeCacheMatrix <- function(x = matrix()) {
  ## stores the inversed matrix
  inversedMatrix <- null  
  ## set the value of the matrix
  set <- function(matrixValue) {
    storedMatrix <<- matrixValue
    ## new matrix stored so initialis the inversedMatrix
    inversedMatrix <- null
  }
  ## get the value of the matrix
  get <- function() {
    storedMatrix
  }
  ## set the inverse of the matrix
  ## I assume that the matrix supplied is always invertible
  setInverse <- function(inv){
    inversedMatrix <<- inv
  }
  ## get the inverse of the matrix 
  getInverse <- function (){
    inversedMatrix
  }
  ## return a list of the functions
  list(
    set=set,
    get=get,
    setInverse=setInverse,
    getInverse=getInverse)
}

## gets the cached matrix
cacheSolve <- function(x, ...) {
  ## get the cached inversed matrix
  inversedMatrix <- x.getInverse()
  ## check if there is a cached version of the inversed matrix 
  if(!is.null(inversedMatrix))
  {
    ## return the existing matrix
    return(inversedMatrix)
  }
  ## get the matrix because there wasn't a cached version
  matrixData <- x$get()
  ## calculate the inverse of the matrix
  inversedData <- solve(matrixData,...)
  ## cached the inversed matrix
  x$setInverse(inversedData)
  ## return the inversed matrix
  inversedData
}

