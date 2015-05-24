## The following two functions are used to calculate the inverse of a matrix.
## The first function, makeCacheMatrix(), creates a list that stores a matrix
## inverse. The function cacheSolve() takes a list of the type created by 
## makeCacheMatrix(), and if the matrix inverse has not already been set, 
## calculates and sets it.


## makeCacheMatrix():
##
## This function takes an invertible matrix and creates a list object that
## stores the inverse of the matrix.
##
## Usage: cacheMatrix <- makeCacheMatrix(myMatrix)
## myMatrix - an invertible matrix
## cacheMatrix - a list object containing the matrix inverse and functions to
##               access it
##
## member functions:
## set() - sets the input data (the input matrix)
## get() - returns the input data (the input matrix)
## setInverse() - sets the inverse matrix
## getInverse() - returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invMx){
    inverse <<- invMx
  }
  
  getInverse <- function(){
    inverse
  }
  
  ## Return the list with the member functions.
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## cacheSolve():
##
## This function takes a list object of the type created by makeCacheMatrix(),
## and sets the inverse of the input matrix, if it has not already been set.
##
## Usage: inverseMatrix <- cacheSolve(cacheMatrix)
## cacheMatrix - a list object containing the matrix inverse and functions to
##               access it
## inverseMatrix - the inverse matrix

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  
  inverse <- solve(x$get())
  
  x$setInverse(inverse)
  
  inverse
}
