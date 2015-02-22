## makeCacheMatrix and cacheSolve are a pair of functions that can be used to calculate 
## inverse of an invertible square matrix. They are used instead of a direct call to solve(), 
## to avoid re-computing the inverse for a given matrix, if it was already computed and  
## to re-use such a value that exists in the global environment.
##
## How to use:  fisrt call makeCacheMatrix and set the invertible square matrix;
## which returns a special matrix object (a list with access to get/set methods) that can
## then be passed to cacheSolve to compute the inverse.



## This function caches the inverse of a matrix. It returns a list containg get
## and set functions to access the matrix and its inverse. The matrix can be set by  
## passing it as a function argument, or by using makeCacheMatrix$set
##
## getInverse doesnt compute the value of inverse. It only returns cashed value
## Use cacheSolve function to compute.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  
  ## set can be used to initialize or change the associated matrix
  ## set function resets 'matrix' to the value provided, and resets its 'inverse' to null
  set <- function (matrix) {
      x <<- matrix
      inverse <<- NULL
  } 
  getInverse <- function() inverse
  setInverse <- function(inverseMat) inverse <<- inverseMat
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )

}


## This fuction computes inverse of a matrix, only when it does not find a
## corresponding cashed value in its argument. If found, the cache is returned. 
## Once the value is computed, it is cashed for future use. 
## The function uses solve() to invert the matrix (assuming invertible)
## Argument : 
## matrixObject -- is an oject returned from function makeCacheMatrix()

cacheSolve <- function(matrixObject, ...) {
 
  invr <- matrixObject$getInverse()
  
  if(is.null(invr)){
    ## compute only if there is no cache
    invr <- solve(matrixObject$get())
    matrixObject$setInverse(invr) 
  }   
  
  invr
}
