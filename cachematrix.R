# The two functions bellow, named makeCacheMatrix and cacheSolde, are permitting
# to cache potentially time consuming computations, avoiding to user to repeatedly
# being computing the inverse of the same matrix. For the two functions, the 
# matrix inverse is computed only if the matrix changed. If the matrix doesn't
# change and the matrix inverse is not null,the second function, instead of 
# computing, cache the matrix inverse from the first function and skip the 
# the conputation.

# define the first matrix named makeCacheMatrix
makeCacheMatrix <- function( M = matrix() ) {
  
  ## Matrix inverse, MInv, is initialize to null
  MInv <- NULL
  
  ## sub-function permitting to set the matrix
  setM <- function( matrix ) {
    M <<- matrix
    MInv <<- NULL
  }
  
  ## sub-function permitting to get the matrix
  getM <- function() {
    ## Return the matrix
    M
  }
  
  ## sub-function permitting to set the matrix inverse
  setMInv <- function(inverse) {
    MInv <<- inverse
  }
  
  ## sub-function permitting to get the matrix inverse
  getMInv <- function() {
    ## Return the inverse of the matrix
    MInv
  }
  
  ## Return a list of sub-functions
  list(setM = setM, getM = getM,
       setMInv = setMInv,
       getMInv = getMInv)
}


## The second function bellow, named cacheSolde, Compute the inverse of the 
## special matrix returned by the first function, makeCacheMatrix. But before 
## computing, it checks the inverse from the first matrix. If the inverse is 
## already computed  and the matrix doesn't change, he gets it and skips the
## the computation. If not, it computes the inverse and sets it.

# define the first matrix named cacheSolde
cacheSolve <- function(M, ...) {
  
  ## getting the matrix inverse from the first function
  MInv <- M$getMInv()
  
  ## checks and return the set matrix inverse
  if( !is.null(MInv) ) {
    message("getting cached Mdata")
    return(MInv)
  }
  
  ## if the check above failed, matrix data is gotten from the first function
  Mdata <- M$getM()
  
  ## compute the invere from the matric data
  MInv <- solve(Mdata)
  
  ## Set the inverse computed
  M$setMInv(M)
  
  ## return the inverse
  MInv
} 
