## Put comments here that give an overall description of what your
## functions do
##
## this is the ASSIGNMENT 2 for the coursera R-programme
##
## created by : bbandaru 
## DATE: 20th December 2014
## 
## this contains mainly two functions
## 1-> makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2-> cacheSolve: This function computes the inverse of the special "matrix" 
##                returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##                then the cachesolve should retrieve the inverse from the cache.
## computing the inverse of a square matrix solve(x) function in R. 
## as per the assingment instructions  matrix  X is asummed to be an invertible matrix always

## Write a short comment describing this function
## this function is used to crate a cached matrix from the given base matrix
## it stores the inverse of the matrix and it also provides
## new methods for set and get the matrix in cache and also
## setsolve and getsolve to set and get the inverse of the matix


makeCacheMatrix <- function(x = matrix()) {
  
  ## place to store the inverse of matrix
  minv <- NULL
  
  ## the following function is to set the cache with new matrix
  set <- function(y) {
    
    ## it is better to check the input is a matrix or not?
    if (!is.matrix(y)) {
      ## input is not matrix thro a message and do nothing
      
      message( "Input is not a matrix. Please check your input")

    } else {
    
      ## as per the assignment isntructions here we are 
      ## assuming that the given matrix is invertible
    
      x <<- y
    
      ## every time the matrix is set with new matrix 
      ## reset matrix inverse value to NULL so it will be recaluated
      minv <<- NULL
    
    }
  }
  
  ## the following function simply returns the cached matrix
  get <- function() x 
  
  ## the following function set the value of inverse matrix with new value
  setsolve <- function(min) {
    ## before setting with a new matrix inverse check if it a matrix or not
    if (!is.matrix(min)){
      ##send error message and do nothing
      message ("input is not a matrix. Please check")
       
    } else {
      ## ok now set the value for inverse matrix
      minv <<- min
    }
  }  
  
  ## the follwoing fucntion retruns the cached inverse matrix
  getsolve <- function() minv 
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function

## this function returns the cached inverse matrix if available
## otherwise it calculates using the standard solve () fucntion 
## and will cache the value of inverse matrix and return the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  minv <- x$getsolve()
  if(!is.null(minv)) {
    ## It appears there is already invese matrix is cached.
    ## now make sure the cached matrix is same are not 
    data <- x$get()
    
    
    
    if (is.matrix(data))
      message("getting cached inversed matrix data")
    return(minv)
  }
  ## the inverse matrix is not cached or matrix is changed
  ## hence calculate it now
  
  data <- x$get()
  minv <- solve(data, ...)
  x$setsolve(minv)
  x
  minv
  
}