## Put comments here that give an overall description of what your
## functions do
#This pair of functions are used to create a special object
#that stores a matrix and caches its inverse.


## Write a short comment describing this function
#makeCacheMatrix is a function which creates a special "matrix"
#that can cache its inverse.  In reality, it creates a list containing a function to:
#a. set the value of a matrix,
#b. get the value of a matrix,
#c. set the value of the inverse,
#d. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()){
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function(){
    x
  }
  set_inverse <- function(solve){
    inverse_matrix <<- solve #allows inverse_matrix to call the solve function
  }
  get_inverse <- function(){
    inverse_matrix
  }
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
#cacheSolve first checks to see if the inverse of the matrix 'x'
#has been calculated and cached (and the matrix has not changed).  
#If the inverse is cached, this inverse is returned.  
#If the inverse is not available in cache, the inverse is calculated,
#cached, and returned.  The inverse is set in the cache via the 
#set_inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}
