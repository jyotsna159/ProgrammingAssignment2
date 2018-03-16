## These functions return the inverse matrix of a given matrix. makeCacheMatrix takes matrix as an input 
## and stores the matrix and it's inverse as a list of getter and setter methods. This is to take advantage
## of the scoping rules of R to cache potentially time consuming computations. cacheSolve function first checks
## if the inverse matrix is already cached. If it is cached then it returns the cached value else calculates 
## the inverse matrix.


## This function takes a matrix as input and returns a list of 4 functions. <<- operator is used to cache variables 
## between function calls.

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) x_inv <<- solve
  getSolve <- function() x_inv
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
  
}


## This function returns the inverse of matrix (x) . It first check for cache, if it is there then it returns
## that value. If there is no cached value then it first checks if the matrix is a square matrix (whose number of 
## rows and columns are equal) then use solve method to get the inverse of the matrix
## Note: For this assignment, assumption is that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  
  x_inv <- x$getSolve()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  #check for square matrix
  if(length(data[,1]) == length(data[1,])){
    x_inv <- solve(data, ...)
    x$setSolve(x_inv)
  }else{
    message("Not a square matrix")
  }
  
  x_inv
}
