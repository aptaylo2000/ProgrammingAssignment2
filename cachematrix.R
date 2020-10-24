
## Put comments here that give an overall description of what your
## functions do

#These functions find the inverse of a matrix
# and caches the value; if it is needed later,
# the inverse can be retrieved without recalculating
#the value

## Write a short comment describing this function
# This function saves/caches value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  library(matlib) 
  
  m <- NULL #sets cached inverted matix to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

# This function looks for the cached inverse
# If it exists, it retrieves it.
# If it does not exist, it finds the inverse and
# caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

#Test Matrix
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#m1
#myMatrix_object <- makeCacheMatrix(m1)
#myMatrix_object


#myInvertedMatrix <- cacheSolve(myMatrix_object)
#myInvertedMatrix 


