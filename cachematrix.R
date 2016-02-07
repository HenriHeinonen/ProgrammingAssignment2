## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## by Henri Tapani Heinonen: "makeCacheMatrix creates a special matrix, which is really 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix"
## https://www.coursera.org/learn/r-programming/discussions/ThBCeMroEeW9ug73QL_FVQ
## https://www.coursera.org/learn/r-programming/module/6BaZ2/discussions/RN9nzcYWEeWl4RIs9EHA5Q
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## > A <- matrix(c(2,4,3,1,5,7,10,10,2), nrow=3,ncol=3,byrow=TRUE)
## > matriisi <- makeCacheMatrix(A)
## > matriisi
## > matriisi$get()
##      [,1] [,2] [,3]
## [1,]    2    4    3
## [2,]    1    5    7
## [3,]   10   10    2
## > matriisi$getsolve()
## NULL
## > cacheSolve(matriisi)
##        [,1]    [,2]     [,3]
## [1,] -1.875  0.6875  0.40625
## [2,]  2.125 -0.8125 -0.34375
## [3,] -1.250  0.6250  0.18750
## > matriisi$getsolve()
##        [,1]    [,2]     [,3]
## [1,] -1.875  0.6875  0.40625
## [2,]  2.125 -0.8125 -0.34375
## [3,] -1.250  0.6250  0.18750
## > cacheSolve(matriisi)
## getting cached data
##        [,1]    [,2]     [,3]
## [1,] -1.875  0.6875  0.40625
## [2,]  2.125 -0.8125 -0.34375
## [3,] -1.250  0.6250  0.18750
## > matriisi$getsolve()
##        [,1]    [,2]     [,3]
## [1,] -1.875  0.6875  0.40625
## [2,]  2.125 -0.8125 -0.34375
## [3,] -1.250  0.6250  0.18750
## > matriisi2 <- cacheSolve(matriisi)
## getting cached data
## > matriisi2
##        [,1]    [,2]     [,3]
## [1,] -1.875  0.6875  0.40625
## [2,]  2.125 -0.8125 -0.34375
## [3,] -1.250  0.6250  0.18750
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Write a short comment describing this function
## by Henri Tapani Heinonen: "This function solves the inverse of the matrix and stores 
## the value so it will not be necessary to do the time-consuming 
## computation again..."
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
