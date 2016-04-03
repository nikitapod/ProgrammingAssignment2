## This set of two function allows to cache the result of inverse matrix calculation, and use this result if
## the calculation is already done, instead of resolving the matrix again.

## makeCacheMatrix function creates list of functions to set the matrix, get the matrix, 
## set the values of inverse matrix, and get the values of inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
         }
      get <- function() x
      setsolve <- function(solution) inv <<- solution
      getsolve <- function() inv
      
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)

}


## cacheSolve function returns inverse matrix from cache if it is available, 
## otherwise it solves the input matrix, then cashes and returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              inv <- x$getsolve()
              if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)
              x$setsolve(inv)
              inv
}

## Usage example is below:
## > mymatrix <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## > cacheSolve(mymatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mymatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 