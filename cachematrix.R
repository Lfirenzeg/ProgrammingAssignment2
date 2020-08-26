## The first function makeCacheMatrix creates a vector to obtain a value of the inverse of a matrix
## The second function cacheSolve checks to see if the inverse has already been calculated, getting it
##    from cache and skipping the computation. Otherwise, it calculates the inverse of the matrix 
## library(MASS) is useful to be able to calculate non squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 ## For the first function, a vector "inv" is initialized as NULL
  set <- function(y){
                 x<<- y 
                 inv<<- NULL
                    }  
  get <- function ()x    ##Create function to get matrix X
  setinv<- function (inverse)inv <<- inverse
  getinv<- function () {
                        inver<- ginv(x)
                        inver%*%x       ##This functions obtains the inverse of the matrix
                        } 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function will get cache data

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)){         ##This will check if inverse is NULL, if it is then it will calculate it
          message("getting cached data")
          return (inv)        ## This is were the inverse would be returned from cache data
    }
    data <- x$get()
    inv <- solve(data, ...)       ##This is were the inverse would be calculated
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
