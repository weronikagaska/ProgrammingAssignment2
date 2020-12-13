## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function calculates matrix invert and saves it to cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#See if that works:
#create a matrix
matr<-matrix(c(1,-7,2,-1,4,0,9,5,0,-3,2,2,-5,7,3,8),4,4)
r<-makeCacheMatrix(matr)
cacheSolve(r)
