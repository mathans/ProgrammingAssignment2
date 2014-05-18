## Caching Inverse of a Matrix to avoid recalculation
## functions do

## Encapsulation for Matrix Variable with set get setinverse getinverse

makeCacheMatrix <- function(x = matrix()) {
  im<- NULL
  set<-function(y){
    x<<-y
    im<-NULL
  }
  get<- function() x
  setinverse<-function(solved) im<- solved
  getinverse<-function() im
  list(set=set, get = get, setinverse= setinverse, getinverse=getinverse)
}


## Validate Is the Inverse is calculated already if so without recalculation return it else calculate it

cacheSolve <- function(x, ...) {
  im<- x$getinverse()
  if(!is.null(im)){
    message("Getting Cached Inverse")
    return(im)
  }
  data<-x$get()
  im<-solve(data, ...)
  x$setinverse(im)
  im
        ## Return a matrix that is the inverse of 'x'
}
