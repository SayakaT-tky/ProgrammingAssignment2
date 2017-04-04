## These are functions to calculate the inverse of a matrix,
## and once the calculation is done, they cahce it to save a costly computation next time.

## The first function 'makeCahceMatrix' is a function which create a 
## special matrix object as a reference for 'cachesolve' function.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x ## temporal assignment of matrix'x' to be referred
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  M<<-list (set=set, get=get, setinv=setinv, getinv=getinv) ## assigns the list to a temporal object in the parent environment
}


## The second function 'cachesolve' finally returns the inverse of the given matrix,
## while it first tries to find whether there is the cache of the inverse recorded before.

cacheSolve <- function(x, ...) {
        inv<-M$getinv()     ## this expression tries to find if the inverse is cached
        if(!is.null(inv)){  ## this condition checks if the inverse is cached
          message("getting cached data")
          return(inv)       ## returnes cached inverse and get out from the function
        }
        data <-M$get()       ## gets the original matrix data from 'makeCacheMatrix'
        inv<-solve(data,...) ## calculates the inverse
        M$setinv(inv)        ## records the cache
        inv                  ## returnes the inverse of the given matrix
        
}
