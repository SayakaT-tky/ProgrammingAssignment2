## A set of functions below caches the inverse of a matrix to avoid a costly computation.

## The function below creates a set of functions or matrices for reference from the next function.Used together with the next function, this can cache its inverse.
## The argument x here is supposed always to be a square invertible matrix
## The list returned is consisted of functions to (1)  set the matrix, (2) get the matrix, (3) get the inverse, (4) get the inverse.
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
   x<<-y
   inv<<-NULL
 }
 get<-function() x
 setinv<-function(inverse) inv<<-inverse
 getinv<-function() inv
 cacheM<-list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Next function cachesolve calculates a inverse of a matrix. It first evaluate whether a cashe of the calculation has been saved. If the cache exsits, the function below retrieves the inverse from the cache, but if it doesn't, then function computes the inverse. 
cacheSolve <- function(x, ...) {
        inv<-cacheM$getinv()
        if(!is.null(inv)){
          message("getting chached data")
          return(inv)
        } ## above expressions get the inverse matrix from the cache if it's available and exit from the function
        calcM<-cacheM$get()
        inv<-solve(calcM,...)
        cacheM$setinv(inv) ## this expression assigns the result of the newly calculated inverse to hte cache
        inv
}



