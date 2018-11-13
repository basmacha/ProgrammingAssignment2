## Put comments here that give an overall description of what your
## functions do

## From assignment: "creates a special "matrix" object that can cache its inverse" 

makeCacheMatrix <- function(x = matrix()) {
  z<-NULL
  set<function(y){
    x<<-y
    z<<-NULL
  }
  get<-function()x
  setsolve<-function (solve) z<<-solve
  getsolve<-function()z
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## from assignment:"...computes the inverse of the special "matrix" returned by makeCacheMatrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z<-x$getsolve()
  if(!is.null(z)){
    return(z) 
  }
  data<-x$get()
  z<-solve(data,...)
  x$setsolve(z)
  z
}
