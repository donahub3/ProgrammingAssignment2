## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Initalizes the Matrix object used in the cachedSolve function 
makeCacheMatrix <- function(x = matrix()) {##sets x default to empty matrix
  m<-NULL ##Clears value in m
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x #gets value x
  setinv<-function(solve) m<<-solve #sets inverse
  getinv<-function() m #gets inverse
  
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##Solves for the inverse of a matrix using the makeCachedMatrix value
cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  data=matrix()
  data<-x$get()
  m<- solve(a=data)
  x$setinv(m)
  m
  
  
  ## Return a matrix that is the inverse of 'x'
}
