## makeCacheMatrix and cacheSolve work together to compute and cache the inverse of a matrix
## so it can be retrieved without recomputing 


## makeCacheMatrix can store and retrieve a matrix and its inverse.

## makeCacheMatrix returns a list containing four functions:
##1: 'set'
##2: 'get'
##3: 'setinverse'
##4: 'getinverse' 

## makeCacheMatrix takes an optional input of class 'matrix'. If an input is provided,
## it is stored as the initial matrix value. The matrix value can be set or 
## over-written with the 'set' function.  
## the 'get' function returns the matrix if one is stored. Else it returns a 1X1 NA matrix.
## the 'setinverse' function takes the inverse of a matrix as an argument and stores its value 
## the 'getinverse' function returns the matrix inverse if one is stored. Else it returns a 
## NULL value.

makeCacheMatrix <- function(x = matrix()) 
{
  i<- NULL
  set<-function(y)
  {
    x <<-y
    i <<- NULL
  }
  get<-function() x 
  setinverse<-function(inverse)
    i<<-inverse
  getinverse<-function() i
  list(get=get, set=set,getinverse=getinverse,setinverse=setinverse)
}


## cacheSolve takes as input the list returned from makeCacheMatrix.
## It first checks if a value for the matrix inverse is stored 
## by calling getinverse from the makeCacheMatrix function.
## If a value is stored, it returns that value with the message
##"getting cached data". 
## Else, it retrieves the matrix stored in makeCacheMatrix, 
## computes and returns its inverse, and passes the value to the 
## setinverse function in makeCacheMatrix. 

cacheSolve <- function(x, ...) 
{
  
  i<-x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  
  i
}
