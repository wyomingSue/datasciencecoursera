

# This function sets and gets the value of a matrix 
# then it sets and gets the INVERSE of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    
    # note: '<<-' assigns a val to an object in a different environment
    x <<-y
    inverse2 <<-NULL
  }
  
  # GET to get the value of the function x
  get <- function () x
  
  
  #set the value of the inverse, 
  setinverse <- function(inverse2) inverse <<- inverse2
  
  #get the value of the inverse    
  getinverse <- function () inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# CacheSolve:  computes the inverse of the matrix created by makeCacheMatrix, 
# first, test if the inverse has been calculated, if so, retrieve the inverse from cache
# if inverse not calculated, inverse is calculated and stored in object p


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # test if inverse is populated, if so, retrieve
  if (!is.null(inverse))  {
    
    return (inverse)
  }
  
  #if inverse not available, calculate it with Solve()
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  return (inverse)
}

 # test

x = rbind(c(20, -40), c(-40, 20))
m = makeCacheMatrix(x)
m$get()
cacheSolve(makeCacheMatrix(x))


