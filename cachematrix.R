## cache inverse matrix

## The first function, makecachematrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##get the value of the matrix
  get <- function() x
  
  ##set the value of the inversed matrix
  setinverse <- function(myinverse) m <<- myinverse
  
  ##get the value of the inversed matrix
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve calculates the inversed matrix of the special "matrix" created with the above function. 
##However, it first checks to see if the inversed matrix has already been calculated. 
##If so, it gets the inversed matrix from the cache and skips the computation. 
##Otherwise, it calculates the inversed matrix of the data and sets the value of
##the inversed matrix in the cache via the setinverse function.

cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
  return(m)
}

##test functions

a <- makeCacheMatrix()                 #initialize
a$set(matrix(1:4,2,2))                 #set the matrix
a$get                                  #get the matrix
cacheSolve(a)                          #calculate the inversed matrix 
cacheSolve(a)                          #when is called back use the cached inversed matrix