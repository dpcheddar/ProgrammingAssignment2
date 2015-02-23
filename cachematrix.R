## Put comments here that give an overall description of what your
## functions do

## creates a special matrix, which is a list containing a function to
## set the matrix, get the matrix, set the inverse of the matrix
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      #m is initially assigned as NULL
      m <- NULL
      
      #sets the matrix (allows for resetting to another matrix)
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      #gets the matrix
      get <- function() x
      
      #sets the inverse (caches in m)
      setinverse <- function(inverse) m <<- inverse
      
      #gets the inverse
      getinverse <- function() m
      
      #creates the special matrix
      #a list of the four functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## computes the inverse of the special matrix created by above function
## if inverse already calculate, then cachesolve will retrieve it from the cache

cacheSolve <- function(x, ...) {

      m <- x$getinverse()
      
      #if m is not NULL
      if(!is.null(m)) {
            #print this message since inverse is cached, and then return the cached inverse
            message("getting cached data")
            return(m)
      }
      
      #calls the get function and assigns the matrix to data
      data <- x$get()
      
      #use the solve function to find the inverse of the matrix in data, 
      #assign the inverse to to m
      m <- solve(data, ...)
      
      #calls the setinverse function on m
      x$setinverse(m)
      m
}
