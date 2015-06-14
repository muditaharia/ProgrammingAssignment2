#This is a program designed to find the inverse of a matrix. 
#If the inverse of the matrix has already been computed once, 
#then this program caches the result, and retrieves it from the memory, rather 
#than re-computing the result


#The makeCacheMatrix function takes in the original matrix, 
#finds inverse and saves it to memory
#It contains get, set, getinverse and setinverse functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The cacheSolve function takes in a matrix, 
#and checks to see if its inverse has already been computed. 
#If it has been computed, then it retrieves the solution from the memory
#Otherwise, it computes the inverse, and stores the solution to the memory
#This function uses the functions defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  return (m)
}
