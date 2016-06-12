## Put comments here that give an overall description of what your
## functions do
## The code is to calculate the inverse of a matrix, and store the value in cache. Everytime I 
## want to get the inverse I check cache first and then decide to calculate or just get the 
## existing value

## Write a short comment describing this function
## makeCacheMatrix function takes a matrix as its argument, and store it in cache. Meanwhile, 
## the inverse also hold a position in cache. All the sub-functions will be returned as a list
## for user to call using "$"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve takes the result of makeCacheMatrix as its argument, and takes advantage of the 
## sub-functions of makeCacheMatrix to decide whether there already exists an inverse of the 
## original matrix. If there is, it simply took the value in cache; if there's not, it calculate
## it instead. At last, the inversed matrix will be returned.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(is.matrix(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
