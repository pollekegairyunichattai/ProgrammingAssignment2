
## makeCacheMatrix will store a matrix in its variable X
## it provides set and get functions for changing the matrix in x
## it provides setinv and getinv functions for manipulating the inverse of the matrix, stored in inv
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve takes a list made with makeCacheMatrix, calculates the inverse of the matrix object in the list
## if it doesn't already exists, if it calculates the inverse it stores it in the list
## if the inverse was calculated it gets printed
## if the inverse wasn't calculated but already cached in the list, a message 'getting cached data' is printed before the inverse
## if no inverse exists (solve throws an error), leave the inverse as NULL and print a message saying the inverse doesn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(nrow(x$get())==ncol(x$get())) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- tryCatch(solve(data, ...), error = function(cond) {
      message("No inverse exists")
      NULL
      }
    ) 
    x$setinv(inv)
    inv
  }
  else {
    message("matrix is not symmetrical, inverse can't be calculated")
  }
}
