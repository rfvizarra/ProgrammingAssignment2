## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # inv will contain the inverse of the matrix
  # set the inverse to NULL
  inv <- NULL
  # define the set function that will set both the values of x an inv
  # in the makeCacheMatrix environment
  set <- function(y) {
    x <<- y
    # when a new matrix is set, the old inverse should be reset
    inv <<- NULL    
  }
  # the get function will return the value of the original matrix
  get <- function() x
  # the setinv function is used to set the value of the inv variable
  # in the makeCacheMatrix environment
  setinv <- function(x) inv <<- x
  # the getinv function is used to get the value of the inv variable
  getinv <- function() inv
  # the following list is what the makeCacheMatrix returns
  # it is composed of four elements, the get, set, setinv and getinv
  # functions defined previously
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # gets the inv variable from x 
  m <- x$getinv()
  if (!is.null(m)) {
    # if m is not null, it means the inverse is cached and we can
    # return it directly
    message("getting cached data")
    return (m)
  }
  # if m is NULL, thet the inverse of the matrix x is not
  # yet cached. The matrix is retrieved and stored in the
  # variable data
  data <- x$get()
  # the inverse of data is calculated and stored in the variable m
  m <- solve(data, ...)
  # the, just calculated, inverse is stored in the x object
  x$setinv(m)
  # and the inverse is returned
  m
}
