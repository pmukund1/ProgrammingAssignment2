## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special vector which is
# really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
# The list is just a glorified description of the input x
# it defines a set of fuctions that can be used on x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # <<- makes x a global variable
    m <<- NULL # <<- allows value to be remembered for subsequent calls
               # like static variable in C
  }
  get <- function() x    
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# The following function computes the inverse of the special
# vector created with the above function.
# It first checks if the inverse has already been computed,
# and if it has, it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Note that elements of list can be accesses by $name
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached inverse")
    print(m)
    return(m)
  } 
  else {
    message("computing inv first time ... printing matrix and then its inverse")
    data <- x$get()
    print(data)
    m <- solve(data,...)
    x$setinv(m)
    print(m)
    return(m)
  }
}

doit <- function() {
  # first define the matrix, x
  x <- matrix(1:4,2,2)
  # y is a glorified description of x
  # it defines a set of fuctions that can be used on x
  # the set of functions of x is specified as a list
  y <- makeCacheMatrix(x)
  # the list is passed to cacheSolve which computes inverse
  # the first time and stores it in the "m" variable of y
  z1 <- cacheSolve(y)
  # the second time cacheSolve is called, it looks
  # up the cached variable and returns it
  z2 <- cacheSolve(y)
}
