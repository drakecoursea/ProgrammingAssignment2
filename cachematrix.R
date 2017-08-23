## ANSWERS BY DRAKECOURSERA

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## This assignment aims to write a pair of functions which can cache the inverse
## of a matrix.


## makeCacheMatrix function:
# This function creates a special "matrix" object that can cache its inverse.
# A list is created containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function:
# This function computes the inverse of the special "matrix"returned by the
# makeCacheMatrix function.
# However, it first checks to see if the inverse has already been calculated
# (and the matrix has not changed).
# If yes, it retrieves the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of
# the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}


####### Trial example #######

# > x <- matrix(1:4,2,2)

# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > solve(x)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > m <- makeCacheMatrix(x)

# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > y <- matrix(5:8,2,2)

# > y
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8

# > solve(y)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

# > m <- makeCacheMatrix(y)

# > cacheSolve(m)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5