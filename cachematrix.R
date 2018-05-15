## Week 3 R Programming Assignment 2

## Caching the inverse of a matrix

## Caching saves on computing power and can be used repeatedly

## This function creates a special matrix that can cache its inverse by:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix from the
## makeCacheMatrix function above by:
## 1. It first checks to see if the inverse has already been computed
## 2. If yes, it skips the computation
## 3. If no, it computes the inverse from the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## testing makeCacheMatrix and cacheSolve

matrix <- matrix(1:4, 2,2)
matrix1 <- makeCacheMatrix(matrix)
cacheSolve(matrix1)



