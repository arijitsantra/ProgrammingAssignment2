## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) { ## changes the matrix stored in makeCacheMatrix 
      x <<- y
      matinv <<- NULL
    }
    get <- function() x ## returns matrix x stored in makeCacheMatrix
    setInverse <- function(inverse) matinv <<- inverse ## stores the value of the input in matinv
    getInverse <- function() matinv ## returns the value of matinv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The function catchSolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then catchSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getInverse()
  if (!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  mat <- x$get()
  matinv <- solve(mat, ...) ## solve computes the inverse of the matrix
  x$setInverse(matinv)
  matinv
}
