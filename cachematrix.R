## Dada una matriz, la dejemos en cache utilizando "matrix<-makeCacheMatrix(matrix)".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            ## inicia la variable
  set <- function(y) {   ##set: PONE
    x <<- y
    inv <<- NULL
  }
  get <- function() x    ##get: LLAMA
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Hallamos su inversa en Cache tambien con "cacheSolve(matrix)"

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

## Se ejecuta asi:
> matrix<-matrix(c(2,-3,0,-2,1,5,-3,5,-1), nrow=3, ncol=3)
> matrix<-makeCacheMatrix(matrix(c(2,-3,0,-2,1,5,-3,5,-1), nrow=3, ncol=3))
> matrix$get()
> cacheSolve(matrix)
##
