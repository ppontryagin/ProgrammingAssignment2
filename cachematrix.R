makeCacheMatrix <- function(x = numeric()) {
  
  cache <- NULL
  
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  getMatrix <- function() {
    x
  }
  
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  getInverse <- function() {
    cache
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

cacheSolve <- function(y, ...) {
  
  invMatrix <- y$getInverse()
  if(!is.null(invMatrix)) {
    message("cache hit")
    return(invMatrix)
  }

  matrix <- y$getMatrix()
  invMatrix <- solve(matrix)
  y$cacheInverse(invMatrix)
  
  invMatrix
}