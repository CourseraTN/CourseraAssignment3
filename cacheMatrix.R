#Matrix inverse assignment
#Part 1 of assignment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <- y
    inv <- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}


#Part 2 of assignment
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Currently getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}