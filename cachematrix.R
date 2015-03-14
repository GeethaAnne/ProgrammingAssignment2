
## Below described are functions that do two things, caching and 
## calculating of the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  var_inverse <- NULL
  set <- function(x1) {
    x <<- x1;
    var_inverse<<- NULL;
  }
  get <- function() return(x);
  setinversefunc <- function(inv) var_inverse <<- inv;
  getinversefunc <- function() return(var_inverse);
  return(list(set = set, get = get, setinversefunc = setinversefunc, getinversefunc = getinversefunc))
}


##  The below function will calculate the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been computed with no changes in the matrix, then
## `cacheSolve` will produce the inverse from the cache.

cacheSolve <- function(x, ...) {
  var_inverse <- x$getinversefunc()
  if(!is.null(var_inverse)) {
    message(" cached data retirval in progress")
    return(var_inverse)
  }
  data <- x$get()
  var_inverse <- solve(data, ...)
  x$setinversefunc(var_inverse)
  return(var_inverse)
}
