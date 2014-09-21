# makeCacheMatrix stores a matrix and a cached value of the inverse of the matrix. Contains the following functions:
# set:      set the value of a matrix
# get:      get the value of a matrix
# setinv:   set the cached inverse matrix
# getinv:   get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv_matrix <- NULL
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv_matrix <<- solve
      getinv <- function() inv_matrix
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}


# cacheSolve returns the inverse of the matrix created by makeCacheMatrix
# if the chached inverse matrix is available, it gets retrieved,
# otherwise, it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
      inv_matrix <- x$getinv()
      if(!is.null(inv_matrix)) {
            message("getting cached inverse matrix")
            return(inv_matrix)
      }
      data <- x$get()
      inv_matrix <- solve(data)
      x$setinv(inv_matrix)
      
      inv_matrix
}
