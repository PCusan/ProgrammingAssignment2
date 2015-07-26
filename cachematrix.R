# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a matrix and a cached value of the inverse of the 
# matrix.

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - setinvmat the value of the inverse matrix
##   - getinvmat the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
            x <<- y 
            c <<- NULL        
    }
    get <- function() x
    setinvmat <- function(invmat) c <<- invmat
    getinvmat <- function() {c}
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
            
}

# This function first checks if the inverse matrix is in the cache.
# If it is not in the memory it will use solve() to invert the matrix.

  cacheSolve <- function(x, ...) {
           c <- x$getinvmat()
           if(!is.null(c)) {
                  message("getting cached data")
                  return(c)
           }
          data <- x$get()
          c <- solve(data, ...)
          x$setinvmat(c)
          c
  }
    