## This code in R has 2 functions to handle repetitive inverse calculation of 
## a matrix efficiently using cache

## Inverse a matrix and store the inversed one in cache
makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(,0,0)
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinver <- function(solve) m <<- solve
    getinver <- function() m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinver()
    #check if there was a cached matrix. 
    #Since the assignment says that all input matrices are inversable, 
    #checking whether the cached matrix is 0x0 is enough
    if(!(ncol(m)==0)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinver(m)
    m
}
