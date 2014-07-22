## these two functions allow to undestand scoping in R



## This function creates a special "matrix" object that can cache its inverse




makeCacheMatrix <- function(x = matrix()) {
  
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

###testing
#matrix <- rbind(c(4, 3), c(3, 2)) 
#m <- makeCacheMatrix(matrix)

# m$get()
#[,1] [,2]
#[1,]    4    3
#[2,]    3    2
# cacheSolve(m)
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
