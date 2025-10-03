## Matrix inversion is usually a costly computation; cached inverse object can 
## speed up the computing and save the system resource

## Function to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invobj <- NULL
  
  # store the new matrix input to and clear cached inverse
  set <- function(pm) {
    x <<- pm
    invobj <<- NULL
  }
  
  # get the stored matrix
  get <- function() x
  
  # store cached inverse object
  setInverse <- function(inverse) invobj <<- inverse
  
  # get stored cached inverse object
  getInverse <- function() invobj
  
  # return as list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function to compute the inverse, or return cached inverse

cacheSolve <- function(x, ...) {
  invobj <- x$getInverse()
  ## check if cached inverse object exist
  if(!is.null(invobj)) {
    message("Inverse is not null, getting cached data- after the 1st run")
    return(invobj)
  }
  
  matdata <- x$get()
  ## get inverse result
    cat("calculating the matrix inverse, show this message at the 1st run\n")
  invobj <- solve(matdata, ...)
  x$setInverse(invobj)
  invobj
}

##test to run the functions above
MA <- matrix(c(2.5,3.2,2,1,6,4.6,5.5,5,2),3,3)

#wrap it with caching object
CMA <- makeCacheMatrix(MA)

##1st call - compute and return inverse
cacheSolve(CMA)

##2nd call - return the same cached version with four decimals
print(cacheSolve(CMA), digits = 4)
