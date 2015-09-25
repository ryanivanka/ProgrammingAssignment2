## cachematrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- matrix(1:16, nrow=4, ncol=4)
##  matrixStorage <- makeCacheMatrix()$set(M)
##  cacheSolve(matrixStorage)
##  matrixStorage$getInverse() # Return the inverse matrix
##
##  N <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  matrixStorage$set(N)      # Set new matrix, clean the cache
##  matrixStorage$get()  # Returns the matrix being cached, it's N
##  matrixStorage$getInverse() # Return null

makeCacheMatrix <- function() {
  x <- NULL
  cachedInverse <- NULL
  
  ## Original matrix setter and getter,  inverse matrix reset to null
  set <- function(y) {
    if (is.matrix(y) && nrow(y) == ncol(y) && det(y)) {
      ## cache it
      x <<- y
      cachedInverse <<- NULL    
    } else {
      stop("Validation Error: This matrix is not a squared matrix. It cannot be inversed.")
    }
  }
  get <- function() x
  
  ## Inverse matrix setter and getter
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object
cacheSolve <- function(makeCacheMatrixInterface, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- makeCacheMatrixInterface$getInverse()
  
  ## Check if 
  if(!is.null(inverseMatrix)) {
    message("Use cached inverse matrix.")
    return (invFunc)
  }
  
  ## Calculate inverse matrix
  originalMatrix <- makeCacheMatrixInterface$get()
  inverseMatrix <- solve(originalMatrix)
  
  ## Cache inverse matrix
  makeCacheMatrixInterface$setInverse(inverseMatrix)
  
  ## Return inverse matrix
  inverseMatrix
}

