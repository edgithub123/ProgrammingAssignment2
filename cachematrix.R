#######################################################################
# The first function produces an object that contains the matrix, but also the functions that set and get the matrix.
#also this function sets and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  Q <- NULL
  set <- function(y) {
    x <<- y
    Q <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) Q <<- solve
  getinverse <- function() Q
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function calculates the inverse Q of matrix x, or gets it in the cached part of the object produced by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Q <- x$getinverse()
  if(!is.null(Q)) {
    message("getting cached data")
    return(Q)
  }
  data <- x$get()
  Q <- inv(data, ...)
  x$setinverse(Q)
  Q
}


#####################################################################################
#here I will test the functions developed. I firstly produce a matrix A. I "get" this matrix.
A <- makeCacheMatrix(matrix( c(2, 1, 0, 1,-1, 1, 1, 0,-1), nrow=3, byrow=TRUE))
A$get()
#Then I "get" the inverse of the matrix which doesn't exist in the object...yet.
A$getinverse()
#Then I change the matrix using the "set" function in the object
A$set(matrix( c(1, 1, 0, 1,-1, 1, 1, 0,-1), nrow=3, byrow=TRUE))
#I solve the inverse of this newly set matrix...
cacheSolve(A)
#I repeat this: this time no calculation because the function takes the cached matrix in the obejct.
cacheSolve(A)
#I double check that the inverse is indeed in the object.
A$getinverse()
