#makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
#set the elements of the matrix
#get the elements of the matrix
#set the elements of the matrix inverse
#get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##calculates the inverse of the special “matrix” and 
#sets it in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
  }

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()

my_Matrix$getinverse()

cacheSolve(my_Matrix)
my_Matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
my_Matrix$getinverse()
