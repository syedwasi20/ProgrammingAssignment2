## Put comments here that give an overall description of what your
## functions do
#This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,

# get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
#The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

####Test 2 [2*2 Matrix] #####
TestMatrix <- matrix(c(1,5,8,2),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 3 [3*3 Matrix] --> singulat Matrix #####
#matrix(1:9,3,3) is not possible (singule matrix) because it is giving det(A)  = 0
#matrix = 1/det(A)[3,-6,3,-6,12,-6,3,-6,3] |det(A) = 1/(1*3 +4* (-6) + 7 *3) = 1/0
 TestMatrix <- matrix(1:9,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

####Test 4 [3*3 Matrix]#####
TestMatrix <- matrix(1:8,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 5 [4*4 Matrix]#####
TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)
