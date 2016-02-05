#These two functions cache and return the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL 
  }
 
  get <- function() x

  setinv <- function(solve) mat <<- solve
  getinv <- function() mat
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}





#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
        ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {

  mat <- x$getinv()
 
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  data <- x$get()
  mat <- solve(data, ...)
  x$setinv(mat)
  
  mat
}


#example
# > testmat <- matrix(10:13,2,2)
# > testmat
# [,1] [,2]
# [1,]   10   12
# [2,]   11   13

# > cache <- makeCacheMatrix(testmat)
# > cacheSolve(cache)
# [,1] [,2]
# [1,] -6.5    6
# [2,]  5.5   -5
# [2,]  5.5   -5