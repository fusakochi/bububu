
  ## function cacheSolve, together with makeCacheMatrix will calculate the inverse of matrix. 
  

 
  ##The function below ceates the list of functions, 
  #1.set - set matrix 2. get- get the value of matrix 3. set inverse - set the inverse of the matrix
  #4. getinverse - get the inverse of the matrix
  
  ##once the inverse of matrix is calculated it stores under "getinverse". 
makeCacheMatrix <- function(x = matrix()){ 
  I <- NULL
  set <- function(y) {
  x <<- y
  I <<- NULL
}
get <- function() x
setinverse <- function(inverse) I <<- inverse
getinverse <- function() I
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}
##The function below calculate the inverse of matrix which is specified by makeCacheMatrix. If the inverse is already stored,
##it returns "getting cached data", indicating the it skips calculation, but retrieves directory from the stored value

cacheSolve <- function(x, ...) {
  
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  
  data <- x$get()
  library(MASS)
  I <- ginv(data, ...)
  x$setinverse(I)
  I
}