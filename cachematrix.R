## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

     invMat <- NULL
     
     setMatrix <- function(mat){
          x <<- mat
          invMat <<- NULL
     }
     
     getMatrix <- function(){
          x
     }
     
     calculateMatInverse <- function(){
          invMat <<- solve(x)
          
     }
     
     setInverseMatrix <- function(inv){
          invMat <<- inv
     }
     
     list(setMatrix = setmatrix, 
          getMatrix = getMatrix,
          calculateMatInverse = calculateMatInverse,
          setInverseMatrix = setInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getInv()
     if(!is.null(inv)){
          print("getting cached data...")
          return(inv)
     }
     data <- x$getMatrix()
     inv <- x$calculateMatInverse()
     
     x$setInverseMatrix(inv)
     inv
}


