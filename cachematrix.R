## makeCacheMatrix() : takes the matrix object x as parameter. 
#### 1. setMatrix() : takes matrix object and sets x equal to mat
#### 2. getMatrix() : returns the matrix object x
#### 3. calculateMatInverse() : calculates inverse of matrix x and caches in invMat
#### 4. 
## functions do

## makeCacheMatrix() : takes the matrix object x as parameter. 

makeCacheMatrix <- function(x = matrix()) {

     invMat <- NULL
     
     ## setMatrix() : takes matrix object and sets x equal to mat
     setMatrix <- function(mat){
          x <<- mat
          invMat <<- NULL
     }
     
     ## getMatrix() : returns the matrix object x
     getMatrix <- function(){
          x
     }
     
     ## calculateMatInverse() : calculates inverse of matrix x and caches in invMat
     calculateMatInverse <- function(){
          invMat <<- solve(x)
          invMat
     }
     
     getInverseMatrix <- function(){
          invMat
     } 
     
     setInverseMatrix <- function(inv){
          invMat <<- inv
     }
     
     list(setMatrix = setMatrix, 
          getMatrix = getMatrix,
          calculateMatInverse = calculateMatInverse,
          getInverseMatrix = getInverseMatrix,
          setInverseMatrix = setInverseMatrix)
}


## 

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverseMatrix()
     if(!is.null(inv)){
          print("getting cached data...")
          return(inv)
     }
     data <- x$getMatrix()
     inv <- x$calculateMatInverse()
     
     x$setInverseMatrix(inv)
     inv
}


