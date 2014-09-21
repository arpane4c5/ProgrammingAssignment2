## makeCacheMatrix() function takes the matrix object and calculates the
## inverse of the matrix. The sub-functions provided in makeCacheMatrix()
## are used for the following operations:
## 1. set the matrix values
## 2. get the matrix values
## 3. calculate and return the inverse of the matrix
## 4. set the inverse matrix object
## 5. get the inverse matrix object
##

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
     ## It returns the calculated inverse of the matrix
     calculateMatInverse <- function(){
          invMat <<- solve(x)
          invMat
     }
     
     ## getInverseMatrix() : It returns the inverse matrix object
     getInverseMatrix <- function(){
          invMat
     } 
     
     ## setInverseMatrix() : It sets the inverse matrix object equal to inv
     setInverseMatrix <- function(inv){
          invMat <<- inv
     }
     
     list(setMatrix = setMatrix, 
          getMatrix = getMatrix,
          calculateMatInverse = calculateMatInverse,
          getInverseMatrix = getInverseMatrix,
          setInverseMatrix = setInverseMatrix)
}


## cachSolve function used to cache the value of the inverted matrix by 
## using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
     ## get the value of the inverse of matrix from makeCacheMatrix
     inv <- x$getInverseMatrix()
     
     ## if the value is not null then return the cached value of inverse
     if(!is.null(inv)){
          print("getting cached data...")
          return(inv)
     }
     
     ## if the value is not cached then calculate the inverse of matrix
     data <- x$getMatrix()
     inv <- x$calculateMatInverse()

     ## set the value of the inverse matrix 
     x$setInverseMatrix(inv)
     
     ## Return a matrix that is the inverse of 'x'
     inv
}


