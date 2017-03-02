#Coursera R Programming Assignment 2: Lexical Scoping
#Caching inverse of a matrix

#Two primary functions:
#   - makeCacheMatrix
#       - This function returns a set of functions that allows makeCacheMatrix to store a matrix and its inverse created from cacheSolve
#           - setNew <- flushes the cache and assign a new matrix to x while not creating a new object everytime
#           - mat <- retrieves original matrix (x)
#           - cachInver <- sets (invert) to inverted matrix passed from cacheSolve
#           - cached <- retrieves inverted matrix
#   - cacheSolve
#       - This function will check and retrieve an inverted matrix from makeCacheMatrix
#       - If there is no inverted matrix, then this function will invert a given matrix and pass it to makeCacheMatrix to hold


# makeCacheMatrix function - caches original and inverted matrix
# invert is initialized as an empty variable to eventually store inverted matrix
# also creates a list object of named functions so that cacheSolve can use them later
makeCacheMatrix <- function(x = matrix()){
  invert <- NULL
  setNew <- function(newMatrix){
    x <<- newMatrix
    invert <<- NULL
  }
 
  mat <- function() x
  cachInver <- function(solved) invert <<- solved
  storedInv <- function() invert
  
  list(mat = mat, storedInv = storedInv, cached = cached)
}


# cacheSolve function - returns an inverted matrix
# argu retrieves cached matrix from makeCacheMatrix   
# the if statement checks if retrieved contains a matrix and returns it if it does, else inverts cached original matrix
# data retrieves the original matrix from makeCacheMatrix
# inverts the original cached matrix and assigns it to argu
# x$cachInver passes the inverted matrix to be cached in makeCacheMatrix
# function lastly returns the inverted matrix
cacheSolve <- function(x){
  argu <- x$storedInv
  if(is.null(argu)){
    data <- x$mat()
    argu <- solve(data)
    x$cachInver(argu)
    return(argu)
  }
  
  argu
}
