#Coursera R Programming Assignment 2: Lexical Scoping
#Caching inverse of a matrix

#Two primary functions:
#   - makeCacheMatrix
#       - This function returns a set of functions which allows makeCacheMatrix to store a matrix and its inverse created from cacheSolve
#           - mat <- retrieves original matrix (x)
#           - cachInver <- sets (invert) to inverted matrix passed from cacheSolve
#           - cached <- retrieves inverted matrix
#   - cacheSolve
#       - This function will check and retrieve an inverted matrix from makeCacheMatrix
#       - If there is no inverted matrix, then this function will invert a given matrix and pass it to makeCacheMatrix to hold


# makeCacheMatrix function - caches original and inverted matrix
# x creates an empty matrix
# invert is initialized as an empty variable to eventually store inverted matrix
# Since we don't want to create a new object everytime, we use setNew to flush the cache and assign a new matrix to x in the parent environment
# mat returns original cached matrix
# cachInver caches the inverted matrix from cacheSolve
# storedInv returns cached inverted matrix
# Create a list object of named functions so that cacheSolve can use them later
makeCacheMatrix <- function(x = matrix()){
  invert <- NULL
  setNew <- function(newMatrix){
    x <<- newMatrix
    invert <<- NULL
  }
 
  mat <- function() x
  cachInver <- function(solved) invert <<- solved
  storedInv <- function() invert
  
  list(main = mat, storedInv = storedInv, cached = cached)
}


# cacheSolve function - returns an inverted matrix
# argu retrieves cached matrix from makeCacheMatrix   
# the if statement checks if retrieved contains a matrix and returns it if it does, else inverts cached original matrix
# data retrieves the cached original matrix from makeCacheMatrix
# inverts the original matrix and assigns it to argu
# x$cachInver passes the inverted matrix to be cached in makeCacheMatrix
# returns the inverted matrix
cacheSolve <- function(x){
  argu <- x$storedInv
  if(!is.null(argu)){
    print("worked")
    return(argu)
  }
  
  data <- x$mat()
  argu <- solve(data)
  x$cachInver(argu)
  argu
}
