#Coursera R Programming Assignment 2: Lexical Scoping
#Caching inverse of a matrix

#Two primary functions:
#   - makeCacheMatrix
#       - This function returns 3 other functions which allows makeCacheMatrix to store a matrix and its inverse created from cacheSolve
#       - Internal functions:
#           - mat <- retrieves original matrix (x)
#           - cachInver <- sets (invert) to inverted matrix passed from cacheSolve
#           - cached <- retrieves inverted matrix
#   - cacheSolve
#       - This function will check and retrieve an inverted matrix from makeCacheMatrix
#       - If there is no inverted matrix, then this function will invert a given matrix and pass it to makeCacheMatrix to hold


#makeCacheMatrix function - caches original and inverted matrix
makeCacheMatrix <- function(x = matrix()){
  
  #x creates an empty matrix
  #initialize invert as an empty variable to eventually store inverted matrix
  invert <- NULL
  
  #Since we don't want to create a new object everytime, we use setNew to flush the cache and assign a new matrix to x in the parent environment
  setNew <- function(k){
    x <<- k
    invert <<- NULL
  }
  
  #returns original cached matrix
  mat <- function() x
  
  #caches the inverted matrix from cacheSolve
  cachInver <- function(solved) invert <<- solved
  
  #returns inverted cached matrix
  cached <- function() invert
  
  #Creates a list object of named functions so that cacheSolve can use them later using $
  list(main = mat, cachInver = cachInver, cached = cached)
}


#cacheSolve function - returns an inverted matrix
cacheSolve <- function(x){
  
  #Retrieves cached matrix from makeCacheMatrix
  argu <- x$cached
  
  #checks if retrieved actually contains a matrix and returns it if it does
  if(!is.null(argu)){
    print("worked")
    return(argu)
  }
  
  #if argu does not contain a matrix, then cacheSolve inverts the original stored matrix
  
  #data retrieves the cached original matrix from makeCacheMatrix
  data <- x$mat()
  #inverts the original matrix and assigns it to argu
  argu <- solve(data)
  #passes the inverted matrix to be cached in makeCacheMatrix
  x$cachInver(argu)
  #returns inverted matrix
  argu
}
