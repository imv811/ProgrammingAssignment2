##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( x = matrix()){
  inversematrix <- NULL ## make an empty name for the inversematrix
  set <- function(y){
    x <<- y
    inversematrix <<- NULL
  } ## function that sets the input y as the value of the matrix and supersaves as matrix 'x', makes an empty cache of inversematrix
  get <- function() x ## gets the value of the matrix
  setinverse <- function(inverse) inversematrix <<- inverse ## function that supersaves the input into inversematrix
  getinverse <- function() inversematrix ## function that gives the value for inversematrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## list of the 4 functions that were made
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
##Need to run above function first, save output, and use the output as x in the cacheSolve
cacheSolve <- function(x, ...){
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  } ## see if inversematrix is empty, if not then display the message and give inversematrix
  data <- x$get()  ## get the matrix using the get function an save it into data
  inversematrix <- solve(data) ##solve function produces the inverse of matrix 'data'
  x$setinverse(inversematrix) ## run the setinverse function to supersave the inversematrix
  inversematrix ##give the value of inversematrix
}
