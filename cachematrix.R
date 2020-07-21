## Write a short comment describing this function
#This function returns a list of functions that along with the cacheSolve will return the inverse of a square matrix
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL  #Everytime you introduce an argument here the variable m is reset to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  # The argument solve is the inverse of the matrix, that means m now is equal to the inverse of the matrix and because of the <<- operator the value is saved in the local environment of this function and now this is called instead of m <- NULL
  getinverse <- function() m #This is called when you write the same argument in the function cacheSolve, if that's the case m is now equal to the inverse of the matrix
  list(set = set, get = get,    
       setinverse = setinverse,
       getinverse = getinverse)
  #This function returns a list with 4 functions that along with cacheSolve will return the inverse of
  #the matrix introduced as an argument
}


## Write a short comment describing this function
#This function returns the inverse of a square matrix, but also if you enter the same argument it just returns the result that is already stored in m.
#In other words it only calculates the result once and then if the input is the same it returns the result stored in the cache 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()    
  if(!is.null(m)) {   # This only activates if m is not NULL anymore. In other words, It only activates if you have already introduce the same argument
    message("getting cached data")   #If that's the case it just shows this message and return the value saved in m
    return(m)
  }
  data <- x$get()  # Otherwise, it gets the matrix you introduce as argument in makeCacheMatrix and assign the value to the variable data
  m <- solve(data, ...)  # Here it applies de function solve and get the inverse of the square matrix and save it to the variable m
  x$setinverse(m) #Then it gives this variable as an argument for the function setinverse so it can be saved to the variable m thus m is no more NULL
  m #It returns m
}
