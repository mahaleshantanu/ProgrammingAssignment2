## Function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()){
  mat_inverse <- NULL
  set <- function(y){
              x <<- y
              mat_inverse <<- NULL
  }
  
  get <- function(){
    x
    }
  set_inverse <- function(cal_inverse) {
    mat_inverse <<- cal_inverse
    }
  get_inverse <- function() {
    mat_inverse
  }
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## Function cacheSolve
cacheSolve <- function(x, ...){
   sol_inv<- x$get_inverse()
  
  if(!is.null(sol_inv)){
    message("Getting Cache Data")
    return(sol_inv)
  }
  
  mat_data <- x$get()
  sol_inv <- solve(mat_data, ...)
  x$set_inverse(sol_inv)
  sol_inv
}
