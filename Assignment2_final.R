# this function will return a list of functions which defined in makeCacheMatrix
makeCacheMatrix <- function(x = matrix()){# input a matrix variable
	i <- NULL # the index of cache
	set <- function(y){
		x <<- y     ## when a new matrix is set
		i <<- NULL  ## the cache will be set as null
  }
	get <- function() x # return the input matrix
	setinverse <- function(inverse) i <<- inverse # set i as inverse
	getinverse <- function() i # get i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # all elements in this list are functions defined above
}

# this function will either return either the cache (if there is a one)
# or inverse the matrix, assign to the cache then return the inversed matrix (if no cache)
cacheSolve <- function(x, ...){# the input value must be a "makeCacheMatrix"
	i <- x$getinverse() # get the index of cache
	if(!is.null(i)){                    ## if the index is not null
		message("getting cached data")    ## print a message
		return(i)                         ## and return the i, then break the function
	}
	data <- x$get()                     ## otherwise, assign the matrix to "data"
	i <- solve(data, ...) ## inverse the matrix
	x$setinverse(i)       ## and get the cache
	i                     ## then return the inversed matrix
}
