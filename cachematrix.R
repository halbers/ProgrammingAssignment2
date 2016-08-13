## This function will take in a matrix and return the matric with it's inverse cached. 
## This means the values will be remembered and stored each time this matrix is acessed.

makeCacheMatrix <- function(x = matrix()) {
	xInv <- NULL #set the initial value of xInv to null
	set <- function(y) { #takes in y value
		x <<- y #caches y to x value
		xInv <<- NULL #caches xInv as null	
	}
	get <- function() x #returns x
	setInv <- function(inv) xInv <<- inv #caches inv to xInv
	getInv <- function() xInv #returns xInv
	list(set = set, get = get, setInv = setInv, getInv = getInv) #puts values
}


## This method will compute the inverse. If the inverse has already been calculated it will then it retrieves the invest value
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	xInv <- x$getInv() #gets the x inverse value and sets to xInv
	if(!is.null(xInv)){ #if xInv is not null
		message("getting cached data") #return message
		return(xInv) #returns xInv value
	}
	xInv <- solve(x$get())#solve function takes in (a, b, tol, LINPACK = FALSE, ...)
					#If missing, b is taken to be an identity matrix and solve will return the inverse of a.
					#since only a is given the inverse will be returned 
	x$setInv(xInv) #sets the inverse of x to xInv
	return(xInv) #returns xInv value
}
