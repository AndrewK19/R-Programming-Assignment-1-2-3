###Week 2 Assignment - Creating a makeCacheMatrix function to cache an inputted matrix then pulling this 
##cached matrix into the second function where the cachesolve function inverses the matrix. 

#cachesolve() calculates the inverse, and makeCacheMatrix() stores the new inversed matrix

## Creating a function to cache an inputted matrix

makeCacheMatrix <- function(x = matrix()) { #intialization of two objects x and inv
        inv <- NULL #stored as NULL for later use
        set <- function(y) { #assigns y to parent env x, and assigns NULL to inv in parent env.
                             #This clears any value of m that had been cached by a prior execution.
                x <<- y
                inv <<- NULL
        }
        get <- function() x #x is not defined within get(), retrieved from parent env makeVector() (lexical)
        setInverse <- function(inverse) inv <<- inverse #assigns inverse input to inv in parent env
        getInverse <- function() inv #utilizes lexical scoping to find the value of inv in parent env.
        
        #assigns each get() and set() functions as elements in a list and returns the list to the parent env.
         list(set = set, #gives the name 'set' to the set() function defined above
              get = get, #gives the name 'get' to the get() function defined above
             setInverse = setInverse, #gives the name 'setInverse' to the setInverse() function defined above
             getInverse = getInverse) #gives the name 'getInverse' to the getInverse() function defined above
         
}#Naming the list elements is what allows us to use the $ form of the extract operator to access 
#the functions by name rather than using the [[ form of the extract operator.

# Attempts to retrieve a previously cached matrix, if inv = NULL, then the inputted matrix arugment
# is inversed and cached as a new matrix of inv

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() #attempts to retrieve matrix passed as functions argument.
                              # It calls the get() function, checks to see if its NULL. 
        if(!is.null(inv)) {#if it is not equal to NULL, it returns the valid inversed matrix to the parent env
                message("getting cached data")
                return(inv) 
        }
        data <- x$get() #if the !is.null(inv) is FALSE, makeCacheMatrix() gets the matrix from the input obj
        inv <- solve(data, ...) #calculates the solve(), uses setInverse() to set the matrix in the input obj,
        x$setInverse(inv) #then returns the value of the inverse to the parent env by printing the inverse obj.
        inv
}
