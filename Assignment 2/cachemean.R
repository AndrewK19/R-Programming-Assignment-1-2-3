makeVector <- function(x = numeric()) { #intialization of two objects x & m
        m <- NULL  #m is NULL or stored for later use
        
        #set() function does the same thing as the parent makeVector() function
                set <- function(y) { #assigns y to parent env x, and assigns NULL to m in parent env.
                             #This clears any value of m that had been cached by a prior execution.
                
                x <<- y #<<- assigns the value on the right side of the operator to an object in the 
                        #parent environment named by the object on the left side of the operator.
               
                 m <<- NULL #<<- assigns the value on the right side of the operator to an object in the 
                           #parent environment named by the object on the left side of the operator.
        }
        get <- function() x #x is not defined within get(), retrieved from parent env makeVector() (lexical)
        setmean <- function(mean) m <<- mean #assigns mean input to m in parent env
        getmean <- function() m #utilizes lexical scoping to find the value of m in parent env.
        
        #assigns each get() and set() functions as elements in a list and returns the list to the parent env.
        list(set = set, #gives the name 'set' to the set() function defined above
             get = get, #gives the name 'get' to the get() function defined above
             setmean = setmean, #gives the name 'setmean' to the setmean() function defined above
             getmean = getmean) #gives the name 'getmean' to the getmean() function defined above
} #Naming the list elements is what allows us to use the $ form of the extract operator to access 
#the functions by name rather than using the [[ form of the extract operator.

#the makeVector() is incomplete without the below cachemean() function
cachemean <- function(x, ...) {
        m <- x$getmean() #attempts to retrieve mean passed as functions argument.
                        # It calls the get() function, checks to see if its NULL. 
        if(!is.null(m)) { #if it is not equal to NULL, it returns the valid cached mean to the parent env
                message("getting cached data")
                return(m)
        }
        data <- x$get() #if the !is.null(m) is FALSE, cachemean() get the vector from the input obj
        m <- mean(data, ...)  #calculates the mean(), uses setmean() to set the mean in the input obj,
        x$setmean(m)  #then returns the value of the mean to the parent env by printing the mean obj.
        m
} #cachemean() calculates the mean, and makeVector() stores the new vector mean


#EXAMPLE OF HOW THEY WORK IN REAL TIME
#aVector <- makeVector(1:10)
#aVector$get()               # retrieve the value of x
#aVector$getmean()           # retrieve the value of m, which should be NULL
#aVector$set(30:50)          # reset value with a new vector
#cachemean(aVector)          # notice mean calculated is mean of 30:50, not 1:10
#aVector$getmean()           # retrieve it directly, now that it has been cached
