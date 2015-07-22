## Make an inverse caching matrix

## Create the cacheMatrix structure/class with get, set, getInverse
##  and setInverse methods
makeCacheMatrix <- function(inputMatrix = matrix())
{
  # Validate inputMatrix is a square matrix
  if(!ValidateMatrix(inputMatrix)) return("Error: Input not a square matrix");
  
  matrixInverse <- NULL
  
  # set the inputMatrix and set inverse to NULL
  set <- function(mtx)
  {
    inputMatrix <<- mtx
    matrixInverse <<- NULL
  }
  
  # get retuns the input matrix
  get <- function() inputMatrix
  
  # setInverse sets the matrix inverse
  setInverse <- function(inverse) matrixInverse <<- inverse
  
  # getInverse returns the matrix inverse
  getInverse <- function() matrixInverse
  
  # Setup methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse);
}

## Returns any prievousally calculated inverse from inputMatrix
##  or calculates, caches and returns the inverse.
cacheSolve <- function(inputMatrix, ...)
{
  # Check if the input has a cached inverse
  inverse <- inputMatrix$getInverse();
  
  if(!is.null(inverse))
  {
    message("Getting cached data");
    return(inverse);
  }
  
  # Calculate the inverse
  message("Calculating Inverse");
  matrixData <-inputMatrix$get();
  inverse <- solve(matrixData, ...);
  
  # Cache and return the inverse
  inputMatrix$setInverse(inverse);
  
  return(inverse);
}


## ValidateMatrix is used to check that the matrix is square
ValidateMatrix <- function(inputMatrix)
{
  nRow <- dim(inputMatrix)[1];
  nCol <- dim(inputMatrix)[2];
  
  return(!is.null(nRow) && !is.null(nCol) && nRow == nCol);
}