
## Functions for spatial contraints: the North, East, West and South neighbors
## for the visited box
findNEWSneighbors <- function(m, i, j){

  #North
  N <- tryCatch(
    {
      m[i - 1,j]
    },
    error = function(err1){
      return(NA)
    }
  )

  #East
  E <- tryCatch(
    {
      m[i,j + 1]
    },
    error = function(err2){
      return(NA)
    }
  )

  # West
  W <- tryCatch(
    {
      m[i,j - 1]
    },
    error = function(err3){
      return(NA)
    }
  )

  # South
  S <- tryCatch(
    {
      m[i + 1,j]
    },
    error = function(err4){
      return(NA)
    }
  )

  ret <- c(N,E,W,S)
  return(ret)
}

findNSneighbors <- function(m, i, j){

  N <- tryCatch(
    {
      m[i - 1,j]
    },
    error = function(err1){
      return(NA)
    }
  )

  S <- tryCatch(
    {
      m[i + 1,j]
    },
    error = function(err4){
      return(NA)
    }
  )

  ret <- c(N,S)
  return(ret)
}

findWEneighbors <- function(m, i, j){
  # for right neighboor
  E <- tryCatch(
    {
      m[i,j + 1]
    },
    error = function(err2){
      return(NA)
    }
  )

  # For left neighboor
  W <- tryCatch(
    {
      m[i,j - 1]
    },
    error = function(err3){
      return(NA)
    }
  )

  ret <- c(W,E)
  return(ret)
}

checkConstraints <- function(m, row, col, mode){

  if (mode == "NS") {
    neighbors <- findNSneighbors(m,row,col)
  }else if (mode == "WE") {
    neighbors <- findWEneighbors(m, row, col)
  }else if (mode == "NEWS") {
    neighbors <- findNEWSneighbors(m, row, col)
  }else{
    logging::logerror("The mode provided is not correct")
  }
  return(neighbors)
}
