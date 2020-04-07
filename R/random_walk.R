
# random walk of the matrix to fill
# m est une matrice
# forbidden_cells is a vector containing the xy coordinates of the boxes
# interdites
# toVisit contains the wells in form A1, and contains only the wells authorized to be filled in
# d is the user-supplied dataframe
# groups is the number of distinct groups existing in user data
# constraint is the neighborhood constraint mode chosen by the user
randomWalk <- function(m, toVisit, d, constraint){

  if(class(m) != "matrix"){
    logerror("m is not a matrix, m: %s", class(m))
    warning("Need m to be a matrix")
  }

  visited <- c() # visited wells
  nb_lig <- dim(m)[1]
  nb_col <- dim(m)[2]
  ret = m
  placed = c() # samples already picked up and placed
  # loginfo("nrow(d): %s", nrow(d), logger = "randomWalk")
  while (length(visited) != nrow(d)) {
    # loginfo("length(visited): %s", length(visited), logger = "randomWalk")

    # loginfo("toVisit: %s", toVisit, logger = "randomWalk")

    cell <- resample(toVisit, size = 1)
    visited <- c(visited,cell)
    LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

    i <- as.numeric(match(toupper((stringr::str_extract(cell, "[aA-zZ]+"))), LETTERS702))
    j <- as.numeric(stringr::str_extract(cell, "[0-9]+"))


    # uniformisation de plaque
    test <- solveCell(m=ret,
                      d=d,
                      i=i,
                      j=j,
                      already_drawn = placed,
                      constraint = constraint)
    if(class(test)=="numeric"){
      return(1)
    }else{
      ret <- test$m
      placed <- test$already_drawn
      # we look after the last placed element
      d[which(d$ID == placed[length(placed)]),]$Row <- i
      d[which(d$ID == placed[length(placed)]),]$Column <- j
      toVisit <- toVisit[!toVisit == cell]
    }

  }

  return(d)
}