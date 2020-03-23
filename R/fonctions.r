
## Functions for contraints
# Spatial constraint for the North, East, West and South neighbors for the box visited
neighborhoodNEWS <- function(m, i, j){

  #North
  N <- tryCatch(
    {
      m[i-1,j]
    },
    error = function(err1){
      return(NA)
    }
  )

  #East
  E <- tryCatch(
    {
      m[i,j+1]
    },
    error = function(err2){
      return(NA)
    }
  )

  # West
  W <- tryCatch(
    {
      m[i,j-1]
    },
    error = function(err3){
      return(NA)
    }
  )

  # South
  S <- tryCatch(
    {
      m[i+1,j]
    },
    error = function(err4){
      return(NA)
    }
  )

  ret <- c(N,E,W,S)
  return(ret)
}

neighborhoodNS <- function(m, i, j){

  N <- tryCatch(
    {
      m[i-1,j]
    },
    error = function(err1){
        return(NA)
    }
  )

  S <- tryCatch(
    {
      m[i+1,j]
    },
    error = function(err4){
      return(NA)
    }
  )

  ret <- c(N,S)
  return(ret)
}

neighborhoodWE <- function(m, i, j){
  # for right neighboor
  E <- tryCatch(
    {
      m[i,j+1]
    },
    error = function(err2){
      return(NA)
    }
  )

  # For left neighboor
  W <- tryCatch(
    {
      m[i,j-1]
    },
    error = function(err3){
      return(NA)
    }
  )

  ret <- c(W,E)
  return(ret)
}

checkConstraints <- function(m, row, col, mode){

  if(mode == "NS"){
    neighbors <- neighborhoodNS(m,row,col)
  }else if(mode=="WE"){
    neighbors <- neighborhoodWE(m, row, col)
  }else if(mode=="NEWS"){
    neighbors <- neighborhoodNEWS(m, row, col)
  }else{
    logerror("The mode provided is not correct")
  }
  return(neighbors)
}


resample <- function(x, ...) x[sample.int(length(x), ...)]


solveCell <- function(m, d, i, j, already_drawn, constraint){

  if(class(m) != "matrix"){
    logerror("m is not a matrix, m: %s", class(m))
    warning("Need m to be a matrix")
  }
  if(class(d) != "data.frame"){
    logerror("d is not a dataframe, d: %s", class(d))
    warning("Need d to be a dataframe")
  }


  # we look at which individuals are neighbors of the current box
  if(constraint != "none"){
    neighbors <- checkConstraints(m, row=i, col=j, mode=constraint)
  }else{
    neighbors <- c(NA,NA,NA,NA)
  }

  if("Group" %in% colnames(d)){

    # identify which group the neighbors belong to in order to obtain a reduced
    # list of possibilities of groups for the current cell to fill
    forbidden_groups <- unique(d$Group[which(d$Sample.name %in% neighbors)])
    possible_groups <- levels(d$Group)[which(!levels(d$Group) %in% forbidden_groups)]


    if(length(possible_groups)==0){
      #there are no more possibilities
      return(1)
    }else{
      # only take in individuals belonging to the possible groups
      # and who are not in already_drawn
      possible_ind <- d$Sample.name[which(d$Group %in% possible_groups)]
      available_ind <- d$Sample.name[which(d$Sample.name %in% possible_ind & !(d$Sample.name %in% already_drawn))]

      if(length(available_ind)==0){
        #there are no more possibilities
        return(1)
      }else{
        # use resample because this function also works as expected when there is
        # only one element in the set to be sampled.
        chosen_ind <- resample(available_ind,size=1)
        m[i,j] <- chosen_ind
        already_drawn <- c(already_drawn,chosen_ind)
      }
    }
  }else{ # if there are no groups in the dataframe

    available_ind <- d$Sample.name[which(!(d$Sample.name %in% already_drawn))]
    # use resample because this function also works as expected when there is
    # only one element in the set to be sampled.
    chosen_ind <- resample(available_ind,size=1)
    m[i,j] <- chosen_ind
    already_drawn <- c(already_drawn,chosen_ind)
  }


  return(list("m" = m, "already_drawn" = already_drawn))
}


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
    i <- as.numeric(match(toupper(substr(cell, 1, 1)), LETTERS))
    j <- as.numeric(substr(cell, 2, 5))
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
      d[which(d$Sample.name == placed[length(placed)]),]$Row <- i
      d[which(d$Sample.name == placed[length(placed)]),]$Column <- j
      toVisit <- toVisit[!toVisit == cell]
    }

  }

  return(d)
}

# Function generating a plate map according to the input parameters
# user_df      : dataframe [Sample.name, Group, Well, Status, Row, Column]
# nb_rows      : integer (number of lines on the plate)
# nb_cols      : integer (number of columns on the plate)
# df_forbidden : dataframe [Sample.name, Group, Well, Status, Row, Column]
# mod          : character (neighborhood spatial constraint)
# max_it       : integer (maximum number of attempts to generate a plate plan before
#                returning a failure.)
generateMapPlate <- function(user_df, nb_rows, nb_cols, df_forbidden, mod, max_it, updateProgress = NULL){

  nb_attempts = 1
  ret=1

  while (ret==1 & nb_attempts <= max_it) {

    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("attempt number", nb_attempts)
      updateProgress(detail = text)
    }

    mat = matrix(NA,nrow=nb_rows, ncol=nb_cols)

    # Generate all the cells that are allowed to be filled
    toVisit <- NULL
    for(i in LETTERS[1:nb_rows]){
      for (j in 1:nb_cols){
        toVisit <- c(toVisit, paste0(i,j))
      }
    }


    if(!is.null(df_forbidden$Well)){
      toVisit <- toVisit[!toVisit %in% df_forbidden$Well]
    }


    if(length(toVisit) < nrow(user_df)){
      return(0)
    }

    ret <- randomWalk(m = mat,
                      toVisit = toVisit,
                      d = user_df,
                      constraint = mod
                      )

    if(class(ret)=="data.frame"){
      ret$Well <- paste0(LETTERS[ret$Row], ret$Column, sep = "")
      ret <- rbind(ret, df_forbidden)
      logwarn("number of attempts: %d", nb_attempts,
              logger = "fonctions.generateMapPlate")
      return(ret)
    }

    nb_attempts = nb_attempts + 1
  }
  logwarn("we reeched the maximal number of iterations with no success",
          logger = "fonctions.generateMapPlate")
  return(NULL)
}


# d: the user dataframe
# nb_p: the number of plates to fill
# df_max_size : the maximum number of samples that can be placed on the current plate
balancedGrpDistrib <- function(d, nb_p, df_max_size){

  grouped <- d %>%
    group_by(Group)
  # vecteur contenant les effectifs pour chaque groupe
  workforces <- group_size(grouped)
  test <- group_split(grouped)
  # effectifs par groupe pour chaque plaque
  w <- round(workforces/nb_p)

  toReturn <- list()
  missing <- rep(list(0), times = nb_p)

  for(p in 1:nb_p){
    df = data.frame(matrix(ncol = ncol(grouped)))
    names(df) <- names(grouped)

    for(g in 1:length(workforces)){
      if(nrow(test[[g]]) < w[g]){
        df <- rbind(df, as.data.frame(test[[g]]))

        missing[[p]] <- missing[[p]] + as.numeric(w[g] - nrow(test[[g]]))
        test[[g]] <- data.frame("Sample.name" = NA,
                                "Group" = NA, "Well" = NA,
                                "Status" = NA, "Row" = NA,
                                "Column" = NA)
      }else{
        selected <- sample(test[[g]]$Sample.name, size = w[g])
        wg <- as.data.frame(test[[g]][test[[g]]$Sample.name %in% selected,])
        test[[g]] <- test[[g]][!test[[g]]$Sample.name %in% selected,]
        df <- rbind(df,wg)
      }


    }
    df <- df[!is.na(df$Sample.name),]
    df$Sample.name <- as.character(df$Sample.name)
    df$Group <- as.factor(df$Group)
    df$Status <- as.factor(df$Status)

    toReturn[[p]] <- df
  }


  # loginfo("nrow in toReturn BEFORE while(nrow(m) !=0): %s ", unlist(lapply(toReturn, function(x) nrow(x))), logger = "balancedGrpDistrib")



  m <- bind_rows(test)
  m <- m[!is.na(m$Sample.name),]

  # loginfo("df_max_size: %s", df_max_size, logger = "balancedGrpDistrib")

  # as long as samples remain unassigned to a plate.
  while(nrow(m) !=0){

    # they are assigned to the plate with the lowest number of employees without
    #exceeding the maximum authorized number of samples per plate.

    # print(unlist(lapply(toReturn, function(x) nrow(x))))

    incomplete_plate <- which.min(unlist(lapply(toReturn, function(x) nrow(x))))

    incomplete_size <- nrow(toReturn[[incomplete_plate]])

    if(incomplete_size < df_max_size){
      toTake <- sample(m$Sample.name, size = (df_max_size-incomplete_size))
      totake_df <- m[which(m$Sample.name %in% toTake),]
      toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], totake_df)
      m <- subset(m, !(m$Sample.name %in% toTake))
    }else if(incomplete_size >= df_max_size){
      maxi <- which.max(unlist(lapply(toReturn, function(x) nrow(x))))

      if(incomplete_size < nrow(toReturn[[maxi]])){
        toTake <- sample(m$Sample.name, size = (nrow(toReturn[[maxi]])-incomplete_size))
        totake_df <- m[which(m$Sample.name %in% toTake),]
        toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], totake_df)
        m <- subset(m, !(m$Sample.name %in% toTake))
      }else if(incomplete_size == nrow(toReturn[[maxi]])){
        toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], m)
        m <- m[!m$Sample.name,]
      }

    }
  }
  loginfo("nrow in toReturn: %s ", unlist(lapply(toReturn, function(x) nrow(x))), logger = "balancedGrpDistrib")
  return(toReturn)
}
