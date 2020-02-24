
## Functions for contraints
# Contrainte spatiale pour les voisins Nord, Est, Ouest et Sud pour la case visitée
neighborhoodNEWS <- function(m, i, j){

  #Pour le Nord
  N <- tryCatch(
    {
      m[i-1,j]
    },
    error = function(err1){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodNEWS")
      return(NA)
    }
  )

  #Pour l'Est
  E <- tryCatch(
    {
      m[i,j+1]
    },
    error = function(err2){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodNEWS")
      return(NA)
    }
  )

  # Pour l'Ouest
  W <- tryCatch(
    {
      m[i,j-1]
    },
    error = function(err3){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodNEWS")
      return(NA)
    }
  )

  # Pour le Sud
  S <- tryCatch(
    {
      m[i+1,j]
    },
    error = function(err4){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodNEWS")
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
        # logerror("error, NA returned", logger = "fonctions.neighborhoodNS")
        return(NA)
    }
  )

  S <- tryCatch(
    {
      m[i+1,j]
    },
    error = function(err4){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodNS")
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
      #logerror("error, NA returned", logger = "fonctions.neighborhoodWE")
      return(NA)
    }
  )

  # For left neighboor
  W <- tryCatch(
    {
      m[i,j-1]
    },
    error = function(err3){
      # logerror("error, NA returned", logger = "fonctions.neighborhoodWE")
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
  # loginfo("neighbors are %s", neighbors, logger = "fonctions.checkConstraints")
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


  # identify which group the neighbors belong to in order to obtain a reduced
  # list of possibilities of groups for the current cell to fill
  forbidden_groups <- unique(d$Group[which(d$Sample.name %in% neighbors)])
  possible_groups <- levels(d$Group)[which(!levels(d$Group) %in% forbidden_groups)]

  # loginfo("possible_groups : %s", possible_groups)
  if(length(possible_groups)==0){
    #there are no more possibilities
    return(1)
  }else{
    # only take in individuals belonging to the possible groups
    # and who are not in already_drawn
    possible_ind <- d$Sample.name[which(d$Group %in% possible_groups)]
    # loginfo("nb of possible_ind : %s", length(possible_ind))
    available_ind <- d$Sample.name[which(d$Sample.name %in% possible_ind & !(d$Sample.name %in% already_drawn))]
    # loginfo("available_ind: %s", available_ind)
    # loginfo("length(available_ind): %s", length(available_ind))
    if(length(available_ind)==0){
      #there are no more possibilities
      return(1)
    }else{
      # use resample because this function also works as expected when there is
      # only one element in the set to be sampled.
      chosen_ind <- resample(available_ind,size=1)
      # loginfo("chosen_ind : %s", chosen_ind)
      m[i,j] <- chosen_ind
      already_drawn <- c(already_drawn,chosen_ind)
      # loginfo("already_drawn: %s", already_drawn)
    }
  }

  return(list("m" = m, "already_drawn" = already_drawn))
}


# parcours aléatoire de la matrice à remplir
# m est une matrice
# forbidden_cells est un vecteur contenant les coordonnées sous forme xy des cases
# interdites
# toVisit contient les cases sous forme A1, et ne contient que les cases
# autorisée à être remplies
# d est le dataframe fournit par l'utilisateur
# groups est le nombre de groupes distincts existant dans les données utilisateur
# constraint est le mode de contrainte de voisinnage choisi par l'utilisateur
randomWalk <- function(m, toVisit, d, constraint){

  if(class(m) != "matrix"){
    logerror("m is not a matrix, m: %s", class(m))
    warning("Need m to be a matrix")
  }

  visited <- c() # cases visitées
  nb_lig <- dim(m)[1]
  nb_col <- dim(m)[2]
  ret = m
  placed = c() # échantillons déjà tirés et placés
  # tant que tous les échantillons n'ont pas été placés dans visited
  # loginfo("nrow(d): %s", nrow(d), logger = "fonctions.randomWalk")


  while (length(visited) != nrow(d)) {

    cell <- resample(toVisit, size = 1)
    # loginfo("chosen cell: %s", cell, logger = "fonctions.randomWalk")
    # loginfo("visited: %s", visited, logger = "fonctions.randomWalk")
    # mise à jour des cases visitées
    visited <- c(visited,cell)
    # loginfo("length(visited): %s", length(visited), logger = "fonctions.randomWalk")
    # loginfo("length(toVisit): %s", length(toVisit))

    i <- as.numeric(match(toupper(substr(cell, 1, 1)), LETTERS))
    j <- as.numeric(substr(cell, 2, 5))
    # uniformisation de plaque
    test <- solveCell(m=ret,
                      d=d,
                      i=i,
                      j=j,
                      already_drawn = placed,
                      constraint = constraint)
    # loginfo("test: %s", class(test))
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
  # loginfo("length of toVisit: %d", length(toVisit), logger = "fonctions.randomWalk")
  # loginfo("length(visited) : %s", length(visited), logger = "fonctions.randomWalk")
  # loginfo(d[1,], logger = "fonctions.randomWalf")
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
  # loginfo("mode: %s", mod, logger = "fonctions.generateMapPlate")
  while (ret==1 & nb_attempts <= max_it) {

    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("attempt number", nb_attempts)
      updateProgress(detail = text)
    }


    loginfo("attempt n° %d", nb_attempts, logger = "fonctions.generateMapPlate")
    mat = matrix(NA,nrow=nb_rows, ncol=nb_cols)

    # Generate all the cells that are allowed to be filled
    toVisit <- NULL
    for(i in LETTERS[1:nb_rows]){
      for (j in 1:nb_cols){
        toVisit <- c(toVisit, paste0(i,j))
      }
    }

    # loginfo("is.null(df_forbidden$Well): %s",is.null(df_forbidden$Well))
    if(is.null(df_forbidden$Well)){

      # toVisit <- toVisit[1:nrow(user_df)]
    }else{
      toVisit <- toVisit[!toVisit %in% df_forbidden$Well]
    }
    # loginfo("toVisit: %s", toVisit)
    ret <- randomWalk(m = mat,
                      toVisit = toVisit,
                      d = user_df,
                      constraint = mod
                      )
    # loginfo("class(ret): %s", class(ret), logger = "fonctions.generateMapPlate")
    if(class(ret)=="data.frame"){
      ret$Well <- paste0(LETTERS[ret$Row], ret$Column, sep = "")


      ret <- rbind(ret, df_forbidden)
      logwarn("number of attempts: %d", nb_attempts, logger = "fonctions.generateMapPlate")
      return(ret)
    }

    nb_attempts = nb_attempts + 1
  }
  logwarn("we reeched the maximal number of iterations with no success", logger = "fonctions.generateMapPlate")
  return(NULL)
}



#*******************************************************************************
#
#                                   TEST ZONE
#
#*******************************************************************************

# # preparation des inputs comme ceux qu'on obtient dans l'appli shiny
# d <- read.csv2("./data/ind_groupes_NASH-160.csv",
#                 header = TRUE,
#                 sep = ";",
#                 col.names = c("Sample.name", "Group"),
#                 stringsAsFactors = FALSE)
#
# d$Group <- as.factor(d$Group)
# d$Well <- as.character(NA)
# d$Status <- as.factor("allowed")
# d$Row <- NA
# d$Column <- NA


# nb_l <- 8
# nb_c <- 12
# nb_p <- 3
# forbidden_wells <- "A1,A2,A3,A10,A11,A12,B1,B12,G1,G12,H1,H2,H3,H10,H11,H12"
# fw <- as.vector(unlist(strsplit(as.character(forbidden_wells),
#                                 split=",")))
# fw <- convertVector2Df(fw, nb_l, nb_c)
# mod <- "NEWS"
# max_it <- 20
# # lancement de l'algo
# # plate <- generateMapPlate(user_df = df, nb_rows = nb_l, nb_cols = nb_c, df_forbidden = fw, mod = mod, max_it = max_it)
# # drawPlateMap(df = plate, nb_gps = 11, plate_lines = nb_l, plate_cols = nb_c)
# #
# #

#
#
#
# #
# d_input <- d

balancedGrpDistrib <- function(d, nb_p){

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
  incomplete_plate <- which.min(unlist(lapply(toReturn, function(x) nrow(x))))
  m <- bind_rows(test)
  m <- m[!is.na(m$Sample.name),]
  toReturn[[incomplete_plate]] <- rbind(toReturn[[incomplete_plate]], m)

  return(toReturn)

}



