
# Function generating a plate map according to the input parameters
# user_df      : dataframe [Sample, Group, Sample.name, Well, Status, Row, Column]
# nb_rows      : integer (number of lines on the plate)
# nb_cols      : integer (number of columns on the plate)
# df_forbidden : dataframe [Sample, Group, Sample.name, Well, Status, Row, Column]
# mod          : character (neighborhood spatial constraint)
# max_it       : integer (maximum number of attempts to generate a plate plan before
#                returning a failure.)
generateMap <- function(user_df, nb_rows, nb_cols, df_forbidden, mod, max_it, updateProgress = NULL){

  nb_attempts = 1
  ret=1
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  while (ret==1 & nb_attempts <= max_it) {

    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("attempt number", nb_attempts)
      updateProgress(detail = text)
    }

    mat = matrix(NA,nrow=nb_rows, ncol=nb_cols)

    # Generate all the cells that are allowed to be filled
    toVisit <- NULL
    for(i in LETTERS702[1:nb_rows]){
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
      ret$Well <- paste0(LETTERS702[ret$Row], ret$Column, sep = "")

      # if(!("Group" %in% colnames(ret))){
      #   loginfo("on rajoute la colonne Group!")
      #   ret$Group <- as.character("1")
      # }


      ret %>% dplyr::mutate_if(is.factor, as.character) -> ret


      ret <- dplyr::bind_rows(ret, df_forbidden)
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
