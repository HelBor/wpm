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
