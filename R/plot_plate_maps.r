
# function to place the blanks on the plate according to the selected mode: it
# generates a dataframe containing the row and column coordinates for each blank
placeBlanksOnPlate <- function(p_lines, p_cols, mod = "none", start_blank){

  p_lines <- as.numeric(p_lines)
  p_cols <- as.numeric(p_cols)
  if(mod == "none"){
    result <- NULL
  }else{
    if(start_blank == "even"){
      start = 2
    }else{
      start = 1
    }
    if(p_lines == 0 | p_cols == 0){
      result <- NULL
    }else{
      switch (mod,
              "by_column" = {

                nb_rows <- p_lines * ceiling(p_cols/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = start, to = p_cols, by=2)){
                  for(i in 1:p_lines){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }

              },
              "by_row" = {

                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))

                k=1
                for(i in seq(from = start, to = p_lines, by=2)){
                  for(j in 1:p_cols){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }
              },
              "checkerboard" = {
                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = 1, to = p_cols)){
                  if(j%%2 == 0){ # j is even
                    for(i in seq(from = 2, to = p_lines, by=2)){
                      df$Row[k] <- i
                      df$Column[k] <- j
                      k = k + 1
                    }
                  }else{ # j is odd
                    for(i in seq(from = 1, to = p_lines, by=2)){
                      df$Row[k] <- i
                      df$Column[k] <- j
                      k = k + 1
                    }
                  }
                }
              }
      )

      # df$Sample <- NA
      df$Group <- as.character("blank")
      df$Sample.name <- as.integer(NA)
      df$Status <- as.character("blank")
      df$Row <- as.numeric(df$Row)
      df$Column <- as.numeric(df$Column)

      LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
      df$Letters <- LETTERS702[df$Row]
      df$Well <- apply( df[ , c("Letters", "Column") ] , 1 , paste0 , collapse = "" )
      df$Letters <- NULL
      # remove space between letters and numbers
      df$Well <- str_remove(df$Well, " ")

      result <- df %>%
        distinct(Row, Column, .keep_all = TRUE)

      # delete extra rows (Row and Column contain NAs)
      if(anyNA(result$Row) | anyNA(result$Column)){
        result <- na.omit(result, cols = c("Row", "Column"))
      }

    }
  }


  return(result)
}



#*******************************************************************************
# Function to determine the coordinates of the forbidden wells for the plot
# and generates the dataframe containing the Row and Column coordinates
#*******************************************************************************
convertVector2Df <- function(forbidden_wells, max_Row, max_Col, status){

  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

  if(length(forbidden_wells)>0){


    check_columns <- as.numeric(stringr::str_extract(forbidden_wells, "[0-9]+"))
    check_rows <- (stringr::str_extract(forbidden_wells, "[aA-zZ]+"))

    # actually simulates if user hasn't finished typing everything
    if(any(is.na(as.numeric(match(toupper(check_rows), LETTERS702)))) | any(is.na(check_columns))){
      return("ya un pb")
    }else if((max(as.numeric(match(toupper(check_rows), LETTERS702))) > max_Row) | (max(check_columns) > max_Col) ){

      # depending on the plate sizes that have been provided
      result <- NULL
    }else{
      # put the forbidden wells into the df
      forbidden <- setnames(setDF(lapply(c(NA, as.character(status), NA, NA, as.character(status), NA, NA),
                                         function(...) character(length(forbidden_wells)))),
                            c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))
      forbidden$Sample <- as.character(NA)
      forbidden$Group <- as.character(status)
      forbidden$Sample.name <- as.integer(NA)
      forbidden$Well <- as.character(forbidden_wells)
      forbidden$Status <- as.character(status)
      forbidden$Row <- as.numeric(NA)
      forbidden$Column <- as.numeric(NA)


      # converts Well names to Row / Column coordinates as this is what is used
      # to calculate the backtracking step.
      forbidden <- mutate(forbidden,
                          Row=as.numeric(match(toupper(check_rows), LETTERS702)),
                          Column=check_columns)
      #erase all duplicated rows
      result <- forbidden %>%
        distinct(Row, Column, .keep_all = TRUE)

    }

  }else{
    # no forbidden wells, so doesn't return a dataframe
    result <- NULL
  }
  return(result)
}






#*******************************************************************************
# Function to plot the input dataframe containing the Sample names, the Row,
# Column coordinates, the group and the status
#*******************************************************************************

# gp_levels: Group column levels  before adding the forbidden wells to df
drawPlateMap <- function(df, sample_gps, gp_levels, plate_lines, plate_cols, project_title){
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  if("Group" %in% colnames(df)){
    df$Group <- as.factor(df$Group)
    nb_gps <- length(levels(df$Group))
  }else{
    df$Group <- as.factor(1)
  }

  # loginfo("gp_levels: %s", gp_levels, logger = "drawPlateMap")
  # loginfo("nb_gps: %s", nb_gps,logger = "drawPlateMap")
  # this palette allows coloring depending on whether it is a blank, a
  # prohibited well, a Not Randomized sample or a randomized sample

  palette_strains <- c("blank"="#8B8378", "forbidden"="red", "notRandom" = "black")
  # loginfo("sample_gps: %s", sample_gps, logger = "drawPlateMap")
  # control the number of colors to pick according to the number of groups
  if(sample_gps == 1){
    sub_palette <- RColorBrewer::brewer.pal(n = 4, "Paired")[4]
  }else if(sample_gps == 2){
    sub_palette <- RColorBrewer::brewer.pal(n = 8, "Paired")[c(4,8)]
  }else{
    sub_palette <- RColorBrewer::brewer.pal(n = sample_gps, "Paired")
  }
  # loginfo("gp_levels: %s", gp_levels, logger = "drawPlateMap")
  names(sub_palette) <- gp_levels
  palette_strains <- c(palette_strains, sub_palette)
  colScale <- scale_color_manual(values = palette_strains)

  g <- ggplot(data = df, aes(x = Column, y = Row, color = Group)) +
    geom_point(data = expand.grid(seq(1, plate_cols), seq(1, plate_lines)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 21, size = 10) +
    geom_point(aes(colour = Group), size = 10) +
    geom_point(colour = "white", size = 7) +
    colScale +
    coord_equal()+
    scale_y_reverse(breaks = seq(1, plate_lines), labels = LETTERS702[1:plate_lines]) +
    scale_x_continuous(breaks = seq(1, plate_cols)) +
    geom_text(aes(label = Sample.name), colour = "black", size = 3, na.rm = TRUE) +
    labs(title = project_title) +
    guides(colour = guide_legend(override.aes = list(size=7))) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey50"),
      panel.grid = element_blank(),
      legend.key = element_rect(fill = "white"),
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold", size = 12),
      axis.title = element_blank(),
      legend.title = element_text(face = "bold"),
      legend.justification = c("top")
    )



  return(g)
}

