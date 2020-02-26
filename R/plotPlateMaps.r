
# function to place the blanks on the plate according to the selected mode: it
# generates a dataframe containing the row and column coordinates for each blank
placeBlanksOnPlate <- function(p_lines, p_cols, mod = "none"){

  p_lines <- as.numeric(p_lines)
  p_cols <- as.numeric(p_cols)
  if(mod == "none"){
    result <- NULL
  }else{
    if(p_lines == 0 | p_cols == 0){
      result <- NULL
    }else{
      switch (mod,
              "by_column" = {
                nb_rows <- p_lines * ceiling(p_cols/2)
                df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = 1, to = p_cols, by=2)){
                  for(i in 1:p_lines){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }

              },
              "by_row" = {
                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample.name", "Group", "Well", "Status", "Row", "Column"))

                k=1
                for(i in seq(from = 1, to = p_lines, by=2)){
                  for(j in 1:p_cols){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }
              },
              "checkerboard" = {
                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = 1, to = p_cols)){
                  if(j%%2 == 0){ # j is peer
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

      df$Sample.name <- NA
      df$Group <- as.factor("blank")
      df$Status <- as.factor("blank")
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
    check_rows <- as.numeric(match(toupper(substr(forbidden_wells, 1, 1)), LETTERS702))
    check_columns <- as.numeric(substr(forbidden_wells, 2, 5))
    # actually simulates if user hasn't finished typing everything
    if(any(is.na(check_rows)) | any(is.na(check_columns))){
      return("ya un pb")
    }else if((max(check_rows) > max_Row) | (max(check_columns) > max_Col) ){
      # depending on the plate sizes that have been provided
      result <- NULL
    }else{
      # put the forbidden wells into the df
      forbidden <- setnames(setDF(lapply(c(NA, as.character(status), NA, as.character(status), NA, NA),
                                         function(...) character(length(forbidden_wells)))),
                            c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
      forbidden$Well <- as.character(forbidden_wells)
      forbidden$Sample.name <- as.integer(NA)
      forbidden$Group <- as.factor(status)
      forbidden$Status <- as.factor(status)
      forbidden$Row <- as.numeric(NA)
      forbidden$Column <- as.numeric(NA)

      # converts Well names to Row / Column coordinates as this is what is used
      # to calculate the backtracking step.
      forbidden <- mutate(forbidden,
                          Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS702)),
                          Column=as.numeric(substr(Well, 2, 5)))
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


drawPlateMap <- function(df, plate_lines, plate_cols, project_title = NULL){
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  nb_gps <- length(levels(df$Group))
  # this palette allows coloring depending on whether it is a blank, a
  # prohibited well, a Not Randomized sample or a randomized sample
  palette_strains <- c("blank"="#8B8378", "forbidden"="red", "notRandom" = "#9ACD32")
  # palette_complete <- c("#FFC125", "#FF7F00", "#458B00", "#104E8B", "#48D1CC", "#CD6889", "#FFD39B", "#8B1A1A", "#EEDC82", "#9F79EE", "#FF7F50")
  palette_complete <- c("#00AFBB", "#FC4E07", "#FFDB6D", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
  palette_choisie <- palette_complete[1:nb_gps]
  names(palette_choisie) <- levels(df$Group)
  palette_strains <- c(palette_strains, palette_choisie)
  colScale <- scale_color_manual(values = palette_strains)

  g <- ggplot(data = df, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, plate_cols), seq(1, plate_lines)), aes(x = Var1, y = Var2),
               color = "grey95", fill = "white", shape = 21, size = 6) +
    geom_point(aes(colour = Group), size = 10, shape = 19) +
    geom_point(colour = "white", size = 7, shape =19) +
    colScale +
    coord_fixed(ratio = (13/plate_cols)/(9/plate_lines), xlim = c(0.9, plate_cols+0.1), ylim = c(0, plate_lines+1)) +
    scale_y_reverse(breaks = seq(1, plate_lines), labels = LETTERS702[1:plate_lines]) +
    scale_x_continuous(breaks = seq(1, plate_cols)) +
    geom_text(aes(label = Sample.name), size = 3, na.rm = TRUE) +
    labs(title = project_title) +
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


#*******************************************************************************
# How to use the functions
#*******************************************************************************
# # # preparation des inputs comme ceux qu'on obtient dans l'appli shiny
# d <- read.csv2("./data/ind_groupes_NASH-80.csv",
#                header = TRUE,
#                sep = ";",
#                col.names = c("Sample.name", "Group"),
#                stringsAsFactors = FALSE)
#
# d$Group <- as.factor(d$Group)
# d$Well <- as.character(NA)
# d$Status <- as.factor("allowed")
# d$Row <- NA
# d$Column <- NA
#
#
# nb_l <- 8
# nb_c <- 12
# nb_p <- 1
# forbidden_wells <- "A1,A2,A3,A10,A11,A12"
# fw <- as.vector(unlist(strsplit(as.character(forbidden_wells),
#                                 split=",")))
# fw <- convertVector2Df(fw, nb_l, nb_c, "forbidden")
# mod <- "NEWS"
# max_it <- 20
# # # lancement de l'algo
# plate <- generateMapPlate(user_df = d, nb_rows = nb_l, nb_cols = nb_c, df_forbidden = fw, mod = mod, max_it = max_it)
# drawPlateMap(df = plate, plate_lines = nb_l, plate_cols = nb_c)
