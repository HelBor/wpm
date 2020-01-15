#*******************************************************************************
# theme functions for ggplot plate map
# code from https://github.com/briandconnelly/ggplot2bdc/blob/master/R/theme_bdc_grey.R
# and https://github.com/briandconnelly/ggplot2bdc/blob/master/R/theme_bdc_microtiter.R
#*******************************************************************************
theme_bdc_grey <- function(base_size = 12, base_family = "",
                           base_grey = "grey70",
                           grid.x = FALSE, grid.y = FALSE,
                           gridmin.x = grid.x, gridmin.y = grid.y,
                           ticks.x = TRUE, ticks.y = TRUE,
                           pmargin = base_size / 2) {
  half_line <- base_size / 2
  quarter_line <- base_size / 4
  line_size <- 0.5
  medgrey <- "grey40"

  t <- theme(
    line = element_line(color = base_grey, size = line_size,
                        linetype = 1, lineend = "square"),
    rect = element_rect(fill = "white", color = base_grey,
                        size = line_size, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = "black", size = base_size,
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                        angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(),
    axis.text = element_text(size = rel(0.8), color = medgrey),
    axis.text.x = element_text(
      margin = margin(t = 0.8 * quarter_line),
      vjust = 1
    ),
    axis.text.y = element_text(
      margin = margin(r = 0.8 * quarter_line),
      hjust = 1
    ),
    axis.ticks = element_line(size = line_size / 2),
    axis.ticks.x = element_line(
      size = ifelse(ticks.x, line_size / 2, 0)
    ),
    axis.ticks.y = element_line(
      size = ifelse(ticks.y, line_size / 2, 0)
    ),
    axis.ticks.length = unit(quarter_line, units = "pt"),
    axis.title.x = element_text(
      margin = margin(t = 0.8 * half_line, b = 0.8 * quarter_line)
    ),
    axis.title.y = element_text(
      angle = 90,
      margin = margin(r = 0.8 * half_line, l = 0.8 * quarter_line)
    ),

    legend.background = element_rect(fill = "transparent", color = NA),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing = unit(0, units = "cm"),
    legend.key = element_rect(fill = "transparent", color = NA),
    # legend.key.size = unit(0.01, units = "lines"),
    # legend.key.height = unit(0.5, units = "lines"),
    # legend.key.width = unit(1.0, units = "lines"),
    legend.text = element_text(size = rel(0.6), hjust = 0, vjust = 0.5,
                               color = medgrey),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.6), face = "bold",
                                vjust = 0.5),
    legend.title.align = 1,
    legend.direction = "vertical",
    legend.justification = "top",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.box.margin = margin(0, 0, 0, 0, unit = "pt"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(6, units = "pt"),

    panel.background = element_rect(fill = "white", color = base_grey),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = NA, size = line_size / 5),
    panel.grid.major.x = element_line(
      color = ifelse(grid.x, base_grey, NA)
    ),
    panel.grid.major.y = element_line(
      color = ifelse(grid.y, base_grey, NA)
    ),
    panel.grid.minor = element_line(color = NA, size = line_size / 10),
    panel.grid.minor.x = element_line(
      color = ifelse(gridmin.x, base_grey, NA)
    ),
    panel.grid.minor.y = element_line(
      color = ifelse(gridmin.y, base_grey, NA)
    ),
    panel.spacing = unit(quarter_line, units = "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "transparent", color = NA),
    strip.text = element_text(
      color = "grey40",
      size = rel(0.8),
      face = "bold"
    ),
    strip.text.x = element_text(
      margin = margin(t = quarter_line, b = quarter_line)
    ),
    strip.text.y = element_text(
      angle = -90,
      margin = margin(l = quarter_line, r = quarter_line)
    ),
    strip.switch.pad.grid = unit(0.1, units = "cm"),
    strip.switch.pad.wrap = unit(0.1, units = "cm"),

    plot.background = element_rect(color = "transparent"),
    plot.title = element_text(
      size = rel(1.2),
      #face = "bold",
      hjust = 0,
      margin = margin(b = half_line * 1.2)
    ),
    plot.subtitle = element_text(
      size = rel(0.8),
      color = "grey40",
      face = "italic",
      hjust = 0,
      margin = margin(b = (base_size / 2) * 1.2)
    ),
    plot.caption = element_text(
      size = rel(0.7),
      color = "grey40",
      face = "plain",
      hjust = 0,
      margin = margin(b = base_size * 0.4, t = base_size * 0.4,
                      r = 0, l = 0)
    ),
    plot.margin = margin(pmargin, pmargin, pmargin, pmargin),
    complete = TRUE
  )

  return(t)
}



theme_bdc_microtiter <- function(base_size = 12, base_family = "") {
  t <- theme_bdc_grey(base_size = base_size, base_family = base_family,
                      grid.x = FALSE, grid.y = FALSE,
                      gridmin.x = FALSE, gridmin.y = FALSE,
                      ticks.x = FALSE, ticks.y = FALSE,
                      pmargin = base_size / 2) %+replace%
    theme(
      panel.spacing = unit(0, units = "pt"),
      axis.title = element_blank(),
      axis.text = element_text(size = rel(1.0), face = "bold"),
      axis.text.y = element_text(
        margin = margin(r = 0.4 * base_size, l = 0.4 * base_size)
      ),
      axis.text.x = element_text(
        margin = margin(t = 0.4 * base_size, b = 0.4 * base_size)
      ),
      axis.ticks.length = unit(0, "pt"),
      # legend.direction = "horizontal",
      legend.key.size = unit(0.01, units = "lines"),
      legend.box = "vertical",
      legend.spacing = unit(6, "pt"),
      plot.title = element_text(
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(b = (base_size / 2) * 1.2)
      ),
      plot.subtitle = element_text(
        size = rel(0.8),
        color = "grey50",
        hjust = 0.5,
        margin = margin(b = (base_size / 2) * 1.2)
      )
    )

  return(t)
}


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
      df$Letters <- LETTERS[df$Row]
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
convertVector2Df <- function(forbidden_wells, max_Row, max_Col){

  if(length(forbidden_wells)>0){
    check_rows <- as.numeric(match(toupper(substr(forbidden_wells, 1, 1)), LETTERS))
    check_columns <- as.numeric(substr(forbidden_wells, 2, 5))
    # actually simulates if user hasn't finished typing everything
    if(any(is.na(check_rows)) | any(is.na(check_columns))){
      return("ya un pb")
    }else if((max(check_rows) > max_Row) | (max(check_columns) > max_Col) ){
      # depending on the plate sizes that have been provided
      result <- NULL
    }else{
      # put the forbidden wells into the df
      forbidden <- setnames(setDF(lapply(c(NA, "forbidden", NA, "forbidden", NA, NA),
                                         function(...) character(length(forbidden_wells)))),
                            c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
      forbidden$Well <- as.character(forbidden_wells)
      forbidden$Sample.name <- as.integer(NA)
      forbidden$Group <- as.factor("forbidden")
      forbidden$Status <- as.factor("forbidden")
      forbidden$Row <- as.numeric(NA)
      forbidden$Column <- as.numeric(NA)

      # converts Well names to Row / Column coordinates as this is what is used
      # to calculate the backtracking step.
      forbidden <- mutate(forbidden,
                          Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
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

drawPlateMap <- function(df, nb_gps, plate_lines, plate_cols, project_title){

  # this palette allows coloring depending on whether it is a blank, a
  # prohibited box, or a group
  palette_strains <- c("blank"="grey", "forbidden"="red")
  palette_complete <- c(brewer.pal(7, "Set2"), brewer.pal(7, "Accent"))
  palette_choisie <- palette_complete[1:nb_gps]
  names(palette_choisie) <- levels(df$Group)
  palette_strains <- c(palette_strains, palette_choisie)
  colScale <- scale_colour_manual(name = "group", values = palette_strains)

  g <- ggplot(data = df, aes(x = Column, y = Row)) +
    geom_point(data = expand.grid(seq(1, plate_cols), seq(1, plate_lines)), aes(x = Var1, y = Var2),
               color = "grey90", fill = "white", shape = 21, size = 6) +
    geom_point(aes(shape = Status, colour = Group), size = 12) +
    geom_text(aes(label = Sample.name), size = 4) +
    colScale +
    scale_shape_manual(values = c("forbidden" = 4, "blank" = 15, "allowed" = 19)) +
    coord_fixed(ratio = (13/plate_cols)/(9/plate_lines), xlim = c(0.9, plate_cols+0.1), ylim = c(0, plate_lines+1)) +
    scale_y_reverse(breaks = seq(1, plate_lines), labels = LETTERS[1:plate_lines]) +
    scale_x_continuous(breaks = seq(1, plate_cols)) +
    labs(title = project_title) +
    theme_bdc_microtiter()

  return(g)
}


#*******************************************************************************
# How to use the functions
#*******************************************************************************
# nb_l <- 8
# nb_c <- 12
# test_df <- placeBlanksOnPlate(nb_l,nb_c,"by_column")
#forbid_wells <- c("A1")
#forbid_wells_df <- convertVector2Df(forbid_wells, nb_l, nb_c)
#test2_df <- rbind(test_df,forbid_wells_df)
#test2_df <- forbid_wells_df
#if(class(test2_df) == "data.frame"){
#  drawPlateMap(df = test2_df, 1, plate_lines = nb_l, plate_cols = nb_c)
#}else{
#  print(test2_df)
#}
#df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(0))),
#               c("Sample.name", "Group", "Well", "Status", "Row", "Column"))

#drawPlateMap(df = df, 1, plate_lines = nb_l, plate_cols = nb_c)
