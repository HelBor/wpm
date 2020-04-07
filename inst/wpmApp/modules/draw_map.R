

#*******************************************************************************
# Function to plot the input dataframe containing the Sample names, the Row,
# Column coordinates, the group and the status
#*******************************************************************************

# gp_levels: Group column levels  before adding the forbidden wells to df
#' @importFrom rlang .data
drawMap <- function(df, sample_gps, gp_levels, plate_lines, plate_cols, project_title){
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
  if(sample_gps <= 1){
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

  g <- ggplot(data = df, aes(x = .data$Column, y = .data$Row, color = .data$Group)) +
    geom_point(data = expand.grid(seq(1, plate_cols), seq(1, plate_lines)), aes(x = .data$Var1, y = .data$Var2),
               color = "grey90", fill = "white", shape = 21, size = 10) +
    geom_point(aes(colour = .data$Group), size = 10) +
    geom_point(colour = "white", size = 7) +
    colScale +
    coord_equal()+
    scale_y_reverse(breaks = seq(1, plate_lines), labels = LETTERS702[1:plate_lines]) +
    scale_x_continuous(breaks = seq(1, plate_cols)) +
    geom_text(aes(label = .data$ID), colour = "black", size = 3, na.rm = TRUE) +
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

