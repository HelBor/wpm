

#*******************************************************************************
# Function to plot the input dataframe containing the Sample names, the Row,
# Column coordinates, the group and the status
#*******************************************************************************

# gp_levels: Group column levels  before adding the forbidden wells to df
#' @importFrom rlang .data
#' @import ggplot2
drawMap <- function(df, sample_gps, gp_levels, plate_lines, plate_cols, project_title){
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  if("Group" %in% colnames(df)){
    df$Group <- as.factor(df$Group)
    nb_gps <- length(levels(df$Group))
  }else{
    df$Group <- as.factor(1)
  }

  # this palette allows coloring depending on whether it is a blank, a
  # prohibited well, a Not Randomized sample or a randomized sample

  palette_strains <- c("blank"="#8B8378", "forbidden"="red", "notRandom" = "black")

  # control the number of colors to pick according to the number of groups
  if(sample_gps <= 1){
    sub_palette <- RColorBrewer::brewer.pal(n = 4, "Paired")[4]
  }else if(sample_gps == 2){
    sub_palette <- RColorBrewer::brewer.pal(n = 8, "Paired")[c(4,8)]
  }else{
    sub_palette <- RColorBrewer::brewer.pal(n = sample_gps, "Paired")
  }

  names(sub_palette) <- gp_levels
  palette_strains <- c(palette_strains, sub_palette)
  colScale <- ggplot2::scale_color_manual(values = palette_strains)

  g <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$Column, y = .data$Row, color = .data$Group)) +
    ggplot2::geom_point(data = expand.grid(seq(1, plate_cols),
                                           seq(1, plate_lines)),
                        ggplot2::aes(x = .data$Var1, y = .data$Var2),
                        color = "grey90",
                        fill = "white",
                        shape = 21,
                        size = 10) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Group),
                        size = 10) +
    ggplot2::geom_point(colour = "white",
                        size = 7) +
    colScale +
    ggplot2::coord_equal()+
    ggplot2::scale_y_reverse(breaks = seq(1, plate_lines),
                             labels = LETTERS702[1:plate_lines]) +
    ggplot2::scale_x_continuous(breaks = seq(1, plate_cols)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$ID),
                       colour = "black",
                       size = 3,
                       na.rm = TRUE) +
    ggplot2::labs(title = project_title) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=7))) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "grey50"),
      panel.grid = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = "white"),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(face = "bold", size = 12),
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.justification = c("top")
    )



  return(g)
}

