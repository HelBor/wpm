##' Generate a ggplot object of a plate plan
##'
##' @description Function to plot the input dataframe containing the Sample
##' names, the Row, Column coordinates, the group and the status
##'
##' @param df dataframe containing user data and special wells if any.
##' @param sample_gps number of distinct groups in the file before adding the
##' special wells to df
##' @param gp_levels is Group column levels before adding the special wells to df
##' @param plate_lines integer, number of plate's lines
##' @param plate_cols integer, number of plate's columns
##' @param project_title character, the user's project title
##' @return g, a ggplot object corresponding to the generated plate map.
##' @importFrom rlang .data
##' @examples
##' # example of data containing 5 biological samples, 2 forbidden wells,
##' # 2 buffers and 3 not random wells
##' user_data <- data.frame("Sample" = c(as.character(seq_len(5)), rep_len(NA, 7)),
##'                         "Group" = c(c("A","B","C","A","B"),
##'                                     rep_len("forbidden", 2),
##'                                     rep_len("buffer", 2),
##'                                     rep_len("fixed", 3)),
##'                         "ID" = c(seq_len(5),rep_len(NA,7)),
##'                         "Well" = c("A2","B3","C3","B4","A3","A1","A4","B2","C2","B1","C1","C4"),
##'                         "Status" = c(rep_len("toRandom", 5),
##'                                      rep_len("forbidden", 2),
##'                                      rep_len("buffer", 2),
##'                                      rep_len("fixed", 3)),
##'                         "Row" = c(1,2,3,2,1,1,1,2,3,2,3,3),
##'                         "Column" = c(2,3,3,4,3,1,4,2,2,1,1,4))
##' p <- "My Project"
##' gp_lvl <- levels(as.factor(c("A","B","C")))
##' drawMap(df = user_data, sample_gps = 3, gp_levels = gp_lvl, plate_lines = 3,
##'         plate_cols = 4, project_title = p)
##'
##' # also works when giving a plate with more wells than the number of samples to place.
##' drawMap(df = user_data, sample_gps = 3, gp_levels = gp_lvl, plate_lines = 8,
##'         plate_cols = 12, project_title = p)
##'
##' @export
drawMap <- function(df, sample_gps, gp_levels, plate_lines, plate_cols, project_title){
    # checks that the plate dimensions are compatible with the number of samples
    #  to be placed.
    if(plate_lines*plate_cols < nrow(df)){
        message("The plate dimensions are not compatible with the number of
                samples. Please increase plate size or number of samples.")
    }


    LETTERS702 <- c(LETTERS, vapply(LETTERS,
                                    FUN.VALUE = as.character(seq_len(26)),
                                    function(x) paste0(x, LETTERS)))
    if ("Group" %in% colnames(df)) {
        df$Group <- as.factor(df$Group)
    }else{
        df$Group <- as.factor(1)
    }

    # this palette allows coloring depending on whether it is a buffer solution,
    # a forbidden well, a fixed sample or a randomized sample

    palette_strains <- c("buffer" = "#8B8378",
                         "forbidden" = "red",
                         "fixed" = "black")

    # control the number of colors to pick according to the number of groups
    if (sample_gps <= 1) {
        sub_palette <- RColorBrewer::brewer.pal(n = 4, "Paired")[4]
    }else if (sample_gps == 2) {
        sub_palette <- RColorBrewer::brewer.pal(n = 8, "Paired")[c(4, 8)]
    }else{
        sub_palette <- RColorBrewer::brewer.pal(n = sample_gps, "Paired")
    }

    names(sub_palette) <- gp_levels
    palette_strains <- c(palette_strains, sub_palette)
    colScale <- ggplot2::scale_color_manual(values = palette_strains)

    g <- ggplot2::ggplot(data = df,
                         ggplot2::aes(x = .data$Column,
                                      y = .data$Row,
                                      color = .data$Group)) +
        ggplot2::geom_point(data = expand.grid(seq(1, plate_cols),
                                               seq(1, plate_lines)),
                            ggplot2::aes(x = .data$Var1, y = .data$Var2),
                            color = "grey90",
                            fill = "white",
                            shape = 21,
                            size = 10) +
        ggplot2::geom_point(ggplot2::aes(colour = .data$Group), size = 10) +
        ggplot2::geom_point(colour = "white", size = 7) +
        colScale +
        ggplot2::coord_equal() +
        ggplot2::scale_y_reverse(breaks = seq(1, plate_lines),
                                 labels = LETTERS702[seq_len(plate_lines)]) +
        ggplot2::scale_x_continuous(breaks = seq(1, plate_cols)) +
        ggplot2::geom_text(ggplot2::aes(label = .data$ID),
                           colour = "black",
                           size = 3,
                           na.rm = TRUE) +
        ggplot2::labs(title = project_title) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 7))) +
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

