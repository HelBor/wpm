% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_draw_map.R
\name{drawMap}
\alias{drawMap}
\title{Generate a ggplot object of a plate plan}
\usage{
drawMap(df, sample_gps, gp_levels, plate_lines, plate_cols, project_title)
}
\arguments{
\item{df}{dataframe containing user data and special wells if any.}

\item{sample_gps}{number of distinct groups in the file before adding the
special wells to df}

\item{gp_levels}{is Group column levels before adding the special wells to df}

\item{plate_lines}{integer, number of plate's lines}

\item{plate_cols}{integer, number of plate's columns}

\item{project_title}{character, the user's project title}
}
\value{
g, a ggplot object corresponding to the generated plate map.
}
\description{
Function to plot the input dataframe containing the Sample
names, the Row, Column coordinates, the group and the status
}
\examples{
# example of data containing 5 biological samples, 2 forbidden wells,
# 2 buffers and 3 not random wells
user_data <- data.frame("Sample" = c(as.character(seq_len(5)), rep_len(NA, 7)),
                        "Group" = c(c("A","B","C","A","B"),
                                    rep_len("forbidden", 2),
                                    rep_len("buffer", 2),
                                    rep_len("fixed", 3)),
                        "ID" = c(seq_len(5),rep_len(NA,7)),
                        "Well" = c("A2","B3","C3","B4","A3","A1","A4","B2","C2","B1","C1","C4"),
                        "Status" = c(rep_len("toRandom", 5),
                                     rep_len("forbidden", 2),
                                     rep_len("buffer", 2),
                                     rep_len("fixed", 3)),
                        "Row" = c(1,2,3,2,1,1,1,2,3,2,3,3),
                        "Column" = c(2,3,3,4,3,1,4,2,2,1,1,4))
p <- "My Project"
gp_lvl <- levels(as.factor(c("A","B","C")))
drawMap(df = user_data, sample_gps = 3, gp_levels = gp_lvl, plate_lines = 3,
        plate_cols = 4, project_title = p)

# also works when giving a plate with more wells than the number of samples to place.
drawMap(df = user_data, sample_gps = 3, gp_levels = gp_lvl, plate_lines = 8,
        plate_cols = 12, project_title = p)

}
