% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_backtracking.R
\name{backtracking}
\alias{backtracking}
\title{Backtracking Function}
\usage{
backtracking(
  max_iter = 20,
  user_data,
  wells,
  rows,
  columns,
  nb_plates,
  constraint,
  prog = NULL
)
}
\arguments{
\item{max_iter}{numeric, the maximal number of iterations to do, default
value is 20}

\item{user_data}{dataframe, user samples to place randomly on the plate}

\item{wells}{dataframe, special wells not to be placed randomly on the plate}

\item{rows}{numeric, number of lines on the plate(s)}

\item{columns}{numeric, number of columns on the plate(s)}

\item{nb_plates}{numeric, number of plates}

\item{constraint}{character, spatial mode}

\item{prog}{progress bar used for shiny app only}
}
\value{
a dataframe containing user samples and special wells with their
coordinates for the corresponding plates.
}
\description{
Function used to launch the backtracking algorithm on a
dataframe with the corresponding plate parameters, number of iterations and
special wells
}
