% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_balanced_distribution.R
\name{balancedGrpDistrib}
\alias{balancedGrpDistrib}
\title{Makes a balanced distribution of the elements between several plates.}
\usage{
balancedGrpDistrib(d, nb_p, df_max_size)
}
\arguments{
\item{d}{the user dataframe}

\item{nb_p}{the number of plates to fill}

\item{df_max_size}{the maximum number of samples that can be placed on the
current plate}
}
\value{
a list of dataframes each corresponding to a plate to fill.
}
\description{
This function makes it possible to distribute the samples
equitably on several plates, taking into account the numbers in the groups
(if there are any). This means that, for example, if 2 plates are to be
filled, then 50% of each group will be assigned to each plate. More
generally, all the plates are assigned the same number of elements. When
the numbers do not allow it (in particular when the total number of
elements to be allocated is not a multiple of the number of plates),
there will be a slight difference in the number of samples on the plates.
}
