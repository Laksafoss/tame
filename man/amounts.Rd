% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_helpers.R
\name{amounts}
\alias{amounts}
\title{Frequency tables for medication amount}
\usage{
amounts(
  clustering,
  only = NULL,
  clusters = NULL,
  count_grouper = function(x) {
     cut(x, breaks = c(0, 1, 2, Inf), labels = c("1",
    "2", "3+"))
 },
  additional_data = NULL
)
}
\arguments{
\item{only}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> Expressions that
return a logical value, and are defined in terms of the variables in
\code{object} and/or \code{additional_data}.

The default \code{NULL} selects all clusterings in \code{object}.}

\item{clusters}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> An unquoted
expression naming the cluster or clusters in \code{object} one wants to
see summaries of. Names can be used as if they were positions in the data
frame, so expressions like I:IV can be used to select a range of clusters.

The default \code{NULL} selects all clusters in the chosen clusterings of
\code{object}.}

\item{count_grouper}{A function for grouping counts. As a standard it groups
counts as 1 medication, 2 medications, and 3+ medications.}

\item{additional_data}{A data frame with additional data that may be
(left-)joined onto the \code{clustering_parameters} in \code{object}.}
}
\value{
\code{amounts()} returns a data frame of class \code{summary.medic.amounts}
\itemize{
\item \code{x} very.
\item \code{y} important.
\item \code{z} info.
}
}
\description{
The function \code{amounts()} calculates the number of unique
medications for each individual and presents the count frequencies by
cluster.
}
\details{
TODO
}
\examples{
clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)

tame:::amounts(clust, k == 5, clusters = I:III)

}
\keyword{internal}