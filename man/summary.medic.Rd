% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.medic}
\alias{summary.medic}
\alias{print.summary.medic}
\alias{plot.summary.medic}
\title{Summary of medic object}
\usage{
\method{summary}{medic}(
  object,
  only = NULL,
  clusters = NULL,
  outputs = c("frequencies", "medications", "amounts", "trajectories", "interactions"),
  additional_data = NULL,
  ...
)

\method{print}{summary.medic}(x, ...)

\method{plot}{summary.medic}(x, by, facet, ...)
}
\arguments{
\item{object}{An object for which a summary is desired.}

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

\item{outputs}{A character vector naming the desired characteristics to
output. The default names all possible output types.}

\item{additional_data}{A data frame with additional data that may be
(left-)joined onto the \code{parameters} in \code{object}. This is often
used in conjuction with \code{only} to select specific clusterings based on
\code{additional_data}.}

\item{...}{Additional arguments passed to the internal summary function.
\itemize{
\item \code{cluster_wise}  an option in the \code{medications()} function.
\item \code{m} an option in the \code{medications()} function. A numeric restricting the
number of distinct ATC codes plotted within each cluster. That is, the
(at most) \code{m} most frequent ATC codes within that cluster is given a
color.
\item \code{q} an option in the \code{medications()} function. A numeric between 0 and 1
restricting the minimal ATC codes frequency displayed within each
cluster.
\item \code{count_grouper} an option in the \code{amounts()} function. A function for
grouping counts. As a standard it groups counts as 1 medication, 2
medications, and 3+ medications.
\item \code{atc_groups} A data.frame specifying the ATC groups to summaries by. The
data.frame must have two columns: (1) \code{regex} giving regular expressions
specifying the wanted ATC groups and (2) \code{atc_groups} the name of this
ATC grouping. As a standard the anatomical level (first level) of the ATC
codes is used.
}}

\item{x}{A \code{summary.medic} object for printing or plotting.}

\item{by}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}>}

\item{facet}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}>}
}
\value{
A list of clustering characteristics of class \code{summary.medic} is returned. It
can contain any of the following characteristics:
\subsection{Frequencies}{

The number of individuals assigned to each cluster and the associated
frequency of assignment.
}

\subsection{Medications}{

The number of individuals with a specific ATC code within a cluster.
Moreover, it calculates the percentage of people with this medication
assigned to this cluster and the percent of people within the cluster with
this medication.
}

\subsection{Amounts}{

The number of ATC codes an individual has, and then outputs the number of
individuals within a cluster that has that many ATC codes. Moreover, various
relevant percentages or calculated. See Value below for more details on these
percentages.
}

\subsection{Trajectories}{

The number of unique timing trajectories in each cluster, and the average
timing trajectories in each cluster.
}

\subsection{Interactions}{

The number of people with unique timing trajectory and ATC group, as given by
\code{atc_groups}, in each cluster.
}
}
\description{
Make cluster characterizing summaries.
}
\section{Methods (by generic)}{
\itemize{
\item \code{print(summary.medic)}: Print method for medic-objects

\item \code{plot(summary.medic)}: Plot method for medic-objects

}}
\examples{
clust <- medic(complications, id = id, atc = atc, k = 3:5)

}
