#' Timing and ATC based Medication Clustering
#'
#' Agglomerative hierarchical clustering with a bespoke distance measure based 
#' on medication ATC codes similarities, medication timing and medication amount
#' or dosage. Tools for summarizing, illustrating and manipulating the cluster
#' objects are also available.
#' 
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @importFrom utils as.roman
#' @importFrom Rcpp evalCpp
#' @useDynLib tame, .registration = TRUE
#' @keywords internal
"_PACKAGE"


globalVariables(c("k", "method", "theta_list", "III"))