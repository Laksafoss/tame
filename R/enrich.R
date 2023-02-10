#' Enrich Clustering Parameter
#'
#' Enrich the parameter information in a clustering with user-defined data.
#'
#' @param object A `medic` object for enrichment.
#' @param additional_data A data frame with additional data that may be
#'   (left-)joined onto the `parameters` in `object`.
#' @param join_by A character vector of variables to join by. This variables is
#'    passed to the `by` term in a [dplyr::left_join()] and inherets its
#'    behavior:
#'
#'    If `NULL`, the default, the join will perform a natural join, using all
#'    variables in common across the `parameters` and
#'    `additional_data`.
#'
#'    To join by different variables on `parameters` and
#'    `additional_data`, use a named vector. For example,
#'    `by = c("k" = "cluster_size")` will match `parameters$k` to
#'    `additional_data$cluster_size`.
#'
#'    To join by multiple variables, use a vector with length > 1. For example,
#'    `by = c("k", "summation_method")` will match `parameters$k` to
#'    `additional_data$k` and `parameters$summation_method` to \cr
#'    `additional_data$summation_method`. Use a named vector to match different
#'    variables in `parameters` and `additional_data`.
#'
#'    For example, `by = c("k" = "cluster_size", "summation_method" = "sm")`
#'    will match `parameters$k` to `additional_data$cluster_size` and
#'    `parameters$summation_method` to `additional_data$sm`.
#'
#'    To perform a cross-join, generating all combinations of
#'    `parameters` and `additional_data`, use `join_by = character()`.
#'
#' @details
#' The `enrich()` function is a joining function used for enriching the
#' clustering characteristics with user-defined data. This function is used in
#' all of the investigative functions with a `additional_data` statement such as
#' [`frequencies()`] and [`amounts()`].
#'
#' @return
#' An object of class \emph{medic}.
#'
#' @examples
#' clust <- medic(
#'    tiny_example_data,
#'    id = id,
#'    atc = atc,
#'    timing = first_trimester:third_trimester,
#'    k = 3:5
#' )
#'
#' new_parameters <- data.frame(k = 3:5, size = c("small", "small", "large"))
#'
#' enrich(clust, new_parameters)
#'
#' @export
enrich <- function(clustering, additional_data = NULL, join_by = NULL) {
  if (!is.null(additional_data)) {
    if (is.null(join_by)) {
      bys <- intersect(
        names(additional_data),
        names(new$parameters)
      )
    } else {
      bys <- join_by
    }
    new <- clustering
    new$parameters <- new$parameters %>%
      dplyr::left_join(additional_data, by = bys)
    return(new)
  }
  return(clustering)
}
