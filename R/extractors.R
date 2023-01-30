#' Method Selector
#'
#' The `method_selector()` is a filtering function for extracting only the
#' relevant `clustering_parameters` from a <`medic`> object.
#'
#' @inheritParams summary.medic
#'
#' @details
#' The `method_selector()` function is a filtering function used for extracting
#' the characteristics of the chosen method. This function is used in all of the
#' investigative functions with a \code{only} statement such as
#' [`cluster_frequency()`] and [`medication_amount()`].
#'
#' @return
#' A `clustering_parameters` data frame with at least the following columns
#' * \code{cluster_name} the name of the clustering cluster_name.
#' * \code{clustering} the name of the clustering group before the number of
#'       clusters is chosen.
#' * \code{linkage} the linkage criteria.
#' * \code{alpha} the normalization tuning.
#' * \code{beta} the timing importance tuning.
#' * \code{gamma} the dose importance tuning.
#' * \code{theta} the ATC measure tuning.
#' * \code{k} the number of clusters.
#'
#' where each row specifies the parameters for the chosen methods. Any
#' additional columns present in the the outcome are from the
#' `additional_data`.
#'
#' @seealso [`cluster_selector()`] is another selector method used through out
#'   the package.
#'
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)
#'
#' method_selector(clust, k < 5)
#'
#'
#' # cluster selection based on additional data
#'
#' user_classification <- data.frame(
#'    k = 1:10,
#'    size = rep(c("small", "large"), times = c(4, 6)))
#'
#' method_selector(clust, size == "small", user_classification)
#'
#' @keywords internal
method_selector <- function(clustering, only, additional_data = NULL) {

  clust <- enrich_clustering_parameters(clustering, additional_data)

  if (rlang::quo_is_null(rlang::enquo(only))) {
    return(clust$clustering_parameters)
  }

  res <- clust$clustering_parameters %>%
    dplyr::rowwise() %>%
    dplyr::filter({{ only }}) %>%
    dplyr::ungroup()

  return(res)
}




#' Finding chosen cluster names
#'
#' The `cluster_selector()` function finds the name of the chosen clusters.
#'
#' @inheritParams summary.medic
#'
#' @return
#' A character vector with the names of the chosen clusters.
#'
#' @details
#' The `cluster_selector()` finds the names of the chosen clusters present in
#' the \code{clustering} <`medic`> object. This function is used in all of
#' the investigative functions with a \code{cluster} statement such as
#' [`atc_frequency()`] and [`medication_amount()`].
#'
#' @seealso [`method_selector()`] is another selector method used through out
#'   the package.
#'
#'
#' @examples
#' clust <- medic(
#'    tiny_example_data,
#'    id = id,
#'    atc = atc,
#'    k = 3:5
#' )
#'
#' cluster_selector(clust, clusters = I:III)
#'
#' @keywords internal
cluster_selector <- function(clustering, clusters = NULL) {

  all_names <- clustering$clustering_parameters$cluster_name
  all_clusters <- clustering$clustering %>%
    dplyr::select(dplyr::all_of(all_names)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "cluster_name",
                        values_to = "cluster") %>%
    dplyr::filter(!is.na(.data$cluster))

  # Defaulte NULL handling
  if (rlang::quo_is_null(rlang::enquo(clusters))) {
    return(all_clusters %>% dplyr::pull(.data$cluster) %>% unique())
  }

  # General handling
  chosen_clusters <- all_clusters %>%
    dplyr::distinct() %>%
    dplyr::arrange(as.numeric(as.roman(.data$cluster))) %>%
    dplyr::mutate(dummy = 1) %>%
    tidyr::pivot_wider(names_from = "cluster", values_from = "dummy") %>%
    dplyr::select({{ clusters }}) %>%
    names()

  return(chosen_clusters)
}
