#' Refactor Cluster Levels
#'
#' Refactor the levels of the chosen clusters.
#'
#' @param object A `medic` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Name-value pairs.
#'    `...` is passed to [`dplyr::mutate`], and therefor inherits its behavior:
#'
#'
#'    The name gives the name of the new clustering in the output. The value can
#'    be:
#'
#'    * A vector of length 1, which will be recycled to the correct length.
#'    * A function of another clustering.
#'
#'    When a recording uses the name of an existing clustering, this new
#'    clustering will overwrite the existing one.
#' @param inheret_parameters A logical. If `TRUE` a new clustering overwriting
#'    an existing clustering inherits the `clustering_parameters` of the old.
#'
#' @details
#' TODO
#'
#' @return
#' A `medic` object with relevant clusterings refactored.
#'
#' @examples
#'
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:4)
#'
#' # Refactor one clustering
#' refactor(
#'   clust,
#'   `cluster_1_k=4` = dplyr::recode(`cluster_1_k=4`, IV = "III")
#' )
#'
#' # Refactor all clusterings
#' refactor(
#'   clust,
#'   dplyr::across(
#'     dplyr::everything(),
#'     ~dplyr::recode(., IV = "III")
#'   )
#' )
#'
#' @export
refactor <- function(object, ..., inheret_parameters = TRUE) {

  changes <- rlang::enquos(..., .named = TRUE, .homonyms = "last")

  clust <- clustering
  clust$data <- clust$data %>% dplyr::mutate(...)
  clust$clustering <- clust$clustering %>% dplyr::mutate(...)

  only_changed_clusters <- clust$clustering %>%
    dplyr::transmute(...) %>%
    colnames()

  all_cluster_names <- colnames(clust$clustering)[-1]

  tmp <- data.frame(cluster_name = only_changed_clusters, recoded = TRUE)
  if (length(setdiff(all_cluster_names, only_changed_clusters)) != 0) {
    tmp <- dplyr::bind_rows(
      tmp,
      data.frame(
        cluster_name = setdiff(all_cluster_names, only_changed_clusters),
        recoded = FALSE
      )
    )
  }

  clust$clustering_parameters <- clustering$clustering_parameters %>%
    dplyr::left_join(tmp, by = "cluster_name") %>%
    dplyr::arrange(order(match(cluster_name, all_cluster_names)))

  # inheritance is only name based at the moment -- can we do better?????
  if (!inheret_parameters) {
    clust$clustering_parameters <- clust$clustering_parameters %>%
      dplyr::mutate(
        dplyr::across(
          .data$clustering:.data$p,
          ~ifelse(.data$recoded, NA, .)
        ),
        theta_list = dplyr::if_else(
          .data$recoded,
          list(NULL),
          .data$theta_list
        ),
        dplyr::across(
          .data$theta:.data$k,
          ~ifelse(.data$recoded, NA, .)
        )
      )
  }

  clust$distance_matrix <- NULL # is this too much? should we keep the inherited ??

  return(clust)
}
