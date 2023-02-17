



#' Internal method for goodness of fit calculations
#'
#' @param keys fill
#' @param cluster_assignment fill
#' @param distance_matrix fill
#'
#' @return 1+1
#'
goodness_of_fit_characteristic <- function(keys,
                                           cluster_assignment,
                                           distance_matrix) {


  #   ---   prep list of ids by clustering method and assigned cluster   -----

  cluster_names <- names(cluster_assignment)[-1]

  cluster_key <- keys$key %>%
    dplyr::left_join(cluster_assignment, by = ".internal_character_id")

  cluster_key_by_cluster_id <- cluster_key %>%
    dplyr::select(.data$unique_pattern_key, dplyr::all_of(cluster_names)) %>% # used to be !!!cluster_names
    dplyr::distinct() %>%
    tidyr::pivot_longer(cols = cluster_names,
                        names_to = "method",
                        values_to = "cluster") %>%
    tidyr::nest(index = .data$unique_pattern_key)

  #  ---   find all clustering method and cluster combinations   -------------
  all_cluster_combinations <- dplyr::full_join(
    cluster_key_by_cluster_id,
    cluster_key_by_cluster_id,
    by = "method")


  #   ===   Validation Measures   ==============================================

  # extract within clustering method combinations and compute basic comparisons
  within_clustering_combinations <- all_cluster_combinations %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mean = mean(distance_matrix[.data$index.x[[1]], .data$index.y[[1]]]),
      ssq = sum(distance_matrix[.data$index.x[[1]], .data$index.y[[1]]]^2),
      min = min(distance_matrix[.data$index.x[[1]], .data$index.y[[1]]]),
      max = max(distance_matrix[.data$index.x[[1]], .data$index.y[[1]]])) %>%
    dplyr::ungroup()

  # extract within cluster measures / compactness measures
  cluster_compactness <- within_clustering_combinations %>%
    dplyr::filter(.data$cluster.x == .data$cluster.y) %>%
    dplyr::rename("cluster" = "cluster.x") %>%
    dplyr::select(-.data$cluster.y, -.data$index.x, -.data$index.y)

  # extract between cluster measures / separation measures
  cluster_separation <- within_clustering_combinations %>%
    dplyr::filter(.data$cluster.x != .data$cluster.y) %>%
    dplyr::select(-.data$index.x, -.data$index.y)

  #   ===   Goodness of Fit   ==================================================

  # summarized compactness
  gof_compactness <- cluster_compactness %>%
    dplyr::mutate(k = 1) %>%
    dplyr::group_by(.data$method) %>%
    dplyr::summarise_at(c("k", "ssq", "mean", "min", "max"), ~ sum(.))

  # summarized separation
  gof_separation <- cluster_separation %>%
    dplyr::group_by(.data$method) %>%
    dplyr::summarise_at(c("ssq", "mean", "min", "max"), ~ sum(.))

  # goodness of fit
  gof <- dplyr::left_join(gof_compactness,
                          gof_separation,
                          by = "method",
                          suffix = c("_compactness", "_separation")) %>%
    dplyr::mutate(
      n = nrow(distance_matrix),
      dunn_index = .data$min_separation / .data$max_compactness,
      calinski_harabasz_index = (.data$ssq_separation / .data$ssq_compactness) *
        ((.data$n - .data$k) / (.data$k - 1))) %>%
    dplyr::select(.data$method, .data$k, .data$n,
                  .data$dunn_index, .data$calinski_harabasz_index)

  # can we use cluster::silhouette as well ???


  #   ===   return results   ===================================================
  return(list(gof = gof,
              compactness = cluster_compactness,
              separation = cluster_separation))
}



#  GOODNESS OF FIT   ===========================================================
#' Goodness of fit plotting method
#'
#' The function `clustering_gof_plot()` plots the Dunn and Calinski-Harabasz
#' indexes for the chosen analyses.
#'
#' @inheritParams plot-params
#'
#' @details
#' -- write something here --
#'
#' @export
#'
#' @examples
#' clust <- medication_clustering(small, id = id, atc = atc, k = 3:5)
#' clustering_gof_plot(clust)
clustering_gof_plot <- function(clustering,
                                only = NULL,
                                by = method,
                                additional_data = NULL) {

  clust <- enrich_clustering_parameters(clustering, additional_data)

  plot_data <- method_selector(clust, {{ only }}) %>%
    dplyr::left_join(clust$gof, by = c("method", "k")) %>%
    tidyr::pivot_longer(cols = c("dunn_index", "calinski_harabasz_index"),
                        names_to = "index",
                        values_to = "value") %>%
    dplyr::mutate(
      index = factor(.data$index,
                     levels = c("dunn_index", "calinski_harabasz_index"),
                     labels = c("Dunn Index", "Calinski-Harabasz Index")))



  by_name <- rlang::as_string(deparse1(substitute(by)))
  plot_data <- plot_data %>%
    dplyr::mutate(!!by_name := factor({{ by }}))

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(ggplot2::aes(x = !!rlang::ensym(by_name),
                                     y = .data$value)) +
    ggplot2::facet_wrap(~.data$index, scales = "free", drop = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1),
                   axis.title.y = ggplot2::element_blank())

  return(p)
}




#  COMPACTNESS PLOT   ==========================================================
#' Cluster compactness plot
#'
#' -- oneline explanantion --
#'
#' @inheritParams plot-params
#'
#' @details
#' -- plot explanantion --
#'
#' @export
#'
#' @examples
#' clust <- medication_clustering(small, id = id, atc = atc, k = 3:5)
#' compactness_plot(clust, by = k, clusters = I:III)
compactness_plot <- function(clustering,
                             only,
                             clusters = I:III,
                             additional_data = NULL,
                             by = method) {

  clust <- enrich_clustering_parameters(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_methods <- selected_analyses$method

  plot_data <- selected_analyses %>%
    dplyr::left_join(clust$compactness, by = "method") %>%
    dplyr::filter(.data$cluster %in% selected_clusters)
  # %>% dplyr::mutate(theta = as.character(.data$theta))

  if (!is.null(additional_data)) {
    plot_data <- plot_data  %>%
      dplyr::left_join(additional_data,
                       by = intersect(names(plot_data),
                                      names(additional_data)))
  }

  p <- plot_data %>%
    tidyr::pivot_longer(cols = c("mean", "ssq", "max"),
                        names_to = "statistic",
                        values_to = "value") %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = {{ by }},
                                   y = .data$value)) +
    ggplot2::facet_grid(statistic ~ cluster, scales = "free_y")

  return(p)
}


