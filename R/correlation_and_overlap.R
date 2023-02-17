


#  CLASSIFICATION CORRELATION   ================================================
#' Between cluster Cramer's V correlation
#'
#' -- one line explanation --
#'
#' @inheritParams plot-params
#' @param ... 123
#'
#' @return 123
#' @export
#'
#' @examples
#' # make a propper example !
#' clust <- medication_clustering(small, id = id, atc = atc, k = 3:5)
#' classification_correlation(clust, k == 5)
#' classification_correlation_plot(clust)
classification_correlation <- function(clustering,
                                       only = NULL,
                                       additional_data = NULL,
                                       ...) {
  clust <- enrich_clustering_parameters(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_methods <- selected_analyses$method

  res <- sapply(selected_methods, function(x) {
    sapply(selected_methods, function(y) {
      rcompanion::cramerV(clust$clustering[[x]],
                          clust$clustering[[y]], ...)[[1]]
    })
  })

  return(res)
}


#' @param ... 123
#'
#' @details
#' `clustering_gof_plot()` creates a correlation plot using .....
#'
#' @export
#'
#' @rdname classification_correlation
classification_correlation_plot <- function(clustering,
                                            only = NULL,
                                            additional_data = NULL,
                                            by = method,
                                            ...) {

  clust <- enrich_clustering_parameters(clustering, additional_data)

  plot_data <- classification_correlation(clust, {{only}})

  renaming <- clust$clustering_parameters
  # %>% dplyr::mutate(theta = as.character(.data$theta))

  # if (!is.null(additional_data)) {
  #   renaming <- renaming %>%
  #     dplyr::left_join(additional_data,
  #                      by = intersect(names(clustering$clustering_parameters),
  #                                     names(additional_data)))
  # }

  by_name <- deparse1(substitute(by))
  renaming <- renaming %>% dplyr::mutate(!!by_name := as.character({{ by }})) # !!by_name := forcats::fct_rev(factor({{ by }}))
  rownames(plot_data) <- colnames(plot_data) <- renaming[
    match(rownames(plot_data),renaming$method),] %>%
    dplyr::pull(!!rlang::ensym(by_name)) %>%
    as.character()

  p <- GGally::ggcorr(data = NULL,
                      cor_matrix = plot_data,
                      limits = c(0, 1),
                      midpoint = 0.5,
                      name = "Cramer's V",
                      ...)
  return(p)
}




#  CLASSIFICATION OVERLAP   ====================================================
#' TO DO
#'
#' -- one line explanantion --
#'
#' @inheritParams plot-params
#' @param ... 123
#'
#' @details
#' -- TO DO --
#'
#' @return
#' 123
#'
#' @export
#'
#' @examples
#' clust <- medication_clustering(small, id = id, atc = atc, k = 3:5)
#' classification_overlap(clust)
classification_overlap <- function(clustering,
                                   only = NULL,
                                   by = method,
                                   ...) {
  # to do:
  # counting overlapping or something
  return(1)
}





