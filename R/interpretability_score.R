



#INTERPRETABILITY SCORES   =====================================================
#' Interpretability Score
#'
#' -- one line explanation --
#'
#' @inheritParams plot-params
#' @param atc_levels 123
#'
#' @details
#' -- explain the deal with m = Inf, q = 0 --
#'
#' @return
#' a data frame -- bla bla --
#'
#' @export
#'
#' @examples
#' 1+1
#'
#' @export
interpretability_score <- function(clustering,
                                   only = NULL,
                                   additional_data = NULL,
                                   atc_levels = 1:5,
                                   limit = 10) {

  #
  clust <- enrich_clustering_parameters(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_methods <- selected_analyses$method

  cluster_AUC <- clust$data %>%
    dplyr::mutate(level1 = stringr::str_sub(!!rlang::sym(clust$variables$atc), 1, 1),
                  level2 = stringr::str_sub(!!rlang::sym(clust$variables$atc), 1, 3),
                  level3 = stringr::str_sub(!!rlang::sym(clust$variables$atc), 1, 4),
                  level4 = stringr::str_sub(!!rlang::sym(clust$variables$atc), 1, 5),
                  level5 = stringr::str_sub(!!rlang::sym(clust$variables$atc), 1, 7)) %>%
    dplyr::select(-!!rlang::sym(clust$variables$atc),
                  -dplyr::num_range("level", setdiff(1:5, atc_levels))) %>%
    tidyr::pivot_longer(cols = dplyr::num_range("level", 1:5),
                        names_to = "level", values_to = "atc") %>%
    tidyr::unite("timing", !!!rlang::syms(clust$variables$timing), sep = "") %>%
    dplyr::select(!!rlang::sym(clust$variables$id),
                  !!!rlang::syms(selected_methods),
                  .data$level, .data$atc, .data$timing) %>%
    tidyr::pivot_longer(cols = selected_methods,
                        names_to = "method", values_to = "cluster") %>%
    dplyr::count(.data$method, .data$cluster, .data$timing,
                 .data$level, .data$atc) %>%
    tidyr::complete(.data$cluster, .data$timing, .data$atc,
                    tidyr::nesting(method, level),
                    fill = list("n" = 0)) %>%
    dplyr::group_by(.data$method, .data$timing, .data$atc, .data$level) %>%
    dplyr::mutate(m = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$level) %>%
    tidyr::pivot_longer(cols = c("timing", "atc"),
                        names_to = "margin", values_to = "focus") %>%
    dplyr::group_by(.data$method, .data$cluster, .data$margin, .data$focus, .data$level) %>%
    dplyr::mutate(p_cluster = .data$n / sum(.data$n),
                  p_global = .data$m / sum(.data$m),
                  p_global_loo = (.data$m - .data$n) / sum(.data$m - .data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$p_cluster) %>%
    dplyr::group_by(.data$method, .data$cluster, .data$margin, .data$focus, .data$level) %>%
    dplyr::summarize(AUC = pracma::trapz(cumsum(.data$p_cluster),
                                         cumsum(.data$p_global)),
                     AUC_loo = pracma::trapz(cumsum(.data$p_cluster),
                                             cumsum(.data$p_global_loo)),
                     n_cluster_marginal = sum(.data$n),
                     n_global_marginal = sum(.data$m),
                     .groups = "drop") %>%
    dplyr::mutate(AUC = dplyr::coalesce(AUC, 1),
                  AUC_loo = dplyr::coalesce(AUC_loo, 1)) # is this a good decision ??


  standard_average <- cluster_AUC %>%
    tidyr::pivot_longer(cols = c("AUC", "AUC_loo"),
                        names_to = "calculation", values_to = "AUC") %>%
    dplyr::mutate(calculation = dplyr::if_else(.data$calculation == "AUC_loo",
                                               "Leave Cluster Out Reference",
                                               "Full Reference")) %>%
    dplyr::group_by(.data$method, .data$level, .data$calculation) %>%
    dplyr::summarize(
      average = (1 / (sum(.data$n_cluster_marginal))) *
        sum(.data$n_cluster_marginal * AUC),
      average_of_squares = (1 / (sqrt(sum(.data$n_cluster_marginal)))) *
        sqrt(sum(.data$n_cluster_marginal * AUC^2)),
      min = min(AUC + 1 * (.data$n_cluster_marginal < limit))) # what to do here?????

  return(standard_average)
}

#' @export
interpretability_score_plot <- function(clustering,
                                        only = NULL,
                                        additional_data = NULL,
                                        atc_levels = 1:5,
                                        by = method,
                                        limit = 10) {
  plot_data <- interpretability_score(clustering = clustering,
                                      only = {{ only }},
                                      additional_data = additional_data,
                                      atc_levels = atc_levels,
                                      limit = limit) %>%
    dplyr::left_join(clustering$clustering_parameters, by = "method") %>%
    dplyr::rename("ATC level" = "level")

  if (!is.null(additional_data)) {
    plot_data <- plot_data %>%
      dplyr::left_join(additional_data, by = "method")
  }


  p <- ggplot2::ggplot(data = plot_data,
                       ggplot2::aes(x = .data$average,
                                    y = .data$min,
                                    color = {{ by }})) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$`ATC level`,
                                     fill = {{ by }})) +
    ggplot2::geom_path(ggplot2::aes(group = .data$method)) +
    ggplot2::scale_shape_manual(values = c("level1" = 25,
                                           "level2" = 24,
                                           "level3" = 23,
                                           "level4" = 22,
                                           "level5" = 21),
                                labels = paste0("Level ", 1:5)) +
    ggplot2::facet_wrap(~.data$calculation, scales = "free") +
    ggplot2::labs(x = paste0("Average AUC\nof Cumulative Cluster Margin Incidence",
                             "\nversus Cumulative Reference Margin Incidence"),
                  y = paste0("Minimal AUC\nof Cumulative Cluster Margin Incidence",
                             "\nversus Cumulative Reference Margin Incidence"))

  return(p)

}
