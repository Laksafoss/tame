

#' @details
#' `cluster_frequency_plot()` illustrates the frequencies in a
#' [ggplot2::ggplot()].
#'
#' @rdname cluster_frequency
#' @export
cluster_frequency_plot <- function(clustering = NULL,
                                   only = NULL,
                                   by = k,
                                   facet = clustering,
                                   additional_data = NULL) {

  faceted <- !rlang::quo_is_null(rlang::enquo(facet))
  clust <- enrich_clustering_parameters(clustering, additional_data)

  plot_data <- cluster_frequency(clust, only = {{ only }})
  #%>% dplyr::left_join(clust$clustering_parameters, by = "cluster_name")

  by_name <- rlang::as_string(deparse1(substitute(by)))
  plot_data <- plot_data %>%
    dplyr::mutate(
      !!by_name := forcats::fct_rev(factor({{ by }})),
      .facet = if (faceted) {as.character({{ facet }})} else {1}) %>%
    dplyr::rename(`Frequency of cluster assignment` = .data$percent) %>%
    dplyr::arrange(!!by_name, dplyr::desc(`Frequency of cluster assignment`))


  p <- ggplot2::ggplot(plot_data, ) +
    ggplot2::geom_bar(ggplot2::aes(
      y = !!rlang::ensym(by_name),
      x = .data[["Frequency of cluster assignment"]],
      fill = .data$cluster),
      position = ggplot2::position_stack(reverse = TRUE),
      stat = "identity") +
    ggplot2::scale_x_continuous(name = "Frequency",
                                labels = function(x) paste0(x, "%"))

  if (! faceted) {
    return(p)
  }

  p <- p +
    ggplot2::facet_grid(rows = ".facet", drop = TRUE,
                        scales = "free_y", space = "free_y")

  return(p)
}









#' @param labels A logical indicating whether or not the ATC codes should be
#'   displayed as text labels on the plot. \code{TRUE} is default.
#' @param label_size A numeric giving the text size of the ATC code labels on
#'   the plot.
#' @param remaning_color A character hex color code specifying the color used to
#'   illustrate the remaining less frequent ATC codes.
#'
#' @export
#'
#' @rdname atc_frequency
atc_frequency_plot <- function(clustering,
                               only = NULL,
                               clusters = NULL,
                               cluster_wise = TRUE,
                               by = cluster_name, # do we need to add a facet option here?
                               m = 3,
                               q = 0.05,
                               additional_data = NULL,
                               labels = TRUE,
                               label_size = 2,
                               remaning_color = "#BEBEBE") {

  clust <- enrich_clustering_parameters(clustering, additional_data)
  plot_data <- atc_frequency(clust,
                             {{ only }},
                             {{ clusters }},
                             cluster_wise,
                             m = m,
                             q = q,
                             additional_data = additional_data) %>%
    dplyr::left_join(clust$clustering_parameters, by = "cluster_name")

  chosen_format <- dplyr::if_else(cluster_wise,
                                  "ATC frequency within cluster",
                                  "ATC frequency across cluster")

  by_name <- deparse1(substitute(by))
  plot_data <- plot_data %>%
    dplyr::mutate(!!by_name := forcats::fct_rev(factor({{ by }}))) %>%
    dplyr::rename(`ATC frequency within cluster` = .data$p_cluster,
                  `ATC frequency across cluster` = .data$p_analysis)


  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[chosen_format]],
                                 y = !!rlang::ensym(by_name),
                                 group = .data[[clust$variables$atc]])) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[[clust$variables$atc]]),
                      color = "black",
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(cols = dplyr::vars(.data$cluster))


  if (cluster_wise) {
    remaning_data <- plot_data %>%
      dplyr::select(-!!clust$variables$atc) %>%
      dplyr::group_by_at(dplyr::vars(-n,
                                     -`ATC frequency within cluster`,
                                     -`ATC frequency across cluster`)) %>%
      dplyr::summarise(
        see_through = sum(.data[[chosen_format]]),
        `#BEBEBE` = 1 - sum(.data[[chosen_format]]),
        .groups = "drop") %>%
      tidyr::pivot_longer(cols = c("see_through", remaning_color),
                          names_to = "group",
                          values_to = "ps") %>%
      dplyr::mutate(
        fill_col = dplyr::na_if(.data$group, "see_through"),
        dummy_linetype = dplyr::if_else(.data$fill_col == remaning_color,
                                        "Remaining",
                                        NA_character_))


    p <- p +
      ggplot2::geom_col(data = remaning_data,
                        ggplot2::aes(x = .data$ps,
                                     y = !!rlang::ensym(by_name),
                                     group = .data$group,
                                     linetype = .data$dummy_linetype),
                        fill = remaning_data$fill_col, color = "black",
                        position = ggplot2::position_stack(reverse = FALSE)) +
      ggplot2::scale_linetype_discrete( # when there is no remaning this should not be drawn !!!!
        name = NULL,
        breaks = c("Remaining"),
        guide = ggplot2::guide_legend(override.aes = list(fill = remaning_color)))
  }

  if (labels) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = .data[[clustering$variables$atc]]),
                         position = ggplot2::position_stack(vjust = 0.5,
                                                            reverse = TRUE),
                         size = label_size)
  }

  return(p)
}



###   ATC LANDSCAPE PLOT   =====================================================
#' @param limit 123
#' @export
#'
#' @rdname atc_frequency
atc_landscape_plot <- function(clustering,
                               only = NULL,
                               clusters = NULL,
                               cluster_wise = TRUE,
                               by = cluster_name, # do we need to add a facet option here?
                               limit = 0.01,
                               atc_codes = NULL,
                               additional_data = NULL,
                               p_lim = NULL) {

  clust <- enrich_clustering_parameters(clustering, additional_data)


  plot_data <- atc_frequency(clust,
                             {{ only }},
                             {{ clusters }},
                             cluster_wise,
                             m = Inf,
                             q = 0,
                             additional_data = additional_data)

  if (!is.null(atc_codes)) {
    by_name <- c("cluster_name", "cluster", "atc_codes")
    names(by_name) <- c("cluster_name", "cluster", clust$variables$atc)
    selects_atc <- plot_data %>%
      dplyr::select(cluster_name, cluster) %>%
      dplyr::distinct() %>%
      dplyr::left_join(data.frame("atc_codes" = atc_codes), by = character())
    plot_data <- plot_data %>%
      dplyr::right_join(selects_atc, by = by_name) %>%
      dplyr::mutate(n = dplyr::coalesce(.data$n, 0L),
                    p_analysis = dplyr::coalesce(.data$p_analysis, 0),
                    p_cluster = dplyr::coalesce(.data$p_cluster, 0L))

  }

  plot_data <- plot_data %>%
    dplyr::left_join(clust$clustering_parameters, by = "cluster_name")


  if (limit > 0) {
    plot_data <- plot_data %>%
      dplyr::group_by(.data$cluster_name, !!rlang::sym(clust$variables$atc)) %>%
      dplyr::mutate(qualifies = max(.data$p_cluster) >= limit) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$qualifies)
  }

  if (is.null(p_lim)) {p_lim <- max(plot_data$p_cluster)}

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_col(ggplot2::aes(y = !!rlang::sym(clust$variables$atc),
                                   x = .data$p_cluster,
                                   fill = !!rlang::sym(clust$variables$atc))) +
    ggplot2::facet_grid(cols = dplyr::vars(.data$cluster),
                        rows = dplyr::vars({{ by }}),
                        scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, p_lim)) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 0),
                   axis.title = ggplot2::element_blank())

  return(p)
}







#' @param within A character naming the summarizing context. It defaults to
#'   `cluster`, but may also be `analysis` and `n_unique_medications`.
#'
#'   TODO write about each of these
#' @param aspect A character naming the aspect of the summarizing context of
#'   interest. If defaults to `people`, but may also be `medications`.
#'
#'   TODO write about each of these
#'
#' @details
#' -- something about the plotting --
#'
#' @export
#'
#' @rdname medication_amount
medication_amount_plot <- function(clustering,
                                   only = NULL,
                                   clusters = NULL,
                                   by = cluster_name,
                                   within = "cluster",
                                   aspect = "people",
                                   count_grouper = function(x) {
                                     cut(x,
                                         breaks = c(0, 1, 2, Inf),
                                         labels = c("1", "2", "3+"))},
                                   additional_data = NULL) {

  # find data for plotting
  plot_data <- medication_amount(clustering,
                                 {{ only }},
                                 {{ clusters }},
                                 count_grouper,
                                 additional_data = additional_data) %>%
    dplyr::left_join(clustering$clustering_parameters, by = "cluster_name")

  # choose format
  chosen_format <- names(plot_data)[stringr::str_detect(names(plot_data), paste0("p_", aspect, ".+", within))]
  if (length(chosen_format) != 1) {
    stop("One or both of the chosen 'aspect' or 'within' are invalid.")
  }
  title <- switch(chosen_format,
                  p_people_analysis ="Percentage of People ...",
                  p_people_cluster = "Percentage of People ...",
                  p_people_with_n_unique_medications = "Percentage of People ...",
                  p_medications_in_analysis = "Percentage of Medications ...",
                  p_medications_in_cluster = "Percentage of Medications ...",
                  p_medications_with_n_unique_medications = "Percentage of Medications ...")

  by_name <- rlang::as_string(deparse1(substitute(by)))
  plot_data <- plot_data %>%
    dplyr::select(.data$cluster_name,
                  .data$cluster,
                  .data$n_unique_exposures,
                  {{ by }},
                  !!rlang::sym(chosen_format)) %>%
    dplyr::mutate(!!by_name := forcats::fct_rev(factor({{ by }}))) %>%
    dplyr::rename(
      !!title := !!rlang::sym(chosen_format),
      `Number of unique medications` = .data$n_unique_exposures)


  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_col(ggplot2::aes(x = !!rlang::sym(title),
                                   y = !!rlang::sym(by_name),
                                   fill = .data$`Number of unique medications`),
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_grid(cols = dplyr::vars(.data$cluster)) +
    ggplot2::scale_x_continuous(labels = scales::percent)

  return(p)

}









#' @param split_by_cluster 123
#' @param draw_individual 123
#'
#' @export
#'
#' @rdname atc_frequency
timing_average_plot <- function(clustering,
                                only = NULL,
                                clusters = NULL,
                                cluster_wise = TRUE,
                                by = cluster_name, # do we need to add a facet option here?
                                split_by_cluster = FALSE,
                                draw_individual = FALSE,
                                additional_data = NULL) {
  clust <- enrich_clustering_parameters(clustering, additional_data)
  plot_data <- timing_average(clust,
                              {{ only }},
                              {{ clusters }},
                              additional_data = additional_data)
  plot_data$average <- plot_data$average %>%
    dplyr::left_join(clust$clustering_parameters, by = "cluster_name") %>%
    tidyr::pivot_longer(cols = clust$variables$timing,
                        names_to = "timing",
                        values_to = "percent")

  plot_data$individual <- plot_data$individual %>%
    dplyr::left_join(clust$clustering_parameters, by = "cluster_name") %>%
    dplyr::mutate(individual_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = clust$variables$timing,
                        names_to = "timing",
                        values_to = "trajectory")


  if (split_by_cluster) {
    if (draw_individual) {
      p <- ggplot2::ggplot(data = plot_data$average) +
        ggplot2::geom_line(ggplot2::aes(x = .data$timing, y = .data$percent,
                                        group = .data$cluster),
                           size = 2) +
        ggplot2::geom_line(data = plot_data$individual,
                           ggplot2::aes(x = .data$timing, y = .data$trajectory,
                                        group = .data$individual_id)) +
        ggplot2::facet_grid(rows = dplyr::vars({{ by }}),
                            cols = dplyr::vars(.data$cluster))
    } else {
      p <- ggplot2::ggplot(data = plot_data$average) +
        ggplot2::geom_line(ggplot2::aes(x = .data$timing, y = .data$percent,
                                        group = .data$cluster)) +
        ggplot2::facet_grid(rows = dplyr::vars({{ by }}),
                            cols = dplyr::vars(.data$cluster))
    }
  } else {
    p <- ggplot2::ggplot(data = plot_data$average) +
      ggplot2::geom_line(ggplot2::aes(x = .data$timing, y = .data$percent,
                                      group = .data$cluster,
                                      color = .data$cluster)) +
      ggplot2::facet_grid(rows = dplyr::vars({{ by }}))
  }


  return(p)
}





#' @param split_by_cluster 123
#' @param draw_individual 123
#'
#' @export
#'
#' @rdname atc_frequency
timing_atc_interaction_plot <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    atc_groups = data.frame(regex = paste0("^", LETTERS), atc_groups = LETTERS), # this need to be clust$variables$atc !!!!!! <-<-<-<-<-<-
    by = cluster_name, # we need to ensure that by is always a factor when plotting!!,
    conditional = TRUE,
    additional_data = NULL) {

  clust <- enrich_clustering_parameters(clustering, additional_data)
  plot_data <- timing_atc_interaction(clust,
                                      {{ only }},
                                      {{ clusters }},
                                      {{ atc_groups }},
                                      additional_data = additional_data)

  if (!conditional) {
    plot_data <- plot_data %>%
      dplyr::group_by(.data$cluster_name, .data$cluster, .data$atc_groups) %>%
      dplyr::summarise(dplyr::across(clust$variables$timing,
                                     ~ sum(. * .data$n / .data$cluster_size)),
                       .groups = "drop")
  } else {
    plot_data <- plot_data %>%
      dplyr::group_by(.data$cluster_name, .data$cluster, .data$atc_groups) %>%
      dplyr::summarise(dplyr::across(clust$variables$timing,
                                     ~ sum(. * .data$n) / sum(.data$n)),
                       .groups = "drop")
  }
  plot_data <- plot_data %>%
    dplyr::mutate(plot_group = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = clust$variables$timing,
                        names_to = "timing",
                        values_to = "trajectory") %>%
    dplyr::left_join(clustering$clustering_parameters, by = "cluster_name")


  if (length(unique(dplyr::pull(plot_data, {{ by }}))) != 1) {
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x = .data$timing, y = .data$trajectory,
                                      group = .data$plot_group,
                                      color = {{ by }})) +
      ggplot2::facet_grid(rows = dplyr::vars(.data$atc_groups),
                          cols = dplyr::vars(.data$cluster))
  } else {
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x = .data$timing, y = .data$trajectory,
                                      group = .data$plot_group)) +
      ggplot2::facet_grid(rows = dplyr::vars(.data$atc_groups),
                          cols = dplyr::vars(.data$cluster))
  }

  return(p)
}
