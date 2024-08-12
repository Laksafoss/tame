#' Construct time scale for summary plot
#'
#' The `construct_time_scale()` function constructs a time scale for the summary
#' plot.
#'
#' @param object A `medic` or `summary.medic` object.
#' @param n_breaks An integer specifying the number of breaks in the time scale.
#'
#' @return
#' A list with the following components:
#' * `translator` a data frame with the time scale.
#' * `mid` a numeric value specifying the mid point of the time scale.
#' * `width` a numeric value specifying the width of the time scale.
#' * `breaks` a character vector with the breaks in the time scale.
#'
#' @keywords internal
construct_time_scale <- function(object, n_breaks = 5) {

  ns <- seq_along(object$variables$timing)
  translator <- data.frame(
    timing_names = object$variables$timing,
    Timing = ns - 1 - diff(range(ns)) / 2
  )

  mid <- 0
  width <- -translator$Timing[1] * 2

  if (length(ns) < n_breaks) {
    chosen_n_breaks <- length(ns)
  } else {
    # spiral out until we find a nice number of breaks
    start <- 1
    stop <- length(ns)
    testing_breaks <- n_breaks
    updator <- 1
    while (!all(seq(start, stop, length.out = testing_breaks) %% 1 == 0)) {
      testing_breaks <- testing_breaks + updator
      updator <- -1 * (updator + 1)
    }
    chosen_n_breaks <- testing_breaks
  }

  breaks <- translator[
    seq(1, nrow(translator), length.out = chosen_n_breaks),
  ] |>
    dplyr::pull("Timing", name = "timing_names")

  return(
    list(
      translator = translator,
      mid = mid,
      width = width,
      breaks = breaks
    )
  )
}


#' Flatten trajectory data
#'
#' The `flatten_trajctories()` function flattens the individual and average
#' trajectories.
#'
#' @param object A `medic` or `summary.medic` object.
#' @param individual A logical value or an integer specifying the number of
#'  individuals to sample.
#'
#' @return
#' A data frame with the flattened trajectories.
#'
#' @keywords internal
flatten_trajctories <- function(object, individual) {
  if (!individual) {
    return(object$average |> dplyr::mutate(Origin = "average"))
  }

  if (is.numeric(individual)) {
    class(object) <- class(object)[-1]
    weight <- intersect(
      names(object$individual),
      c("Count", "Number of Medications with Timing Trajectory")
    )
    groups <- intersect(
      names(object$individual),
      c("Clustering", "Cluster", "ATC Groups")
    )
    object$individual <- object$individual |>
      dplyr::group_by(!!!dplyr::syms(groups)) |>
      dplyr::slice_sample(n = individual, weight_by = !!dplyr::sym(weight)) |>
      dplyr::ungroup()
    return(dplyr::bind_rows(object, .id = "Origin"))
  }

  class(object) <- class(object)[-1]
  return(dplyr::bind_rows(object, .id = "Origin"))
}





#' Construct plot data for summary plot
#'
#' The `construct_plot_data()` function constructs the data for the summary
#' plot.
#'
#' @param object A `medic` or `summary.medic` object.
#' @param time_scale A list with the time scale.
#' @param plot_individual A logical value specifying if individual trajectories
#' should be plotted.
#' @param labels A logical value specifying if labels should be plotted.
#' @param alpha_individual A numeric value specifying the transparency of the
#' individual trajectories.
#' @param ... Additional arguments passed to the function.
#'
#' @return
#' A data frame with the plot data.
#'
#' @keywords internal
construct_plot_data <- function(
  object,
  time_scale,
  plot_individual,
  labels,
  alpha_individual = 0.1,
  ...
) {

  cluster_names <- object$cluster_frequency |>
    dplyr::mutate(
      cluster_name = paste0(
        .data$Cluster,
        "\nn = ",
        formatC(.data$Count, format = "d", big.mark = " "),
        "\n(",
        dplyr::if_else(
          .data$Percent == 100,
          "100",
          formatC(.data$Percent, digits = 1, format = "f")
        ),
        "%)"
      ),
      cluster_name = factor(.data$cluster_name, levels = .data$cluster_name)
    )

  medication_frequencies <- object$medication_frequency |>
    dplyr::mutate(
      row_facet = "Medication Frequencies",
      y = .data$`Percent of Medication in Cluster`,
      part = "medication_frequency"
    )

  comedication_counts <- object$comedication_count |>
    dplyr::mutate(
      row_facet = "Comedication Count",
      y = .data$`Percentage of People in Cluster`,
      part = "comedication_count"
    )

  timing_trajectories <- object$timing_trajectory |>
    flatten_trajctories(plot_individual) |>
    dplyr::mutate(
      row_facet = "Average Trajectory",
      part = "timing_trajectory",
      line_group = dplyr::row_number(),
      Origin = dplyr::if_else(.data$Origin == "average", 1, alpha_individual)
    ) |>
    tidyr::pivot_longer(
      cols = object$variables$timing,
      names_to = "timing_names",
      values_to = "y"
    )

  timing_atc_groups <- object$timing_atc_group |>
    flatten_trajctories(plot_individual) |>
    dplyr::mutate(
      row_facet = .data$`ATC Groups`,
      part = "timing_atc_group",
      line_group = dplyr::row_number(),
      Origin = dplyr::if_else(.data$Origin == "average", 1, alpha_individual)
    ) |>
    tidyr::pivot_longer(
      cols = object$variables$timing,
      names_to = "timing_names",
      values_to = "y"
    )

  if (labels) {
    timing_atc_group_labels <- object$timing_atc_group$average |>
      dplyr::mutate(
        label = sprintf(
          "%d (%.0f)",
          .data$`Number of Medications`,
          100 * .data$`Percentage of Medications`
        ),
        row_facet = .data$`ATC Groups`,
        part = "label",
        y = 0
      ) |>
      dplyr::select(-object$variables$timing)

  } else {
    timing_atc_group_labels <- NULL
  }

  plot_data <- dplyr::bind_rows(
    comedication_counts,
    medication_frequencies,
    timing_trajectories,
    timing_atc_groups,
    timing_atc_group_labels
  ) |>
    dplyr::left_join(cluster_names, by = c("Clustering", "Cluster"))  |>
    dplyr::left_join(time_scale$translator, by = "timing_names", copy = TRUE) |>
    dplyr::mutate(
      Timing = tidyr::replace_na(.data$Timing, 0),
      row_facet = factor(
        .data$row_facet,
        levels = c(
          "Comedication Count",
          "Medication Frequencies",
          "Average Trajectory",
          as.character(sort(unique(timing_atc_groups$`ATC Groups`)))
        )
      )
    ) |>
    dplyr::select(
      dplyr::any_of(
        c(
          "Cluster" = "cluster_name",
          "row_facet",
          "part",
          "Timing",
          "y",
          "Medication Count",
          object$variables$atc,
          "ATC Groups",
          "alpha" = "Origin",
          "line_group",
          "label",
          "timing_names"
        )
      )
    )

  attr(plot_data, "atc") <- object$variables$atc

  return(plot_data)
}


#' Construct color scales for summary plot
#'
#' The `construct_color_scales()` function constructs the color scales for the
#' summary plot.
#'
#' @param plot_data A data frame with the plot data.
#' @param comedication_count_colors A character vector with the colors for the
#' comedication count.
#' @param medication_frequency_colors A character vector with the colors for the
#' medication frequency.
#' @param timing_atc_group_colors A character vector with the colors for the
#' ATC groups.
#'
#' @return
#' A list with the color scales:
#' * `comedication_count_colors` a character vector with the colors for the
#'   comedication count.
#' * `medication_frequency_colors` a character vector with the colors for the
#'   medication frequency.
#' * `medication_frequency_linetype` a numeric vector with the linetypes for the
#'   medication frequency.
#' * `timing_atc_group_colors` a character vector with the colors for the ATC
#'   groups.
#'
#' @keywords internal
construct_color_scales <- function(
  plot_data,
  comedication_count_colors = NULL,
  medication_frequency_colors = NULL,
  timing_atc_group_colors = NULL
) {

  if (is.null(comedication_count_colors)) {
    unique_comedication_count <- na.omit(unique(plot_data$`Medication Count`))
    comedication_count_colors <- scales::viridis_pal(
      begin = 0.1, end = 1
    )(length(unique_comedication_count))
    if (!is.null(levels(plot_data$`Medication Count`))) {
      names(comedication_count_colors) <- intersect(
        levels(plot_data$`Medication Count`),
        unique_comedication_count
      )
    } else {
      names(comedication_count_colors) <- sort(unique_comedication_count)
    }
  }

  unique_atc <- na.omit(unique(plot_data[[attr(plot_data, "atc")]]))
  medication_frequency_linetype <- seq_along(unique_atc)
  names(medication_frequency_linetype) <- unique_atc
  if (is.null(medication_frequency_colors)) {
    medication_frequency_colors <- scales::hue_pal()(length(unique_atc))
    if (!is.null(levels(plot_data[[attr(plot_data, "atc")]]))) {
      names(medication_frequency_colors) <- intersect(
        levels(plot_data[[attr(plot_data, "atc")]]),
        unique_atc
      )
    } else {
      names(medication_frequency_colors) <- c(
        sort(unique_atc[unique_atc != "Remaining"]),
        if (any(unique_atc == "Remaining")) "Remaining"
      )
    }
    medication_frequency_colors[
      names(medication_frequency_colors) == "Remaining"
    ] <- "#777777"
  }

  if (is.null(timing_atc_group_colors)) {
    unique_atc_groups <- na.omit(unique(plot_data$`ATC Groups`))
    if (length(unique_atc_groups) <= 8) {
      timing_atc_group_colors <- scales::brewer_pal("qual")(
        length(unique_atc_groups)
      )
    } else {
      timing_atc_group_colors <- scales::hue_pal()(
        length(unique_atc_groups)
      )
    }
    if (!is.null(levels(plot_data$`ATC Groups`))) {
      names(timing_atc_group_colors) <- intersect(
        levels(plot_data$`ATC Groups`),
        unique_atc_groups
      )
    } else {
      names(timing_atc_group_colors) <- unique_atc_groups
    }
  }

  return(
    list(
      "comedication_count_colors" = comedication_count_colors,
      "medication_frequency_colors" = medication_frequency_colors,
      "medication_frequency_linetype" =  medication_frequency_linetype,
      "timing_atc_group_colors" = timing_atc_group_colors
    )
  )
}
