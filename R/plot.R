#' Plot Cluster Frequency
#'
#' This function plots the cluster frequency.
#'
#' @param object The object containing the cluster frequency data.
#' @param scale The scale of the y-axis. Must be either "percent" or "count".
#' @param with_population Logical value indicating whether to include the
#'   population cluster.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{cluster_frequency}}
#' @seealso \code{\link{plot_medication_frequency}}
#' @seealso \code{\link{plot_comedication_count}}
#' @seealso \code{\link{plot_timing_trajectory}}
#' @seealso \code{\link{plot_timing_atc_group}}
#' @seealso \code{\link{plot_summary}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#' clust |> plot_cluster_frequency()
#' clust |> cluster_frequency() |> plot_cluster_frequency()
#' clust |> summary() |> plot_cluster_frequency()
#'
#' @rdname plot_cluster_frequency
#' @export
plot_cluster_frequency <- function(object, ...) {
  UseMethod("plot_cluster_frequency", object)
}

#' @rdname plot_cluster_frequency
#' @export
plot_cluster_frequency.medic <- function(object, ...) {
  plot_cluster_frequency(cluster_frequency(object, ...), ...)
}

#' @rdname plot_cluster_frequency
#' @export
plot_cluster_frequency.summary.medic <- function(object, ...) {
  if (is.null(object$cluster_frequency)) {
    stop("The summary must contain a 'cluster_frequency' summary.")
  }
  plot_cluster_frequency(object$cluster_frequency, ...)
}

#' @rdname plot_cluster_frequency
#' @export
plot_cluster_frequency.cluster_frequency <- function(
  object,
  scale = "percent",
  with_population = FALSE,
  ...
) {

  chosen_y <- switch(
    scale,
    "percent" = "Percent",
    "count" = "Count",
    stop("'scale' is not recognised. Must be either 'percent' or 'count'.")
  )

  p <- object |>
    dplyr::filter(
      if (with_population) TRUE else .data$Cluster != "Population"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data$Cluster, y = !!dplyr::sym(chosen_y))
    ) +
    ggplot2::geom_col()

  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(rows = "Clustering")
  }

  return(p)
}




#' Plot Medication Frequency
#'
#' This function plots the medication frequency.
#'
#' @param object The object containing the medication frequency data.
#' @param scale The scale of the y-axis. Must be either "percent" or "count".
#' @param scope The scope of the plot. Must be one of "cluster", "global" or
#'  "medication".
#' @param with_population Logical value indicating whether to include the
#'  population cluster.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{medication_frequency}}
#' @seealso \code{\link{plot_cluster_frequency}}
#' @seealso \code{\link{plot_comedication_count}}
#' @seealso \code{\link{plot_timing_trajectory}}
#' @seealso \code{\link{plot_timing_atc_group}}
#' @seealso \code{\link{plot_summary}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' clust |> plot_medication_frequency()
#' clust |> medication_frequency() |> plot_medication_frequency()
#' clust |> summary() |> plot_medication_frequency()
#'
#' @rdname plot_medication_frequency
#' @export
plot_medication_frequency <- function(object, ...) {
  UseMethod("plot_medication_frequency", object)
}

#' @rdname plot_medication_frequency
#' @export
plot_medication_frequency.medic <- function(object, ...) {
  plot_medication_frequency(medication_frequency(object, ...), ...)
}

#' @rdname plot_medication_frequency
#' @export
plot_medication_frequency.summary.medic <- function(object, ...) {
  if (is.null(object$medication_frequency)) {
    stop("The summary must contain a 'medication_frequency' summary.")
  }
  plot_medication_frequency(object$medication_frequency, ...)
}

#' @rdname plot_medication_frequency
#' @export
plot_medication_frequency.medication_frequency <- function(
  object,
  scale = "percent",
  scope = "cluster",
  with_population = FALSE,
  ...
) {

  chosen_y <- if (scale == "percent" && scope == "cluster") {
    "Percent of Medication in Cluster"
  } else if (scale == "percent" && scope == "global") {
    "Percent of All Cluster"
  } else if (scale == "percent" && scope == "medication") {
    "Percent of ATC code"
  }else if (scale == "count") {
    "Count"
  } else {
    stop(
      paste0(
        "'scale' and 'scope' combination is not known. Must be one of\n",
        "scale = 'percent', scope = 'cluster'\n",
        "scale = 'percent', scope = 'global'\n",
        "scale = 'percent', scope = 'medication'\n",
        "scale = 'count'"
      )
    )
  }

  atc_name <- attr(object, "atc")

  p <- object |>
    dplyr::filter(
      if (with_population) TRUE else .data$Cluster != "Population"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = !!dplyr::sym(atc_name), y = !!dplyr::sym(chosen_y))
    ) +
    ggplot2::geom_col()


  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(!!dplyr::sym("Clustering")),
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  } else {
    p <- p + ggplot2::facet_grid(
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  }

  return(p)
}


#' Plot Comedication Count
#'
#' This function plots the comedication count.
#'
#' @param object The object containing the comedication count data.
#' @param scale The scale of the y-axis. Must be either "percent" or "count".
#' @param scope The scope of the plot. Must be one of "cluster", "global" or
#'   "medication count".
#' @param focus The focus of the plot. Must be either "people" or "medication".
#' @param with_population Logical value indicating whether to include the
#'   population cluster.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{comedication_count}}
#' @seealso \code{\link{plot_cluster_frequency}}
#' @seealso \code{\link{plot_medication_frequency}}
#' @seealso \code{\link{plot_timing_trajectory}}
#' @seealso \code{\link{plot_timing_atc_group}}
#' @seealso \code{\link{plot_summary}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' clust |> plot_comedication_count()
#' clust |> comedication_count() |> plot_comedication_count()
#' clust |> summary() |> plot_comedication_count()
#'
#' @rdname plot_comedication_count
#' @export
plot_comedication_count <- function(object, ...) {
  UseMethod("plot_comedication_count", object)
}

#' @rdname plot_comedication_count
#' @export
plot_comedication_count.medic <- function(object, ...) {
  plot_comedication_count(comedication_count(object, ...), ...)
}

#' @rdname plot_comedication_count
#' @export
plot_comedication_count.summary.medic <- function(object, ...) {
  if (is.null(object$comedication_count)) {
    stop("The summary must contain a 'comedication_count' summary.")
  }
  plot_comedication_count(object$comedication_count, ...)
}

#' @rdname plot_comedication_count
#' @export
plot_comedication_count.comedication_count <- function(
  object,
  scale = "percent",
  scope = "cluster",
  focus = "people",
  with_population = FALSE,
  ...
) {

  chosen_y <- if (
    scale == "percent" && scope == "cluster" && focus == "people"
  ) {
    "Percentage of People in Cluster"
  } else if (
    scale == "percent" && scope == "cluster" && focus == "medication"
  ) {
    "Percentage of Medication in Cluster"
  } else if (
    scale == "percent" && scope == "global" && focus == "people"
  ) {
    "Percentage of All People"
  } else if (
    scale == "percent" && scope == "global" && focus == "medication"
  ) {
    "Percentage of All Medications"
  } else if (
    scale == "percent" && scope == "medication count" && focus == "people"
  ) {
    "Percentage of People with the Same Medication Count"
  } else if (
    scale == "percent" && scope == "medication count" && focus == "medication"
  ) {
    "Percentage of Medication with the Same Medication Count"
  } else if (
    scale == "count" && focus == "people"
  ) {
    "Number of People"
  } else if (
    scale == "count" && focus == "medication"
  ) {
    "Number of medications"
  } else {
    stop(
      paste0(
        "'scale', 'scope' and 'focus' combination is not known. ",
        "Must be one of\n",
        "scale = 'percent', scope = 'cluster', focus = 'people'\n",
        "scale = 'percent', scope = 'cluster', focus = 'medication'\n",
        "scale = 'percent', scope = 'global', focus = 'people'\n",
        "scale = 'percent', scope = 'global', focus = 'medication'\n",
        "scale = 'percent', scope = 'medication count', focus = 'people'\n",
        "scale = 'percent', scope = 'medication count', focus = 'medication'\n",
        "scale = 'count', focus = 'people'",
        "scale = 'count', focus = 'medication'",
      )
    )
  }

  p <- object |>
    dplyr::filter(
      if (with_population) TRUE else .data$Cluster != "Population"
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!dplyr::sym("Medication Count"),
        y = !!dplyr::sym(chosen_y)
      )
    ) +
    ggplot2::geom_col()


  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(!!dplyr::sym("Clustering")),
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  } else {
    p <- p + ggplot2::facet_grid(
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  }

  return(p)
}


#' Plot Timing Trajectory
#'
#' This function plots the timing trajectory.
#'
#' @param object The object containing the timing trajectory data.
#' @param focus The focus of the plot. Must be either "average", "individual" or
#'  "both".
#' @param with_population Logical value indicating whether to include the
#' population cluster.
#' @param max_lines The maximum number of lines to plot.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{timing_trajectory}}
#' @seealso \code{\link{plot_cluster_frequency}}
#' @seealso \code{\link{plot_medication_frequency}}
#' @seealso \code{\link{plot_comedication_count}}
#' @seealso \code{\link{plot_timing_atc_group}}
#' @seealso \code{\link{plot_summary}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' clust |> plot_timing_trajectory()
#' clust |> timing_trajectory() |> plot_timing_trajectory()
#' clust |> summary() |> plot_timing_trajectory()
#'
#' @rdname plot_timing_trajectory
#' @export
plot_timing_trajectory <- function(object, ...) {
  UseMethod("plot_timing_trajectory", object)
}

#' @rdname plot_timing_trajectory
#' @export
plot_timing_trajectory.medic <- function(object, ...) {
  plot_timing_trajectory(timing_trajectory(object, ...), ...)
}

#' @rdname plot_timing_trajectory
#' @export
plot_timing_trajectory.summary.medic <- function(object, ...) {
  if (is.null(object$timing_trajectory)) {
    stop("The summary must contain a 'timing_trajectory' summary.")
  }
  plot_timing_trajectory(object$timing_trajectory, ...)
}

#' @rdname plot_timing_trajectory
#' @export
plot_timing_trajectory.timing_trajectory <- function(
  object,
  focus = "average",
  with_population = FALSE,
  max_lines = 50,
  ...
) {

  line_type <- switch(
    focus,
    "average" = NULL,
    "individual" = NULL,
    "both" = dplyr::sym("Calculation Method"),
    stop("'focus' must be one of 'average', 'individual' or 'both'.")
  )

  timing_cols <- setdiff(
    names(object$average),
    c("Clustering", "Cluster", "Count")
  )

  if (focus == "both") {
    plot_data <- object
    class(plot_data) <- class(plot_data)[-1]
    plot_data <- plot_data |> dplyr::bind_rows(.id = "Calculation Method")
    if (max_lines < Inf) {
      plot_data <- plot_data |>
        dplyr::group_by(
          .data$`Calculation Method`,
          .data$Clustering,
          .data$Cluster
        ) |>
        dplyr::slice_sample(n = max_lines) |>
        dplyr::ungroup()
    }
  } else {
    plot_data <- object[[focus]]
    if (max_lines < Inf && focus == "individual") {
      plot_data <- plot_data |>
        dplyr::group_by(.data$Clustering, .data$Cluster) |>
        dplyr::slice_sample(n = max_lines) |>
        dplyr::ungroup()
    }
  }

  plot_data <- plot_data |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(timing_cols),
      names_to = "Timing",
      values_to = "Exposure"
    ) |>
    dplyr::mutate(Timing = factor(.data$Timing, levels = timing_cols))

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$Timing,
        y = .data$Exposure,
        group = .data$row_number,
        linetype = !!line_type
      )
    )

  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(!!dplyr::sym("Clustering")),
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  } else {
    p <- p + ggplot2::facet_grid(
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  }

  return(p)
}



#' Plot Timing ATC Group
#'
#' This function plots the timing ATC group.
#'
#' @param object The object containing the timing ATC group data.
#' @param focus The focus of the plot. Must be either "average", "individual" or
#' "both".
#' @param with_population Logical value indicating whether to include the
#' population cluster.
#' @param max_lines The maximum number of lines to plot.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{timing_atc_group}}
#' @seealso \code{\link{plot_cluster_frequency}}
#' @seealso \code{\link{plot_medication_frequency}}
#' @seealso \code{\link{plot_comedication_count}}
#' @seealso \code{\link{plot_timing_trajectory}}
#' @seealso \code{\link{plot_summary}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' clust |> plot_timing_atc_group()
#' clust |> timing_atc_group() |> plot_timing_atc_group()
#' clust |> summary() |> plot_timing_atc_group()
#'
#' @rdname plot_timing_atc_group
#' @export
plot_timing_atc_group <- function(object, ...) {
  UseMethod("plot_timing_atc_group", object)
}

#' @rdname plot_timing_atc_group
#' @export
plot_timing_atc_group.medic <- function(object, ...) {
  plot_timing_atc_group(timing_atc_group(object, ...), ...)
}

#' @rdname plot_timing_atc_group
#' @export
plot_timing_atc_group.summary.medic <- function(object, ...) {
  if (is.null(object$timing_atc_group)) {
    stop("The summary must contain a 'timing_atc_group' summary.")
  }
  plot_timing_atc_group(object$timing_atc_group, ...)
}

#' @rdname plot_timing_atc_group
#' @export
plot_timing_atc_group.timing_atc_group <- function(
  object,
  focus = "average",
  with_population = FALSE,
  max_lines = 50,
  ...
) {

  line_type <- switch(
    focus,
    "average" = NULL,
    "individual" = NULL,
    "both" = dplyr::sym("Calculation Method"),
    stop("'focus' must be one of 'average', 'individual' or 'both'.")
  )

  timing_cols <- names(object$average)[-c(1:4)]
  atc_group_name <- names(object$average)[3]

  if (focus == "both") {
    plot_data <- object
    class(plot_data) <- class(plot_data)[-1]
    plot_data <- plot_data |> dplyr::bind_rows(.id = "Calculation Method")
    if (max_lines < Inf) {
      plot_data <- plot_data |>
        dplyr::group_by(
          .data$`Calculation Method`,
          .data$Clustering,
          .data$Cluster,
          !!dplyr::sym(atc_group_name)
        ) |>
        dplyr::slice_sample(n = max_lines) |>
        dplyr::ungroup()
    }
  } else {
    plot_data <- object[[focus]]
    if (max_lines < Inf && focus == "individual") {
      plot_data <- plot_data |>
        dplyr::group_by(
          .data$Clustering,
          .data$Cluster,
          !!dplyr::sym(atc_group_name)
        ) |>
        dplyr::slice_sample(n = max_lines) |>
        dplyr::ungroup()
    }
  }

  plot_data <- plot_data |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(timing_cols),
      names_to = "Timing",
      values_to = "Exposure"
    ) |>
    dplyr::mutate(Timing = factor(.data$Timing, levels = timing_cols))

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$Timing,
        y = .data$Exposure,
        group = .data$row_number,
        color = !!dplyr::sym(atc_group_name),
        linetype = !!line_type
      )
    )

  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(!!dplyr::sym("Clustering")),
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  } else {
    p <- p + ggplot2::facet_grid(
      cols = ggplot2::vars(!!dplyr::sym("Cluster"))
    )
  }

  return(p)
}



#' Plot Summary
#'
#' This function plots the summary of the clustering results.
#'
#' @inheritParams summary.medic
#' @param object The object containing the summary data.
#' @param n_breaks The number of breaks for the time scale.
#' @param plot_individual Logical value indicating whether to plot individual
#'  trajectories.
#' @param labels Logical value indicating whether to include labels.
#' @param alpha_individual The alpha value for the individual trajectories.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{summary}}
#' @seealso \code{\link{plot_cluster_frequency}}
#' @seealso \code{\link{plot_medication_frequency}}
#' @seealso \code{\link{plot_comedication_count}}
#' @seealso \code{\link{plot_timing_trajectory}}
#' @seealso \code{\link{plot_timing_atc_group}}
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' clust |> plot_summary()
#' clust |> summary() |> plot_summary()
#'
#' # If the clustering object contains more than one clustering, it is necessary
#' # to filter the clustering, as only one clustering can be plotted at a time.
#' clust <- medic(complications, id = id, atc = atc, k = 3:5)
#' clust |> plot_summary(only = k == 4)
#' clust |> summary(only = k == 4) |> plot_summary()
#'
#' @rdname plot_summary
#' @export
plot_summary <- function(object, ...) {
  UseMethod("plot_summary", object)
}

#' @rdname plot_summary
#' @export
plot_summary.medic <- function(
  object,
  only = NULL,
  clusters = NULL,
  additional_data = NULL,
  ...
) {

  cluster_summary <- do.call(
    "summary",
    as.list(match.call(expand.dots = TRUE))[-1]
  )

  plot_summary(cluster_summary, ...)
}


#' @rdname plot_summary
#' @export
plot_summary.summary.medic <- function(
  object,
  n_breaks = 5,
  plot_individual = FALSE,
  labels = FALSE,
  alpha_individual = 0.1,
  ...
) {

  summary_methods <- c(
    "cluster_frequency",
    "medication_frequency",
    "comedication_count",
    "timing_trajectory",
    "timing_atc_group"
  )

  check_null <- sapply(summary_methods, function(sm) is.null(object[[sm]]))
  if (any(check_null)) {
    stop(
      "One or more summaries are missing to create the full summary plot.\n",
      "Run 'summary(cluster, outputs = 'all')' first."
    )
  }

  if (length(unique(object$cluster_frequency$Clustering)) > 1) {
    stop(
      "'plot_summary' can only be applied to summaries of 1 clustering.\n",
      "Apply further filters before plotting."
    )
  }

  # Find relevant plotting data
  time_scale <- construct_time_scale(object, n_breaks)
  plot_data <- construct_plot_data(
    object,
    time_scale,
    plot_individual,
    labels,
    alpha_individual,
    ...
  )
  color_scales <- construct_color_scales(plot_data, ...)


  # Construct the plot
  p <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$Timing, y = .data$y)
  ) +

    # Setup the facets
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!dplyr::sym("row_facet")),
      cols = ggplot2::vars(!!dplyr::sym("Cluster")),
      scales = "free_y"
    ) +

    # First row : Comedication counts -- legend via 'fill' below
    ggplot2::geom_col(
      data = \(x) x |> dplyr::filter(.data$part == "comedication_count"),
      ggplot2::aes(fill = .data$`Medication Count`),
      width = time_scale$width
    ) +

    # Second row : Medication frequency -- legend via 'linetype' below
    ggplot2::geom_col(
      data = \(x) x |> dplyr::filter(.data$part == "medication_frequency"),
      ggplot2::aes(
        fill = !!dplyr::sym(object$variables$atc),
        linetype = !!dplyr::sym(object$variables$atc)
      ),
      width = time_scale$width
    ) +

    # Third row : Average trajectories
    ggplot2::geom_line(
      data = \(x) x |> dplyr::filter(.data$part == "timing_trajectory"),
      ggplot2::aes(alpha = .data$alpha, group = .data$line_group)
    ) +

    # Remaining rows : Timing ATC code -- legend via 'color' below
    ggplot2::geom_line(
      data = \(x) x |> dplyr::filter(.data$part == "timing_atc_group"),
      ggplot2::aes(
        color = .data$`ATC Groups`,
        alpha = .data$alpha,
        group = .data$line_group
      )
    ) +

    # Legend for comedication count
    ggplot2::scale_fill_manual(
      values = c(
        color_scales$comedication_count_colors,
        color_scales$medication_frequency_colors
      ),
      breaks = names(color_scales$comedication_count_colors),
      guide = ggplot2::guide_legend(order = 1)
    ) +

    # Legend for medication frequency
    ggplot2::scale_linetype_manual(
      values = color_scales$medication_frequency_linetype,
      guide = ggplot2::guide_legend(
        order = 2,
        override.aes = list(fill = color_scales$medication_frequency_colors)
      )
    ) +

    # Legend for timing atc group
    ggplot2::scale_color_manual(
      values = color_scales$timing_atc_group_colors,
      guide =  ggplot2::guide_legend(order = 3)
    ) +

    # Hidden alpha scale
    ggplot2::scale_alpha_continuous(
      range = if (plot_individual) c(alpha_individual, 1) else c(1, 1),
      guide = "none"
    ) +

    # Time Scale
    ggplot2::scale_x_continuous(breaks = time_scale$breaks)

  if (labels) {
    p <- p +
      ggplot2::geom_text(
        data = \(x) x |> dplyr::filter(.data$part == "label"),
        ggplot2::aes(label = .data$label)
      )
  }


  return(p)
}
