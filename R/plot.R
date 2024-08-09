## plot_cluster_frequency ------------------------------------------------------

plot_cluster_frequency <- function(object, ...) {
  UseMethod("plot_cluster_frequency", object)
}

plot_cluster_frequency.medic <- function(object, ...) {
  plot_cluster_frequency(cluster_frequency(object, ...), ...)
}

plot_cluster_frequency.summary.medic <- function(object, ...) {
  if(is.null(object$cluster_frequency)) {
    stop("The summary must contain a 'cluster_frequency' summary.")
  }
  plot_cluster_frequency(object$cluster_frequency, ...)
}

plot_cluster_frequency.summary.medic.cluster_frequency <- function(
    object, 
    scale = "percent",
    with_population = FALSE
) {
  
  chosen_y <- switch(
    scale,
    "percent" = "Percent",
    "count" = "Count",
    stop("'scale' is not recognised. Must be either 'percent' or 'count'.")
  )
  
  p <- object |>
    dplyr::filter(if (with_population) TRUE else .data$Cluster != "Population") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Cluster, y = !!dplyr::sym(chosen_y))) + 
    ggplot2::geom_col() 
  
  if (p$data |> dplyr::distinct(.data$Clustering) |> nrow() > 1) {
    p <- p + ggplot2::facet_grid(rows = "Clustering")
  }
  
  return(p)
}




## plot_medication_frequency   ---------------------------------------------------

plot_medication_frequency <- function(object, ...) {
  UseMethod("plot_medication_frequency", object)
}

plot_medication_frequency.medic <- function(object, ...) {
  plot_medication_frequency(medication_frequency(object, ...), ...)
}

plot_medication_frequency.summary.medic <- function(object, ...) {
  if(is.null(object$medication_frequency)) {
    stop("The summary must contain a 'medication_frequency' summary.")
  }
  plot_medication_frequency(object$medication_frequency, ...)
}


plot_medication_frequency.summary.medic.medication_frequency <- function(
    object,
    scale = "percent",
    scope = "cluster",
    with_population = FALSE
) {
  
  chosen_y <- if(scale == "percent" && scope == "cluster") {
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
    dplyr::filter(if (with_population) TRUE else .data$Cluster != "Population") |>
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






## plot_medication_count   -----------------------------------------------------
plot_comedication_count <- function(object, ...) {
  UseMethod("plot_comedication_count", object)
}

plot_comedication_count.medic <- function(object, ...) {
  plot_comedication_count(comedication_count(object, ...), ...)
}

plot_comedication_count.summary.medic <- function(object, ...) {
  if(is.null(object$comedication_count)) {
    stop("The summary must contain a 'comedication_count' summary.")
  }
  plot_comedication_count(object$comedication_count, ...)
}

plot_comedication_count.summary.medic.comedication_count <- function(
    object,
    scale = "percent",
    scope = "cluster",
    focus = "people",
    with_population = FALSE
) {
  
  chosen_y <- if(scale == "percent" && scope == "cluster" && focus == "people") {
    "Percentage of People in Cluster" 
  } else if(scale == "percent" && scope == "cluster" && focus == "medication") {
    "Percentage of Medication in Cluster" 
  } else if (scale == "percent" && scope == "global" && focus == "people") {
    "Percentage of All People"
  } else if (scale == "percent" && scope == "global" && focus == "medication") {
    "Percentage of All Medications"
  } else if (scale == "percent" && scope == "medication count" && focus == "people") {
    "Percentage of People with the Same Medication Count"
  } else if (scale == "percent" && scope == "medication count" && focus == "medication") {
    "Percentage of Medication with the Same Medication Count"
  } else if (scale == "count" && focus == "people") {
    "Number of People"
  } else if (scale == "count" && focus == "medication") {
    "Number of medications"
  } else {
    stop(
      paste0(
        "'scale', 'scope' and 'focus' combination is not known. Must be one of\n",
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
    dplyr::filter(if (with_population) TRUE else .data$Cluster != "Population") |>
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


## plot_timing_trajectory   ----------------------------------------------------

plot_timing_trajectory <- function(object, ...) {
  UseMethod("plot_timing_trajectory", object)
}

plot_timing_trajectory.medic <- function(object, ...) {
  plot_timing_trajectory(timing_trajectory(object, ...), ...)
}

plot_timing_trajectory.summary.medic <- function(object, ...) {
  if(is.null(object$timing_trajectory)) {
    stop("The summary must contain a 'timing_trajectory' summary.")
  }
  plot_timing_trajectory(object$timing_trajectory, ...)
}

plot_timing_trajectory.summary.medic.timing_trajectory <- function(
    object, 
    focus = "average",
    with_population = FALSE,
    max_lines = 50,
    ...
) {
  
  line_type <- switch (
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



## plot_timing_atc_group   -----------------------------------------------------

plot_timing_atc_group <- function(object, ...) {
  UseMethod("plot_timing_atc_group", object)
}

plot_timing_atc_group.medic <- function(object, ...) {
  plot_timing_atc_group(timing_atc_group(object, ...), ...)
}

plot_timing_atc_group.summary.medic <- function(object, ...) {
  if(is.null(object$timing_atc_group)) {
    stop("The summary must contain a 'timing_atc_group' summary.")
  }
  plot_timing_atc_group(object$timing_atc_group, ...)
}

plot_timing_atc_group.summary.medic.timing_atc_group <- function(
    object, 
    focus = "average",
    with_population = FALSE,
    max_lines = 50,
    ...
) {
  
  line_type <- switch (
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




## plot_summary   --------------------------------------------------------------

plot_summary <- function(object, ...) {
  UseMethod("plot_summary", object)
}

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


construct_time_scale <- function(object, n.breaks = 5) {
  
  ns <- seq_along(object$variables$timing)
  translator <- data.frame(
    timing_names = object$variables$timing,
    Timing = ns - 1 - diff(range(ns)) / 2
  )
  
  mid <- 0
  width <- -translator$Timing[1] * 2
  
  if (length(ns) < n.breaks) {
    chosen_n_breaks <- length(ns)
  } else {
    # spiral out until we find a nice number of breaks
    start <- 1
    stop <- length(ns)
    testing_breaks <- n.breaks
    updator <- 1
    while(!all(seq(start, stop, length.out = testing_breaks) %% 1 == 0)) {
      testing_breaks <- testing_breaks + updator
      updator <- -1 * (updator + 1)
    }
    chosen_n_breaks <- testing_breaks
  }
  
  breaks <- translator[seq(1, nrow(translator), length.out = chosen_n_breaks),] |>
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
      plotting_part = "medication_frequency"
    )
  
  comedication_counts <- object$comedication_count |>
    dplyr::mutate(
      row_facet = "Comedication Count",
      y = .data$`Percentage of People in Cluster`,
      plotting_part = "comedication_count"
    )
  
  timing_trajectories <- object$timing_trajectory |>
    flatten_trajctories(plot_individual) |>
    dplyr::mutate(
      row_facet = "Average Trajectory",
      plotting_part = "timing_trajectory",
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
      plotting_part = "timing_atc_group",
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
        plotting_part = "label",
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
          "plotting_part",
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
    } else { # this else is not optimal - perhaps we should choose different colors
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

plot_summary.summary.medic <- function(
    object,
    n.breaks = 5,
    plot_individual = FALSE,
    labels = FALSE,
    alpha_individual = 0.1,
    # min_count = 100,
    # min_percent = 0.05,
    # count_labels = TRUE,
    # sample_n_individual = 100, 
    # weighted_sample = TRUE,
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
  time_scale <- construct_time_scale(object, n.breaks)
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
      rows = dplyr::vars(!!dplyr::sym("row_facet")), 
      cols = dplyr::vars(!!dplyr::sym("Cluster")),
      scales = "free_y"
    ) +
    
    # First row : Comedication counts -- legend via 'fill' below
    ggplot2::geom_col(
      data = \(x) x |> dplyr::filter(.data$plotting_part == "comedication_count"),
      ggplot2::aes(fill = .data$`Medication Count`),
      width = time_scale$width
    ) +
    
    # Second row : Medication frequency -- legend via 'linetype' below
    ggplot2::geom_col(
      data = \(x) x |> dplyr::filter(.data$plotting_part == "medication_frequency"),
      ggplot2::aes(
        fill = !!dplyr::sym(object$variables$atc), 
        linetype = !!dplyr::sym(object$variables$atc) # ATC_NAME !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ),
      width = time_scale$width
    ) + 
    
    # Third row : Average trajectories
    ggplot2::geom_line(
      data = \(x) x |> dplyr::filter(.data$plotting_part == "timing_trajectory"),
      ggplot2::aes(alpha = .data$alpha, group = .data$line_group)
    ) + 
    
    # Remaining rows : Timing ATC code -- legend via 'color' below
    ggplot2::geom_line(
      data = \(x) x |> dplyr::filter(.data$plotting_part == "timing_atc_group"),
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
         data = \(x) x |> dplyr::filter(.data$plotting_part == "label"),
         ggplot2::aes(label = .data$label)
      ) 
  }
  
  
  return(p)
}
