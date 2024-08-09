crop_summary <- function(object, ...) {
  UseMethod("crop_summary", object)
}

crop_summary.summary.medic.cluster_frequency <- function(
    object,
    top_n = 5,
    min_count = 100,
    min_percent = 0.05
) {
  
  cluster_levels <- c(levels(object$Cluster), "Remaining")
  
  res <- object |>
    dplyr::group_by(.data$Clustering) |>
    dplyr::mutate(
      Cluster = dplyr::if_else(
        top_n < rank(-.data$Count, ties.method = "first") - 1 | 
          .data$Percent < min_percent | 
          .data$Count < min_count,
        "Remaining",
        .data$Cluster
      )
    ) |>
    dplyr::group_by(.data$Clustering, .data$Cluster) |>
    dplyr::summarise(
      Count = sum(.data$Count),
      Percent = sum(.data$Percent),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Cluster = factor(
        .data$Cluster, 
        levels = intersect(cluster_levels, .data$Cluster)
      )
    ) |>
    dplyr::arrange(.data$Clustering, .data$Cluster)
  
  class(res) <- c("summary.medic.cluster_frequency", class(res))
  return(res)
}

crop_summary.summary.medic.medication_frequency <- function(
    object,
    top_n = 5,
    min_count = 0,
    min_percent = 0,
    scope = "cluster"
) {
  
  scope_name <- switch (
    scope,
    "cluster" = "Percent of Medication in Cluster",
    "global" = "Percent of All Medication",
    stop("'scope' must be either 'cluster' or 'global'.")
  )
  
  group_name <- switch (
    scope,
    "cluster" = c("Clustering", "Cluster"),
    "global" = "Clustering" 
  )
  
  atc_name <- attr(object, "atc")
  
  cluster_levels <- c(levels(object$Cluster), "Remaining")
  
  selected_top <- object |> 
    dplyr::group_by(!!!dplyr::syms(group_name)) |>
    dplyr::mutate(
      remaining = top_n < rank(-.data$Count, ties.method = "first") | 
        !!dplyr::sym(scope_name) < min_percent |
        .data$Count < min_count
    ) |>
    dplyr::ungroup() |>
    dplyr::select(!!!group_name, !!atc_name, "remaining") |>
    dplyr::distinct()
  
  res <- object |>
    dplyr::left_join(selected_top, by = c(group_name, atc_name)) |>
    dplyr::mutate(
      "{atc_name}" := dplyr::if_else(
        .data$remaining, 
        "Remaining", 
        !!dplyr::sym(atc_name)
      ),
      "Percent of ATC code" = dplyr::if_else(  #
        .data$remaining,                       # We may be able to make something
        NA_real_,                              # a bit smarter, but this is fine
        .data$`Percent of ATC code`            # for now...
      )                                        #
    ) |>
    dplyr::group_by(.data$Clustering, .data$Cluster, !!dplyr::sym(atc_name)) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          c(
            "Count", 
            "Percent of Medication in Cluster", 
            "Percent of All Medication", 
            "Percent of ATC code"
          )
        ),
        sum
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Cluster = factor(
        .data$Cluster, 
        levels = intersect(cluster_levels, .data$Cluster)
      )
    ) |>
    dplyr::arrange(.data$Clustering, .data$Cluster)
  
  class(res) <- c("summary.medic.medication_frequency", class(res))
  return(res)
}

crop_summary.summary.medic.comedication_count <- function(object, ...) {
  cat("\nTO DO: crop_summary.summary.medic.comedication_count\n")
  return(object)
}

crop_summary.summary.medic.timing_trajectory <- function(
    object, 
    sample_n_individual = 100, 
    weighted_sample = TRUE,
    ...
) {
  
  res <- object
  res$individual <- res$individual |>
    dplyr::group_by(.data$Clustering, .data$Cluster) |>
    dplyr::slice_sample(
      n = sample_n_individual, 
      weight_by = if (weighted_sample) .data$Count 
    )
  
  class(res) <- c("summary.medic.timing_trajectory", class(res))
  return(res)
}

crop_summary.summary.medic.timing_atc_group <- function(
    object, 
    sample_n_individual = 100, 
    weighted_sample = TRUE,
    ...
) {
  
  # Do we need more simplification tools options here?
  
  res <- object
  res$individual <- res$individual |>
    dplyr::group_by(.data$Clustering, .data$Cluster, .data$`ATC Groups`) |>
    dplyr::slice_sample(
      n = sample_n_individual, 
      weight_by = if (weighted_sample) .data$`Number of Medications with Timing Trajectory` 
    )
  
  class(res) <- c("summary.medic.timing_atc_group", class(res))
  return(res)
}

crop_summary.summary.medic <- function(object, which = "all", ...) {
  
  summary_options <- c(
    "cluster_frequency",
    "medication_frequency",
    "comedication_count",
    "timing_trajectory",
    "timing_atc_group"
  )
  
  if (any(! which %in% c("all", summary_options))) {
    stop(
      "'which' must be 'all' or a subset of\n", 
      paste0("'", summary_options, "'", collapse = ", ")
    )
  }
  
  chosen_crops <- if (any(which == "all")) summary_options else which
  
  check_nulls <- sapply(chosen_crops, function(cc) is.null(object[[cc]]))
  if (any(check_nulls)) {
    stop(
      "The following summaries are missing from 'object':\n",
      paste0("'", names(check_nulls)[check_nulls], "'", collapse = ", ")
    )
  }
  
  for (cc in chosen_crops) {
    object[[cc]] <- crop_summary(object[[cc]], ...)
  }
  
  return(object)
}