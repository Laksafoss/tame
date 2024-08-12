#' Crop Clustering Summary
#'
#' Functions for cropping summarized cluster data.
#'
#' @param object The summary object to be cropped.
#' @param top_n integer. In the case of `cluster_frequency` it is the number of
#'   clusters to keep. In the case of `medication_frequency` it is the number of
#'   medications to keep. If `inf`, all clusters or medications are kept.
#' @param min_count integer. The minimum count of a cluster or medication to
#'   keep it in the summary. If 0, the default, the minimum count is zero, i.e.
#'   there is not a minimum count.
#' @param min_percent numeric. The minimum percentage of a cluster or medication
#'   to keep it in the summary. If 0, the default, the minimum percentage is
#'   zero, i.e. there is not a minimum percentage.
#' @param scope character. The scope of the summary crops `top_n`, `min_count`
#'  and `min_percent`. The options are "cluster" and "global". The default is
#'  "cluster". If "cluster", the crop is based on the percentage of medication
#'  in the cluster. If "global", the crop is based on the percentage of all
#'  medication.
#' @param sample_n_individual a logical or integer. If FALSE, no individual
#'   timing trajectories are sampled. If integer, `sample_n_individual` is the
#'   number of individual timing trajectories to sample. To sample all
#'   individual timing trajectories, set `sample_n_individual` to `Inf`.
#' @param weighted_sample a logical, but only used if `sample_n_individual` is
#'   an integer. If TRUE, the individual timing trajectories are sampled
#'   weighted by the number of medications in the individual timing trajectory.
#'   If FALSE, the individual timing trajectories are sampled uniformly.
#' @param which A character vector specifying which summaries to crop. The
#'   options are "cluster_frequency", "medication_frequency",
#'   "comedication_count", "timing_trajectory", and "timing_atc_group". The
#'   default is "all".
#' @param ... Additional arguments to be passed to the specific method.
#'
#' @details
#'
#' # `cluster_frequency` summary crop
#' Extracts the top `top_n` clusters by count. If `top_n` is `Inf`, all clusters
#' are kept. If `min_count` is greater than 0, clusters with a count less than
#' `min_count` are removed. If `min_percent` is greater than 0, clusters with a
#' percentage less than `min_percent` are removed. The remaining clusters are
#' grouped into a "Remaining" cluster.
#'
#' # `medication_frequency` summary crop
#' Extracts the top `top_n` medications by count. If `top_n` is `Inf`, all
#' medications are kept. If `min_count` is greater than 0, medications with a
#' count less than `min_count` are removed. If `min_percent` is greater than 0,
#' medications with a percentage less than `min_percent` are removed. The
#' remaining medications are grouped into a "Remaining" cluster.
#'
#' The `scope` argument determines the scope of the crop. If `scope` is
#' "cluster", the crop is based on the percentage of medication in the cluster.
#' If `scope` is "global", the crop is based on the percentage of all
#' medication.
#'
#' # `comedication_count` summary crop
#' TO DO
#'
#' # `timing_trajectory` summary crop
#' Samples `sample_n_individual` individual timing trajectories. If
#' `sample_n_individual` is `Inf`, all individual timing trajectories are kept.
#' If `weighted_sample` is `TRUE`, the individual timing trajectories are
#' sampled weighted by the number of medications in the individual timing
#' trajectory.
#'
#' # `timing_atc_group` summary crop
#' Samples `sample_n_individual` individual timing trajectories. If
#' `sample_n_individual` is `Inf`, all individual timing trajectories are kept.
#' If `weighted_sample` is `TRUE`, the individual timing trajectories are
#' sampled weighted by the number of medications in the individual timing
#' trajectory.
#'
#' # `summary.medic` summary crop
#' Crops multiple summaries. The `which` argument is a character vector
#' specifying which summaries to crop. The options are "cluster_frequency",
#' "medication_frequency", "comedication_count", "timing_trajectory", and
#' "timing_atc_group". If `which` is "all", all summaries are cropped.
#'
#' The `...` argument is passed to the specific methods, e.g. `top_n` and
#' `min_count` are passed to `cluster_frequency` and `medication_frequency`.
#'
#' @seealso \code{\link{summary}}, \code{\link{cluster_frequency}},
#' \code{\link{medication_frequency}}, \code{\link{comedication_count}},
#' \code{\link{timing_trajectory}}, \code{\link{timing_atc_group}}
#'
#' @return A summary object, which is a modified version of the input summary
#' object.
#'
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#'
#' # Crop the cluster frequency summary
#' clust |>
#'   cluster_frequency() |>
#'   summary_crop(top_n = 3)
#'
#' clust |>
#'   summary() |>
#'   summary_crop(which = "cluster_frequency", top_n = 3)
#'
#'
#' # Crop the medication frequency summary
#' clust |>
#'   medication_frequency() |>
#'   summary_crop(top_n = 3)
#'
#' clust |>
#'   summary() |>
#'   summary_crop(which = "medication_frequency", top_n = 3)
#'
#'
#' # Crop the co-medication count summary
#' clust |>
#'   comedication_count() |>
#'   summary_crop(min_count = 10)
#'
#' clust |>
#'   summary() |>
#'   summary_crop(which = "comedication_count", min_count = 10)
#'
#'
#' # crop the timing trajectory summary
#' clust |>
#'   timing_trajectory() |>
#'   summary_crop()
#'
#' clust |>
#'   summary() |>
#'   summary_crop(which = "timing_trajectory")
#'
#'
#' # crop the timing ATC group summary
#' clust |>
#'   timing_atc_group() |>
#'   summary_crop()
#'
#' clust |>
#'   summary() |>
#'   summary_crop(which = "timing_atc_group")
#'
#' # crop multiple summaries
#' clust |>
#'   summary() |>
#'   summary_crop(
#'     which = c("cluster_frequency", "medication_frequency"),
#'     top_n = 3
#'   )
#' 
#' @rdname summary_crop
#' @export
summary_crop <- function(object, ...) {
  UseMethod("summary_crop", object)
}

#' @rdname summary_crop
#' @export
summary_crop.cluster_frequency <- function(
  object,
  top_n = 5L,
  min_count = 0,
  min_percent = 0
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

  class(res) <- c("cluster_frequency", class(res))
  return(res)
}


#' @rdname summary_crop
#' @export
summary_crop.medication_frequency <- function(
  object,
  top_n = 5L,
  min_count = 0,
  min_percent = 0,
  scope = "cluster"
) {

  scope_name <- switch(
    scope,
    "cluster" = "Percent of Medication in Cluster",
    "global" = "Percent of All Medication",
    stop("'scope' must be either 'cluster' or 'global'.")
  )

  group_name <- switch(
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
      "{atc_name}" := dplyr::if_else(              # nolint: object_name_linter.
        .data$remaining,
        "Remaining",
        !!dplyr::sym(atc_name)
      ),
      "Percent of ATC code" = dplyr::if_else( #
        .data$remaining,                      # We may be able to make something
        NA_real_,                             # a bit smarter, but this is fine
        .data$`Percent of ATC code`           # for now...
      )                                       #
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

  class(res) <- c("medication_frequency", class(res))
  return(res)
}


#' @rdname summary_crop
#' @export
summary_crop.comedication_count <- function(object, ...) {
  cat("\nTO DO: summary_crop.comedication_count\n")
  return(object)
}

#' @rdname summary_crop
#' @export
summary_crop.timing_trajectory <- function(
  object,
  sample_n_individual = 100L,
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

  class(res) <- c("timing_trajectory", class(res))
  return(res)
}

#' @rdname summary_crop
#' @export
summary_crop.timing_atc_group <- function(
  object,
  sample_n_individual = 100L,
  weighted_sample = TRUE,
  ...
) {

  # Do we need more simplification tools options here?

  res <- object
  res$individual <- res$individual |>
    dplyr::group_by(.data$Clustering, .data$Cluster, .data$`ATC Groups`) |>
    dplyr::slice_sample(
      n = sample_n_individual,
      weight_by = if (weighted_sample) {
        .data$`Number of Medications with Timing Trajectory`
      }
    )

  class(res) <- c("timing_atc_group", class(res))
  return(res)
}

#' @rdname summary_crop
#' @export
summary_crop.summary.medic <- function(object, which = "all", ...) {

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
    object[[cc]] <- summary_crop(object[[cc]], ...)
  }

  return(object)
}