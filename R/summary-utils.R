
#' Print Summary of Medication
#'
#' This function prints a summary of medication information.
#'
#' @param x An object of class `summary.medic`.
#' @param ... currently only included for compatibility with generic. Has no
#' effect.
#'
#' @details This function prints various information about medication, including
#' cluster frequency, medication frequency, number of different medication taken
#' in the study period, average exposure trajectories, and average exposure
#' trajectories by ATC groups.
#'
#' @return The function is called for its side effects and does not return any
#' value.
print.summary.medic <- function(x, ...) {
  op <- options(pillar.print_max = 5, pillar.advice = FALSE)
  on.exit(options(op))

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  if (!is.null(x$cluster_frequency)) {
    cat("Cluster frequency:\n")
    print(x$frequency)
    cat("\n")
  }

  if (!is.null(x$medication_frequency)) {
    cat("Medication frequency:\n")
    print(x$medication_frequency)
    cat("\n")
  }

  if (!is.null(x$comedication_count)) {
    cat("Number of Different Medication Taken in Study Period:\n")
    print(x$comedication_count)
    cat("\n")
  }

  if (!is.null(x$timing_trajectory)) {
    cat("Average exposure trajectories:\n")
    print(x$timing_trajectory$average)
    cat("\n")
  }

  if (!is.null(x$timing_atc_group)) {
    cat("Average exposure trajectories by ATC groups:\n")
    print(x$timing_atc_group$average)
    cat("\n")
  }

  return(invisible(x))
}


#' Summary of a medic-object using str function
#'
#' @param object A `medic` object.
#' @param ... Additional arguments passed to `str.default`.
#'
#' This function provides a summary of an object by using the `str` function. It
#' is a modified version of the `str.default` function from the `utils` package,
#' with the maximum level set to 2.
str.summary.medic <- function(object, ...) {
  utils:::str.default(object, max.level = 2, ...)
}



#' Default ATC groups for summaries
#'
#' This function finds the default ATC groups for the summaries. It is used in
#' the `summary.medic` function.
#'
#' @param object A `medic` object.
#' @param min_n The minimum number of ATC groups to be found.
#'
#' @return A data frame with two columns: `regex` and `atc_groups`.
#'
#' @export
default_atc_groups <- function(object, min_n = 2) {
  observed_atc <- object$data[[object$variables$atc]]
  i <- 1L
  while (length(unique(stringr::str_sub(observed_atc, 1, i))) < min_n) {
    i <- switch(i, 3, NA_real_, 4, 5, 7, NA_real_, break)
  }
  found_atc <- sort(unique(stringr::str_sub(observed_atc, 1, i)))
  return(data.frame(regex = paste0("^", found_atc), atc_groups = found_atc))
}