#' Summary of medic object
#'
#' Make cluster characterizing summaries.
#'
#' @param object An object for which a summary is desired.
#' @param only <[`data-masking`][dplyr::dplyr_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the variables in
#'   `object` and/or `additional_data`.
#'
#'   The default `NULL` selects all clusterings in `object`.
#' @param clusters <[`tidy-select`][dplyr::dplyr_tidy_select]> An unquoted
#'   expression naming the cluster or clusters in `object` one wants to
#'   see summaries of. Names can be used as if they were positions in the data
#'   frame, so expressions like I:IV can be used to select a range of clusters.
#'
#'   The default `NULL` selects all clusters in the chosen clusterings of
#'   `object`.
#' @param outputs A character vector naming the desired characteristics to
#'   output. The default names all possible output types.
#' @param additional_data A data frame with additional data that may be
#'   (left-)joined onto the `parameters` in `object`. This is often 
#'   used in conjuction with `only` to select specific clusterings based on 
#'   `additional_data`.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return
#' A list of clustering characteristics of class `summary.medic` is returned. It
#' can contain any of the following characteristics:
#'
#' ## Frequencies
#' 123
#'
#' ## Medications
#' 123
#'
#' ## Amounts
#' 123
#'
#' ## Trajectories
#' 123
#' 
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)
#' summary(clust, k == 3)
#'
#' @export
summary.medic <- function(
    object,
    only = NULL,
    clusters = NULL,
    outputs = c(
      "frequencies",
      "medications",
      "amounts",
      "trajectories",
      "interactions"
      ),
    additional_data = NULL,
    ...) {


  #   ===   Input Check   ======================================================
  valid_outputs <- c(
    "frequencies",
    "medications",
    "amounts",
    "trajectories",
    "interactions"
  )
  if (!is.character(outputs)) {
    stop(
      paste0(
        "'output' must be a character vector naming the chraractization ",
        "outputs.\nValid inputs are 'all' or any of\n   ",
        paste0(valid_outputs, collapse = ", ")
      )
    )
  }
  if (outputs == "all") {
    outputs <- valid_outputs
  }
  if (!all(outputs %in% valid_outputs)) {
    stop(
      paste0(
        ""
      )
    )
  }

  out <- NULL

  if ("frequencies" %in% outputs) {
    res <- 1
    class(res) <- c("summary.medic.frequencies", class(res))
    out <- c(out, res)
  }
  if ("medications" %in% outputs) {
    res <- 1
    class(res) <- c("summary.medic.medications", class(res))
    out <- c(out, res)
  }
  if ("amounts" %in% outputs) {
    res <- 1
    class(res) <- c("summary.medic.amounts", class(res))
    out <- c(out, res)
  }
  if ("trajectories" %in% outputs) {
    res <- 1
    class(res) <- c("summary.medic.trajectories", class(res))
    out <- c(out, res)
  }
  if ("interactions" %in% outputs) {
    res <- 1
    class(res) <- c("summary.medic.interactions", class(res))
    out <- c(out, res)
  }

  return(
    structure(
      out,
      class = "summary.medic"
    )
  )
}


#' @describeIn summary.medic Print method for medic-objects
#' @param x A `summary.medic` object for printing or plotting.
#' @export
print.summary.medic <- function(x, ...) {
  print.default(x)
}

#' @describeIn summary.medic Plot method for medic-obejcts
#' @param x A `summary.medic` object for printing or plotting.
#' @param by <[`data-masking`][dplyr::dplyr_data_masking]>
#' @param facet <[`data-masking`][dplyr::dplyr_data_masking]>
#'
#' @export
plot.summary.medic <- function(x, by, facet, ...) {
  plot(c(1, 2, 3), c(3, 4, 5))
}

