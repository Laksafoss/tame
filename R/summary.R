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
#' @param ... Additional arguments passed to the internal summary function. 
#' 
#'   * `cluster_wise`  an option in the `medications()` function.
#'   * `m` an option in the `medications()` function. A numeric restricting the 
#'     number of distinct ATC codes plotted within each cluster. That is, the 
#'     (at most) `m` most frequent ATC codes within that cluster is given a 
#'     color.
#'   * `q` an option in the `medications()` function. A numeric between 0 and 1 
#'     restricting the minimal ATC codes frequency displayed within each 
#'     cluster.
#'   * `count_grouper` an option in the `amounts()` function. A function for 
#'     grouping counts. As a standard it groups counts as 1 medication, 2 
#'     medications, and 3+ medications.
#'   * `atc_groups` A data.frame specifying the ATC groups to summaries by. The
#'     data.frame must have two columns: (1) `regex` giving regular expressions 
#'     specifying the wanted ATC groups and (2) `atc_groups` the name of this 
#'     ATC grouping. As a standard the anatomical level (first level) of the ATC 
#'     codes is used.
#'  
#'  
#'
#' @return
#' A list of clustering characteristics of class `summary.medic` is returned. It
#' can contain any of the following characteristics:
#'
#' ## Frequencies
#' The number of individuals assigned to each cluster and the associated 
#' frequency of assignment.
#'
#' ## Medications
#' The number of individuals with a specific ATC code within a cluster. 
#' Moreover, it calculates the percentage of people with this medication 
#' assigned to this cluster and the percent of people within the cluster with 
#' this medication.
#'
#' ## Amounts
#' The number of ATC codes an individual has, and then outputs the number of 
#' individuals within a cluster that has that many ATC codes. Moreover, various 
#' relevant percentages or calculated. See Value below for more details on these 
#' percentages.
#'
#' ## Trajectories
#' The number of unique timing trajectories in each cluster, and the average 
#' timing trajectories in each cluster. 
#' 
#' ## Interactions
#' The number of people with unique timing trajectory and ATC group, as given by 
#' `atc_groups`, in each cluster. 
#' 
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3:5)
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
    res <- frequencies(
      object,
      only = {{ only }}, 
      clusters = {{ clusters }},
      additional_data = additional_data
    )
    class(res) <- c("summary.medic.frequencies", class(res))
    out <- c(out, res)
  }
  if ("medications" %in% outputs) {
    res <- medications(
      object,
      only = {{ only }}, 
      clusters = {{ clusters }},
      additional_data = additional_data,
      ...
    )
    class(res) <- c("summary.medic.medications", class(res))
    out <- c(out, res)
  }
  if ("amounts" %in% outputs) {
    res <- amounts(
      object,
      only = {{ only }}, 
      clusters = {{ clusters }},
      additional_data = additional_data,
      ...
    )
    class(res) <- c("summary.medic.amounts", class(res))
    out <- c(out, res)
  }
  if ("trajectories" %in% outputs) {
    res <- trajectories(
      object,
      only = {{ only }}, 
      clusters = {{ clusters }},
      additional_data = additional_data,
      ...
    )
    class(res) <- c("summary.medic.trajectories", class(res))
    out <- c(out, res)
  }
  if ("interactions" %in% outputs) {
    res <- interactions(
      object,
      only = {{ only }}, 
      clusters = {{ clusters }},
      additional_data = additional_data,
      ...
    )
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

#' @describeIn summary.medic Plot method for medic-objects
#' @param x A `summary.medic` object for printing or plotting.
#' @param by <[`data-masking`][dplyr::dplyr_data_masking]>
#' @param facet <[`data-masking`][dplyr::dplyr_data_masking]>
#'
#' @export
plot.summary.medic <- function(x, by, facet, ...) {
  warning("This function is still under construction")
  plot(c(1, 2, 3), c(3, 4, 5))
}

