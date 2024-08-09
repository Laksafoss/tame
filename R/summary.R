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
#' @param ... Additional arguments passed to the specific summary sub-function.
#'
#' @return
#' A list of clustering characteristics of class `summary.medic` is returned. It
#' can contain any of the following characteristics:
#'
#' ## Cluster Frequencies
#' The number of individuals assigned to each cluster and the associated 
#' frequency of assignment.
#'
#' ## Medication Frequencies
#' The number of individuals with a specific ATC code within a cluster. 
#' Moreover, it calculates the percentage of people with this medication 
#' assigned to this cluster and the percent of people within the cluster with 
#' this medication.
#'
#' ## Comedication Count
#' The number of ATC codes an individual has, and then outputs the number of 
#' individuals within a cluster that has that many ATC codes. Moreover, various 
#' relevant percentages or calculated. See Value below for more details on these 
#' percentages.
#'
#' ## Timing Trajectories
#' The number of unique timing trajectories in each cluster, and the average 
#' timing trajectories in each cluster. 
#' 
#' ## Timing and ATC group interactions
#' The number of people with unique timing trajectory and ATC group, as given by 
#' `atc_groups`, in each cluster. 
#' 
#' @examples
#' clust <- medic(complications, id = id, atc = atc, k = 3:5)
#' summary(clust)
#'
#' @export
summary.medic <- function(
    object, 
    only = NULL,
    clusters = NULL, 
    outputs = "all", 
    additional_data = NULL,
    ...
) {
  
  inputs <- as.list(match.call(expand.dots = TRUE))[-1]
  
  valid_outputs <- c(
    "cluster_frequency", 
    "medication_frequency", 
    "comedication_count",
    "timing_trajectory", 
    "timing_atc_group"
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
  if (any(outputs == "all")) { outputs <- valid_outputs }
  if (!all(outputs %in% valid_outputs)) {
    stop(
      paste0(
        "One or more of the summary outputs chosen are not supported.\n",
        "\nValid inputs are 'all' or any of\n   ",
        paste0(valid_outputs, collapse = ", ")
      )
    )
  }
  
  out <- sapply(
    valid_outputs[valid_outputs %in% outputs],
    function(x) {do.call(x, inputs)},
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  
  out <- structure(
    c(
      out, 
      "variables" = list(object$variables), 
      "parameters" = list(object$parameters), # should this be subject to only and clusters?
      "call" = list(object$call)
    ), 
    class = "summary.medic"
  )
  
  if (all(outputs %in% valid_outputs)) { 
    class(out) <- c("summary.medic.all", class(out))
  }
  
  return(out)
}



#' The Frequency of Assignment to Each Cluster
#'
#' The function `cluster_frequency()` calculates the number and frequency of
#' individuals assigned to each cluster.
#'
#' @inheritParams summary.medic
#'
#' @details
#' `cluster_frequency()` calculates the number of individuals assigned to
#' each cluster and the associated frequency of assignment.
#'
#' @return
#' `cluster_frequency()` returns a data frame with class
#' `summary.medic.cluster_frequency`.
#' * `Clustering` the name of the clustering.
#' * `Cluster` the cluster name.
#' * `Count` the number of individuals assigned to the cluster.
#' * `Percent` the percent of individuals assigned to the cluster.
#
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# # make frequency tables
# cluster_frequency(clust, k == 5)
# cluster_frequency(clust, k < 5, I:III)
#
#' @export
cluster_frequency <- function(
    object, 
    only = NULL, 
    clusters = NULL, 
    additional_data = NULL, 
    ...
) {
  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name
  
  if (is.character(selected_clusters)) {
    output_clusters <- c("Population", selected_clusters)
  } else if (is.factor(selected_clusters)) {
    output_clusters <- c("Population", as.character(levels(selected_clusters)))
  }
  
  n_id <- dplyr::n_distinct(
    dplyr::pull(clust$clustering, !!clust$variables$id)
  )
  selected_data <- clust$clustering |>
    dplyr::select(!!clust$variables$id, !!!selected_names) |>
    dplyr::distinct()
  
  res <- selected_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_names), ~"Population")
    ) |>
    dplyr::bind_rows(selected_data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        ~factor(., levels = output_clusters)
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_names), 
      names_to = "Clustering",
      values_to = "Cluster"
    ) |>
    dplyr::count(.data$Clustering, .data$Cluster, name = "Count") |>
    dplyr::mutate(Percent = 100 * .data$Count / n_id) |>
    dplyr::filter(.data$Cluster %in% output_clusters)
  
  class(res) <- c("summary.medic.cluster_frequency", class(res))
  
  return(res)
}




#' ATC Code Frequency Within Clusters
#'
#' The function `medications()` calculates the frequency of the different
#' unique ATC codes within each cluster.
#'
#' @inheritParams summary.medic
#'
#' @details
#' `medication_frequency()` calculates the number of individuals with a specific
#' ATC code within a cluster. Moreover, it calculates the percentage of people 
#' with this medication assigned to this cluster and the percent of people 
#' within the cluster with this medication.
#'
#' @return
#' `medication_frequency()` returns a data frame with class
#' `summary.medic.medication_frequency`.
#' * `Clustering` the name of the clustering.
#' * `Cluster` the cluster name.
#' * _atc_ ATC codes.
#' * `Count` number of individuals with this ATC code in this cluster.
#' * `Percent of All Medication` the percentage of individuals in the study 
#'   with this ATC code and cluster.
#' * `Percent of Medication in Cluster` the percent of individuals in the 
#'   cluster with this ATC code.
#' 
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# medication_frequency(clust, k == 5, clusters = I:III)
#
#' @export
medication_frequency <- function(
    object, 
    only = NULL, 
    clusters = NULL, 
    additional_data = NULL, 
    ...
) {
  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name
  
  if (is.character(selected_clusters)) {
    output_clusters <- c("Population", selected_clusters)
  } else if (is.factor(selected_clusters)) {
    output_clusters <- c("Population", as.character(levels(selected_clusters)))
  }
  
  n_id <- dplyr::n_distinct(
    dplyr::pull(clust$clustering, !!clust$variables$id)
  )
  selected_data <- clust$data |>
    dplyr::select(
      !!clust$variables$id,
      !!clust$variables$atc, 
      dplyr::all_of(selected_names)
    )
  
  res <- selected_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_names), ~"Population")
    ) |>
    dplyr::bind_rows(selected_data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        ~factor(., levels = output_clusters)
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_names), 
      names_to = "Clustering",
      values_to = "Cluster"
    ) |>
    dplyr::count(
      .data$Clustering, 
      .data$Cluster, 
      !!dplyr::sym(clust$variables$atc), 
      name = "Count"
    ) |>
    dplyr::mutate(tmp = .data$Cluster == "Population") |>
    dplyr::group_by(.data$Clustering, .data$Cluster) |> 
    dplyr::mutate(
      "Percent of Medication in Cluster" = .data$Count / sum(.data$Count)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(.data$Clustering, .data$tmp) |> 
    dplyr::mutate(
      "Percent of All Medication" = .data$Count / sum(.data$Count)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(
      .data$Clustering,
      !!dplyr::sym(clust$variables$atc),
      .data$tmp
    ) |> 
    dplyr::mutate(
      "Percent of ATC code" = .data$Count / sum(.data$Count)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::filter(.data$Cluster %in% output_clusters) |>
    dplyr::select(-"tmp") |>
    dplyr::arrange(.data$Clustering, .data$Cluster, dplyr::desc(.data$Count))
  
  class(res) <- c("summary.medic.medication_frequency", class(res))
  attr(res, "atc") <- clust$variables$atc
  
  return(res)
}




#' Frequency tables for medication amount
#'
#' The function `comedication_count()` calculates the number of unique
#' medications for each individual and presents the count frequencies by
#' cluster.
#'
#' @inheritParams summary.medic
#' @param count_grouper A function for grouping counts. As a standard it groups
#'   counts as 1 medication, 2 medications, and 3+ medications.
#'
#' @details
#' `comedication_count()` calculates the number of ATC codes an individual has, 
#' and then outputs the number of individuals within a cluster that has that 
#' many ATC codes. Moreover, various relevant percentages or calculated. See 
#' Value below for more details on these percentages. 
#' 
#' 
#' @return
#' `comedication_count()` returns a data frame of class 
#' `summary.medic.comedication_count`
#' * `Clustering` the name of the clustering.
#' * `Cluster` the name of the cluster. 
#' * `Medication Count` a number of medications. The numbers or groups are 
#'   given by the `count_grouper()` function.
#' * `Number of People` the number of individuals in cluster who has 
#'   `Medication Count` number of comedications in study.
#' * `Number of medications` the number of medications of individuals who has 
#'   `Medication Count` number of comedications in the cluster.
#' * `Percentage of All People` the percentage of individuals is study who has 
#'   `Medication Count` number of comedications in the cluster.
#' * `Percentage of People in Cluster` the percentage of individuals in the 
#'   cluster who has `Medication Count` number of comedications.
#' * `Percentage of All Medications` the percentage of medication in study from
#'   individuals who has `Medication Count` number of comedications in cluster.
#' * `Percentage of Medication in Cluster` the percentage of medication in 
#'   cluster from individuals who has `Medication Count` number of 
#'   comedications.
#' * `Percentage of People with the Same Medication Count` percentage of 
#'   individuals among those with `Medication Count` number of comedications in
#'   this cluster.
#' * `Percentage of Medication with the Same Medication Count` percentage of 
#'   medication among medication of individuals with `Medication Count` number 
#'   of comedications in this cluster.
#' 
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# comedication_count(clust, k == 5, clusters = I:III)
#
#' @export
comedication_count <- function(
  object,
  only = NULL, 
  clusters = NULL, 
  count_grouper = function(x) {
    cut(x, breaks = c(0, 1, 2, Inf), labels = c("1", "2", "3+"))
  }, 
  additional_data = NULL, 
  ...
) {
  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name
  
  if (is.character(selected_clusters)) {
    output_clusters <- c("Population", selected_clusters)
  } else if (is.factor(selected_clusters)) {
    output_clusters <- c("Population", as.character(levels(selected_clusters)))
  }
  
  if (is.null(count_grouper)) { count_grouper <- function(x) x }
  
  n_id <- dplyr::n_distinct(
    dplyr::pull(clust$clustering, !!clust$variables$id)
  )
  selected_data <- clust$data |> 
    dplyr::select(
      !!rlang::sym(clust$variables$id), 
      dplyr::all_of(selected_names)
    ) |>
    dplyr::mutate(population = FALSE)
  
  res <- selected_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_names), ~"Population"),
      population = TRUE
    ) |>
    dplyr::bind_rows(selected_data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        ~factor(., levels = output_clusters)
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_names), 
      names_to = "Clustering", 
      values_to = "Cluster"
    ) |>
    dplyr::count(
      !!rlang::sym(clust$variables$id), 
      .data$Clustering, 
      .data$population,
      .data$Cluster
    ) |>
    dplyr::mutate(n_exposures_grouped = count_grouper(.data$n)) |>
    dplyr::add_count(
      .data$Clustering, 
      .data$population,
      name = "n_people_in_analysis"
    ) |>
    dplyr::add_count(
      .data$Clustering, 
      .data$population,
      wt = .data$n, 
      name = "n_medications_in_analysis"
    ) |> 
    dplyr::add_count(
      .data$Clustering, 
      .data$population,
      .data$Cluster, 
      name = "n_people_in_cluster"
    ) |>
    dplyr::add_count(
      .data$Clustering,
      .data$population,
      .data$Cluster, 
      wt = .data$n, 
      name = "n_medications_in_cluster"
    ) |>
    dplyr::add_count(
      .data$Clustering, 
      .data$population,
      .data$n_exposures_grouped, 
      name = "n_people_with_m_medications_in_analysis"
    ) |> 
    dplyr::add_count(
      .data$Clustering, 
      .data$population,
      .data$n_exposures_grouped, 
      wt = .data$n, 
      name = "n_medications_with_m_medications_in_analysis"
    ) |>
    dplyr::filter(.data$Cluster %in% output_clusters) |>
    dplyr::group_by(
      .data$Clustering, 
      .data$population,
      .data$Cluster, 
      .data$n_exposures_grouped
    ) |>
    dplyr::summarize(
      n_people = dplyr::n(), 
      n_medications = sum(.data$n), 
      p_people_analysis = .data$n_people/.data$n_people_in_analysis[1], 
      p_people_cluster = .data$n_people/.data$n_people_in_cluster[1], 
      p_medications_in_analysis = .data$n_medications/.data$n_medications_in_analysis[1], 
      p_medications_in_cluster = .data$n_medications/.data$n_medications_in_cluster[1], 
      p_people_with_n_unique_medications = .data$n_people/.data$n_people_with_m_medications_in_analysis[1], 
      p_medications_with_n_unique_medications = .data$n_people/.data$n_medications_with_m_medications_in_analysis[1], 
      .groups = "drop"
    ) |>
    dplyr::rename(
      "Medication Count" = "n_exposures_grouped",
      "Number of People" = "n_people", 
      "Number of medications" = "n_medications", 
      "Percentage of All People" = "p_people_analysis", 
      "Percentage of People in Cluster" = "p_people_cluster", 
      "Percentage of All Medications" = "p_medications_in_analysis", 
      "Percentage of Medication in Cluster" = "p_medications_in_cluster", 
      "Percentage of People with the Same Medication Count" = "p_people_with_n_unique_medications", 
      "Percentage of Medication with the Same Medication Count" = "p_medications_with_n_unique_medications"
    ) |>
    dplyr::select(-"population") |>
    dplyr::arrange(.data$Clustering, .data$Cluster, .data$`Medication Count`)
  
  class(res) <- c("summary.medic.comedication_count", class(res))
  
  return(res)
}






#' Timing pattern frequency within clusters
#'
#' `timing_trajectory()` calculates the average timing paths within clusters.
#'
#' @inheritParams summary.medic
#'
#' @details
#' `timing_trajectory()` calculates both the number of unique timing 
#' trajectories in each cluster and the average timing trajectories in each 
#' cluster. 
#'
#' @return
#' `timing_trajectory()` returns a list of class 
#' `summary.medic.timing_trajectory` with two data frames:
#' 
#' ## average
#' * `Clustering` the name of the clustering.
#' * `Cluster` the cluster name.
#' * _timing variables_ the average timing value in the cluster.
#' * `Count` the number of people in the cluster.
#'
#' ## individual
#' * `Clustering` the name of the clustering.
#' * `Cluster` the cluster name.
#' * _timing variables_ unique timing pattern in the cluster.
#' * `Count` number of people with this unique timing pattern.
#'
# @examples
# clust <- medic(
#   complications,
#   id = id,
#   atc = atc,
#   k = 3:5,
#   timing = first_trimester:third_trimester
# )
#
# timing_trajectory(clust, k == 5, clusters = I:III)
#
#' @export 
timing_trajectory <- function(
    object,
    only = NULL, 
    clusters = NULL, 
    additional_data = NULL, 
    ...
) {
  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_name <- selected_analyses$cluster_name
  
  if (is.character(selected_clusters)) {
    output_clusters <- c("Population", selected_clusters)
  } else if (is.factor(selected_clusters)) {
    output_clusters <- c("Population", as.character(levels(selected_clusters)))
  }
  
  
  selected_data <- clust$data |>
    dplyr::select(
      dplyr::all_of(clust$variables$timing), 
      dplyr::all_of(selected_name)
    ) 
  
  summary_data <- selected_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_name), ~"Population")
    ) |>
    dplyr::bind_rows(selected_data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_name),
        ~factor(., levels = output_clusters)
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_name), 
      names_to = "Clustering", 
      values_to = "Cluster"
    ) |>
    dplyr::filter(.data$Cluster %in% output_clusters) |>
    dplyr::arrange(.data$Clustering, .data$Cluster)
  
  res <- list(
    "average" = summary_data |>
      dplyr::group_by(.data$Clustering, .data$Cluster) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(clust$variables$timing), mean), 
        Count = dplyr::n(),
        .groups = "drop"
      ), 
    "individual" = summary_data |> 
      dplyr::count(
        .data$Clustering, 
        .data$Cluster, 
        !!!rlang::syms(clust$variables$timing),
        name = "Count"
      )
  )
  
  class(res) <- c("summary.medic.timing_trajectory", class(res))
  attr(res, "timing") <- clust$variables$timing
  
  return(res)
}





#' Timing and ATC pattern interactions
#'
#' The function `timing_atc_group()` calculates the frequencies of distinct 
#' timing and ATC combinations within clusters.
#'
#' @inheritParams summary.medic
#'
#' @param atc_groups A data.frame specifying the ATC groups to summaries by or 
#'   a funciton that returns such a data.frame. The data.frame must have two 
#'   columns: 
#'   * `regex` giving regular expressions specifying the wanted ATC groups.
#'   * `atc_groups` the name of this ATC grouping.
#'   
#'   As a standard the anatomical level (first level) of the ATC codes is used.
#'
#' @details
#' `timing_atc_group()` calculates both the number of people with unique timing 
#' trajectory and ATC group, as given by `atc_groups`, in each cluster.
#'
#' @return
#' `timing_atc_group()` returns a list of class 
#' `summary.medic.timing_atc_group` with two data frames:
#' 
#' ## average
#' * `Clustering` the name of the clustering.
#' * `Cluster` the name of the cluster.
#' * `ATC Groups` the name of the ATC group. The groups are given by the 
#'   `atc_groups` input. 
#' * _timing variables_ the average timing value in the ATC group and cluster.
#' * `Number of Medications` the number of medications in the ATC group in 
#'   the cluster.
#' * `Percentage of Medications` the percentage of medication in the cluster
#'   with this ATC group.
#' * `Number of Distinct Timing Trajectories` the number of unique timing 
#'   trajectories in the ATC group in the cluster.
#'
#' ## individual
#' * `Clustering` the name of the clustering.
#' * `Cluster` the name of the cluster.
#' * _timing variables_ a unique timing pattern in the ATC group and cluster.
#' * `Number of Medications with Timing Trajectory` the number of medications
#'   with this unique timing trajectory and ATC group. 
#'
# @examples
# clust <- medic(
#   complications,
#   id = id,
#   atc = atc,
#   k = 3:5,
#   timing = first_trimester:third_trimester
# )
#
# timing_atc_group(clust, k == 5, clusters = I:III)
#
#' @export
timing_atc_group <- function(
    object, 
    only = NULL, 
    clusters = NULL, 
    atc_groups = default_atc_groups, 
    additional_data = NULL, 
    ...
) {
  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name
  
  if (is.character(selected_clusters)) {
    output_clusters <- c("Population", selected_clusters)
  } else if (is.factor(selected_clusters)) {
    output_clusters <- c("Population", as.character(levels(selected_clusters)))
  }
  
  by_name <- "regex"
  names(by_name) <- clust$variables$atc
  
  if (is.function(atc_groups)) {
    atc_groups <- atc_groups(object, ...)
  }
  all_atc_groups <- atc_groups$atc_groups
  
  
  selected_data <- clust$data |> 
    dplyr::select(
      dplyr::all_of(clust$variables$id), 
      dplyr::all_of(clust$variables$atc), 
      dplyr::all_of(clust$variables$timing), 
      dplyr::all_of(selected_names)
    )
  
  individual <- selected_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        ~"Population"
      )
    ) |>
    dplyr::bind_rows(selected_data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        ~factor(., levels = output_clusters)
      )
    ) |> 
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_names), 
      names_to = "Clustering",
      values_to = "Cluster"
    ) |> 
    dplyr::filter(.data$Cluster %in% output_clusters) |> 
    fuzzyjoin::regex_inner_join(atc_groups, by = by_name) |> 
    dplyr::rename("ATC Groups" = "atc_groups") |>
    dplyr::mutate(
      "ATC Groups" = factor(.data$`ATC Groups`, levels = all_atc_groups),
    ) |>
    dplyr::summarise(
      "Number of Medications with Timing Trajectory" = dplyr::n(),
      .by = dplyr::all_of(
        c(
          "Clustering", 
          "Cluster", 
          "ATC Groups", 
          clust$variables$timing
        )
      )
    ) |>
    dplyr::relocate(
      "Clustering", 
      "Cluster", 
      "ATC Groups", 
      "Number of Medications with Timing Trajectory"
    ) |>
    dplyr::arrange(
      .data$Clustering, 
      .data$Cluster, 
      .data$`ATC Groups`, 
      dplyr::desc(.data$`Number of Medications with Timing Trajectory`)
    ) 
  
  average <- individual |>
    dplyr::summarise(
      "Number of Medications" = 
        sum(.data$`Number of Medications with Timing Trajectory`),
      "Number of Distinct Timing Trajectories" = dplyr::n(),
      dplyr::across(
        dplyr::all_of(clust$variables$timing), 
        ~ sum(. * .data$`Number of Medications with Timing Trajectory`) / 
          sum(.data$`Number of Medications with Timing Trajectory`)
      ),
      .by = c("Clustering", "Cluster", "ATC Groups")
    ) |>
    dplyr::mutate(
      "Percentage of Medications" = .data$`Number of Medications` /
        sum(.data$`Number of Medications`),
      .by = c("Clustering", "Cluster")
    ) |>
    dplyr::relocate(
      "Number of Medications",
      "Percentage of Medications", 
      "Number of Distinct Timing Trajectories",
      .after = "ATC Groups"
    )
  
  res <- list("average" = average, "individual" = individual)
  class(res) <- c("summary.medic.timing_atc_group", class(res))
  attr(res, "timing") <- clust$variables$timing
  
  return(res)
}