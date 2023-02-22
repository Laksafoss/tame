#' The Frequency of Assignment to Each Cluster
#'
#' The function `frequencies()` calculates the number and frequency of
#' individuals assigned to each cluster.
#'
#' @inheritParams summary.medic
#'
#' @details
#' `frequencies()` calculates the number of individuals assigned to
#' each cluster and the associated frequency of assignment.
#'
#' @return
#' `frequencies()` returns a data frame with class
#' `summary.medic.frequencies`.
#' * `cluster_name` the name of the clustering.
#' * `cluster` the cluster name.
#' * `n` the number of observations assigned to this `cluster`.
#' * `p` the percent of observations assigned to this `cluster`.
#
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# # make frequency tables
# tame:::frequencies(clust, k == 5)
# tame:::frequencies(clust, k < 5, I:III)
#
#' @keywords internal
frequencies <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    additional_data = NULL,
    ...
) {

  clust <- enrich(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name
  n_id <- dplyr::n_distinct(
    dplyr::pull(clust$clustering, !!clust$variables$id)
  )

  res <- clust$clustering %>%
    dplyr::select(!!clust$variables$id, !!!selected_names) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_names),
      names_to = "cluster_name",
      values_to = "cluster"
    ) %>%
    dplyr::filter(.data$cluster %in% selected_clusters) %>%
    dplyr::count(.data$cluster_name, .data$cluster) %>%
    dplyr::mutate(percent = 100 * .data$n / n_id) %>%
    dplyr::left_join(clust$parameters, by = "cluster_name")

  return(res)
}




#' ATC Code Frequency Within Clusters
#'
#' The function `medications()` calculates the frequency of the different
#' unique ATC codes within each cluster.
#'
#' @inheritParams summary.medic
#'
#' @param cluster_wise TODO 
#' @param m A numeric restricting the number of distinct ATC codes plotted
#'   within each cluster. That is, the (at most) `m` most frequent ATC
#'   codes within that cluster is given a color.
#' @param q A numeric between 0 and 1 restricting the minimal ATC codes
#'   frequency displayed within each cluster.
#'
#' @details
#' `medication()` calculates the number of individuals with a specific ATC
#' code within a cluster. Moreover, it calculates the percentage of people with 
#' this medication assigned to this cluster and the percent of people within 
#' the cluster with this medication. 
#'
#' @return
#' `medications()` returns a data frame with class
#' `summary.medic.medications`.
#' * `cluster_name` the name of the clustering.
#' * `cluster` the cluster name.
#' * `atc` ATC codes.
#' * `n` number of people with this ATC code in this `cluster`.
#' * `p_analysis` the percentage of people with this ATC code assigned to this 
#'   `cluster`.
#' * `p_cluster` the percent of people within the `cluster` with this ATC code. 
#'
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# tame:::medications(clust, k == 5, clusters = I:III)
#
#' @keywords internal
medications <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    cluster_wise = TRUE, # do we need cluster_wise ?????????????
    m = 3,
    q = 0.05,
    additional_data = NULL,
    ...
) {

  clust <- enrich(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name

  res <- clust$data %>%
    dplyr::select(!!clust$variables$atc, dplyr::all_of(selected_names)) %>%
    tidyr::pivot_longer(
      cols = selected_names,
      names_to = "cluster_name",
      values_to = "cluster"
    ) %>%
    dplyr::count(
      .data$cluster_name,
      .data$cluster,
      .data[[clust$variables$atc]]
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::group_by(.data$cluster_name) %>%
    dplyr::mutate(p_analysis = .data$n / sum(.data$n)) %>% # could this not just be the total amount of medication?
    dplyr::ungroup() %>%
    dplyr::filter(.data$cluster %in% selected_clusters) %>%
    dplyr::group_by(.data$cluster_name, .data$cluster) %>%
    dplyr::mutate(p_cluster = .data$n / sum(.data$n)) %>%
    dplyr::slice(seq_len(min(m, dplyr::n()))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(if (cluster_wise) q <= .data$p_cluster else q <= .data$p_analysis)

  return(res)
}




#' Frequency tables for medication amount
#'
#' The function `amounts()` calculates the number of unique
#' medications for each individual and presents the count frequencies by
#' cluster.
#'
#' @inheritParams summary.medic
#' @param count_grouper A function for grouping counts. As a standard it groups
#'   counts as 1 medication, 2 medications, and 3+ medications.
#'
#' @details
#' `amounts()` calculates the number of ATC codes an individual has, and then 
#' outputs the number of individuals within a cluster that has that many ATC 
#' codes. Moreover, various relevant percentages or calculated. See Value below
#' for more details on these percentages. 
#' 
#' 
#' @return
#' `amounts()` returns a data frame of class `summary.medic.amounts`
#' * `cluster_name` the name of the clustering.
#' * `cluster` the cluster name.
#' * `m` number of ATC codes.
#' * `n_people` number of people in `cluster` that has `m` different ATC codes. 
#' * `n_medications` the total number of medication across people in this 
#'   `cluster` with `m` different ATC codes.
#' * `p_people_analysis` percentage of people in `cluster` with `m` different 
#'   ATC codes in analysis.
#' * `p_people_cluster` percentage of people with `m` different ATC codes in 
#'   `cluster`.
#' * `p_medications_in_analysis` percentage of medication given in `cluster` 
#'   with `m` different ATC codes in analysis.
#' * `p_medications_in_cluster` percentage of medication given with `m` 
#'   different ATC codes in `cluster`.
#' * `p_people_with_n_unique_medications` percentage of people in `cluster`
#'   with `m` different ATC codes.
#' * `p_medications_with_n_unique_medications` percentage of medication in 
#'   `cluster` with `m` different ATC codes. 
#'
# @examples
# clust <- medic(complications, id = id, atc = atc, k = 3:5)
#
# tame:::amounts(clust, k == 5, clusters = I:III)
#
#' @keywords internal
amounts <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    count_grouper = function(x) {
      cut(x, breaks = c(0, 1, 2, Inf), labels = c("1", "2", "3+"))
    },
    additional_data = NULL,
    ...
) {

  clust <- enrich(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name

  res <- clust$data %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      dplyr::all_of(selected_names)
    ) %>%
    tidyr::pivot_longer(
      cols = selected_names,
      names_to = "cluster_name",
      values_to = "cluster"
    ) %>%
    dplyr::count(
      !!rlang::sym(clust$variables$id),
      .data$cluster_name,
      .data$cluster
    ) %>%
    dplyr::mutate(
      n_exposures_grouped = count_grouper(.data$n)
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      name = "n_people_in_analysis"
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      wt = .data$n,
      name = "n_medications_in_analysis"
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      .data$cluster,
      name = "n_people_in_cluster"
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      .data$cluster,
      wt = .data$n,
      name = "n_medications_in_cluster"
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      .data$n_exposures_grouped,
      name = "n_people_with_m_medications_in_analysis"
    ) %>%
    dplyr::add_count(
      .data$cluster_name,
      .data$n_exposures_grouped,
      wt = .data$n,
      name = "n_medications_with_m_medications_in_analysis"
    ) %>%
    dplyr::filter(
      .data$cluster %in% selected_clusters
    ) %>%
    dplyr::group_by(
      .data$cluster_name,
      .data$cluster,
      .data$n_exposures_grouped
    ) %>%
    dplyr::summarize(
      n_people = dplyr::n(),
      n_medications = sum(.data$n),
      p_people_analysis = .data$n_people /
        .data$n_people_in_analysis[1],
      p_people_cluster = .data$n_people /
        .data$n_people_in_cluster[1],
      p_medications_in_analysis = .data$n_medications /
        .data$n_medications_in_analysis[1],
      p_medications_in_cluster = .data$n_medications /
        .data$n_medications_in_cluster[1],
      p_people_with_n_unique_medications = .data$n_people /
        .data$n_people_with_m_medications_in_analysis[1],
      p_medications_with_n_unique_medications = .data$n_people /
        .data$n_medications_with_m_medications_in_analysis[1],
      .groups = "drop") %>%
    dplyr::rename(
      m = "n_exposures_grouped"
    )

  return(res)
}





#' Timing pattern frequency within clusters
#'
#' `trajectories()` calculates the average timing paths within clusters.
#'
#' @inheritParams summary.medic
#'
#' @details
#' `trajectories()` calculates both the number of unique timing trajectories in 
#' each cluster and the average timing trajectories in each cluster. 
#'
#' @return
#' `trajectories()` returns a list of class `summary.medic.trajectories` with 
#' two data frames:
#' 
#' ## average
#' * `cluster_name` the name of the clustering.
#' * `cluster` the `cluster` name.
#' * _timing variables_ the average timing value in `cluster`.
#' * `n` the number of people in `cluster`.
#'
#' ## individual
#' * `cluster_name` the name of the clustering.
#' * `cluster` the cluster name.
#' * _timing variables_ a unique timing pattern in `cluster`.
#' * `n` number of people with this unique timing pattern.
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
# tame:::trajectories(clust, k == 5, clusters = I:III)
#
#' @keywords internal
trajectories <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    additional_data = NULL,
    ...
) {

  clust <- enrich(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_name <- selected_analyses$cluster_name

  selected_data <- clust$data %>%
    dplyr::select(
      dplyr::all_of(clust$variables$timing),
      dplyr::all_of(selected_name)
    ) %>%
    tidyr::pivot_longer(
      cols = selected_name,
      names_to = "cluster_name",
      values_to = "cluster"
    ) %>%
    dplyr::filter(
      .data$cluster %in% selected_clusters
    )

  res <- list(
    average = selected_data %>%
      dplyr::group_by(
        .data$cluster_name,
        .data$cluster
      ) %>%
      dplyr::summarise(
        dplyr::across(clust$variables$timing, mean),
        n = dplyr::n()
      ),
    individual = selected_data %>%
      dplyr::count(
        .data$cluster_name,
        .data$cluster,
        !!!rlang::syms(clust$variables$timing)
      )
  )

  return(res)
}




#' Timing and ATC pattern interactions
#'
#' The function `interactions()` calculates the frequencies of distinct timing
#' and ATC combinations within clusters.
#'
#' @inheritParams summary.medic
#'
#' @param atc_groups A data.frame specifying the ATC groups to summaries by. The
#'   data.frame must have two columns: 
#'   * `regex` giving regular expressions specifying the wanted ATC groups.
#'   * `atc_groups` the name of this ATC grouping.
#'   
#'   As a standard the anatomical level (first level) of the ATC codes is used.
#'
#' @details
#' `interactions()` calculates both the number of people with unique timing 
#' trajectory and ATC group, as given by `atc_groups`, in each cluster.
#'
#' @return
#' `interactions()` returns a data frame of class `summary.medic.interactions`
#' * `cluster_name` the name of the clustering.
#' * `cluster` the cluster name.
#' * `n` the number of people in the cluster.
#' * `atc_group` ATC groups as specified by the `atc_groups` input.
#' * _timing variable_ a unique timing pattern in `cluster` and `atc_group`.
#' * `n_interaction` number of people in this cluster with this timing and atc 
#'   group combination.
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
# tame:::interactions(clust, k == 5, clusters = I:III)
#
#' @keywords internal
interactions <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    atc_groups = data.frame(regex = paste0("^", LETTERS), atc_groups = LETTERS),
    additional_data = NULL,
    ...
) {

  clust <- enrich(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_clusters <- cluster_selector(clust, {{ clusters }})
  selected_names <- selected_analyses$cluster_name

  by_name <- "regex"
  names(by_name) <- clust$variables$atc
  all_atc_groups <- atc_groups$atc_groups

  res <- clust$data %>%
    dplyr::select(
      dplyr::all_of(clust$variables$id),
      dplyr::all_of(clust$variables$atc),
      dplyr::all_of(clust$variables$timing),
      dplyr::all_of(selected_names)
    ) %>%
    tidyr::pivot_longer(
      cols = selected_names,
      names_to = "cluster_name",
      values_to = "cluster"
    ) %>%
    dplyr::filter(.data$cluster %in% selected_clusters) %>%
    dplyr::group_by(.data$cluster_name, .data$cluster) %>%
    dplyr::mutate(
      n = dplyr::n_distinct(!!rlang::sym(clust$variables$id))
    ) %>%
    dplyr::ungroup() %>%
    fuzzyjoin::regex_inner_join(atc_groups, by = by_name) %>%
    dplyr::arrange(factor(.data$atc_groups, levels = all_atc_groups)) %>%
    dplyr::left_join(clustering$parameters, by = "cluster_name") %>%
    #dplyr::mutate(atc_groups = {{ atc_groups }}) %>% # this only allows one group per row -- e.g. N06A should be allowed to participate in both N and N06 and N06A
    dplyr::count(
      .data$cluster_name,
      .data$cluster,
      .data$n,
      !!!rlang::syms(clust$variables$timing),
      .data$atc_groups,
      name = "n_interaction" # this used to be called "n" !!!
    ) %>%
    dplyr::relocate("cluster_name", "cluster", "n", "atc_groups")

  return(res)

}
