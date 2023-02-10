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
#' * \code{cluster_name} the clustering cluster_name name.
#' * \code{cluster}  the name of the specific cluster.
#' * \code{n} the number of observations assigned to this cluster.
#' * \code{p} the percent of observations assigned to this cluster within this
#'   cluster.
#'
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)
#'
#' # make frequency tables
#' tame:::frequencies(clust, k == 5)
#' tame:::frequencies(clust, k < 5, I:III)
#'
#' @keywords internal
frequencies <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    additional_data = NULL
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
    #dplyr::arrange(.data$cluster_name) %>%
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
#' @param m A numeric restricting the number of distinct ATC codes plotted
#'   within each cluster. That is, the (at most) `m` most frequent ATC
#'   codes within that cluster is given a color.
#' @param q A numeric between 0 and 1 restricting the minimal ATC codes
#'   frequency displayed within each cluster.
#'
#' @details
#' TODO
#'
#' @return
#' `medications()` returns a data frame with class
#' `summary.medic.medications`.
#' * \code{x} very.
#' * \code{y} important.
#' * \code{z} info.
#'
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)
#'
#' tame:::medications(clust, k == 5, clusters = I:III)
#'
#' @keywords internal
medications <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    cluster_wise = TRUE, # do we need cluster_wise ?????????????
    m = 3,
    q = 0.05,
    additional_data = NULL
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
#' TODO
#'
#' @return
#' `amounts()` returns a data frame of class `summary.medic.amounts`
#' * \code{x} very.
#' * \code{y} important.
#' * \code{z} info.
#'
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3:5)
#'
#' tame:::amounts(clust, k == 5, clusters = I:III)
#'
#' @keywords internal
amounts <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    count_grouper = function(x) {
      cut(x, breaks = c(0, 1, 2, Inf), labels = c("1", "2", "3+"))
    },
    additional_data = NULL
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
      n_unique_exposures = "n_exposures_grouped"
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
#' TODO
#'
#' @return
#' `trajectories()` returns a data frame of class `summary.medic.trajectories`
#' * \code{x} very.
#' * \code{y} important.
#' * \code{z} info.
#'
#'
#' @examples
#' clust <- medic(
#'   tiny_example_data,
#'   id = id,
#'   atc = atc,
#'   k = 3:5,
#'   timing = first_trimester:third_trimester
#' )
#'
#' tame:::trajectories(clust, k == 5, clusters = I:III)
#'
#' @keywords internal
trajectories <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    additional_data = NULL
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
#' @param atc_groups 123
#'
#' @details
#' TODO
#'
#' @return
#' `interactions()` returns a data frame of class `summary.medic.interactions`
#' * \code{x} very.
#' * \code{y} important.
#' * \code{z} info.
#'
#' @examples
#' clust <- medic(
#'   tiny_example_data,
#'   id = id,
#'   atc = atc,
#'   k = 3:5,
#'   timing = first_trimester:third_trimester
#' )
#'
#' tame:::interactions(clust, k == 5, clusters = I:III)
#'
#' @keywords internal
interactions <- function(
    clustering,
    only = NULL,
    clusters = NULL,
    atc_groups = data.frame(regex = paste0("^", LETTERS), atc_groups = LETTERS),
    additional_data = NULL
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
      cluster_size = dplyr::n_distinct(!!rlang::sym(clust$variables$id))
    ) %>%
    dplyr::ungroup() %>%
    fuzzyjoin::regex_inner_join(atc_groups, by = by_name) %>%
    dplyr::arrange(factor(.data$atc_groups, levels = all_atc_groups)) %>%
    dplyr::left_join(clustering$parameters, by = "cluster_name") %>%
    #dplyr::mutate(atc_groups = {{ atc_groups }}) %>% # this only allows one group per row -- e.g. N06A should be allowed to participate in both N and N06 and N06A
    dplyr::count(
      .data$cluster_name,
      .data$cluster,
      .data$cluster_size,
      !!!rlang::syms(clust$variables$timing),
      .data$atc_groups
    ) %>%
    dplyr::left_join(clustering$parameters, by = "cluster_name")

  return(res)

}
