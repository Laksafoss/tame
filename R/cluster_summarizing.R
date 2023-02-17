


# CLUSTER SUMMARIZING PLOT   ===================================================
#' Cluster summarizing plot
#'
#' -- one line explanation --
#'
#' @inheritParams plot-params
#' @param atc_groups 123
#' @param count_grouper 123
#' @param m 213
#'
#' @details
#' -- explain the deal with m = Inf, q = 0 --
#'
#' @return
#' a data frame -- bla bla --
#'
#' @export
#'
#' @examples
#' 1+1
#'
#' @export
cluster_summarizing_plot <- function(
    clustering,
    identifier, # select the first cluster_name
    clusters = NULL,
    atc_groups = data.frame(regex = paste0("^", LETTERS), atc_groups = LETTERS),
    count_grouper = function(x) {cut(x, breaks = c(0, 1, 2, Inf),
                                     labels = c("1", "2", "3+"))},
    m = 3,
    additional_data = NULL) {

  clust <- enrich_clustering_parameters(clustering, additional_data)
  selected_analyses <- method_selector(clust, {{ identifier }})
  selected_clusters <- cluster_selector(clust, {{ clusters }}) # make this work
  selected_methods <- selected_analyses$cluster_name
  if (length(selected_methods) > 1) {
    stop("You must choose exactly one clustering to summarize")
  }
  if (length(clust$variables$timing) == 0) {
    stop("We can currenly only do this for clusterings with timing....")
  }

  # helpers --------------------------------------------------------------------
  time_points <- unique(names(clust$key$unique_timing)[-1])
  width <- round(length(time_points) / 2)
  mid <- time_points[width]
  time_scale <- time_points[unique(round(
    seq(1, length(time_points), length.out = 5)))]



  # medication amount ----------------------------------------------------------
  # medication amount data
  med_amount <- medication_amount(clust,
                                  only = {{ identifier }},
                                  clusters = {{ clusters }},
                                  count_grouper = count_grouper) %>%
    dplyr::rename(p = .data$p_people_cluster,
                  col_var = .data$n_unique_exposures) %>%
    dplyr::select(.data$cluster_name, .data$cluster, .data$col_var, .data$p) %>%
    dplyr::mutate(facet = "Medication\nAmount")

  # medication amount data for total column
  med_amount_total <- medication_amount(clust,
                                        only = {{ identifier }},
                                        clusters = NULL,
                                        count_grouper = count_grouper) %>%
    dplyr::rename(col_var = .data$n_unique_exposures) %>%
    dplyr::mutate(p = .data$n_people / sum(.data$n_people)) %>%
    dplyr::group_by(.data$cluster_name, .data$col_var) %>%
    dplyr::summarise(p = sum(.data$p), .groups = "drop") %>%
    dplyr::mutate(cluster = "Total", facet = "Medication\nAmount")

  # join the cluster specific and total medication amount data
  med_amount_comb <- dplyr::bind_rows(med_amount, med_amount_total) %>%
    dplyr::mutate(col_var = as.character(.data$col_var))


  # ATC frequency --------------------------------------------------------------
  # atc frequency
  atc_freq <- atc_frequency(clust,
                            only = {{ identifier }},
                            clusters = {{ clusters }},
                            q = 0,
                            m = m) %>%
    dplyr::rename(p = .data$p_cluster, col_var = clust$variables$atc) %>%
    dplyr::select(.data$cluster_name, .data$cluster, .data$col_var, .data$p) %>%
    dplyr::group_by(.data$cluster_name, .data$cluster) %>%
    tidyr::complete(col_var = "Remaining") %>%
    dplyr::mutate(p = dplyr::if_else(is.na(.data$p),
                                     1 - sum(.data$p, na.rm = TRUE),
                                     .data$p),
                  facet = "ATC\nFrequency") %>%
    dplyr::ungroup()

  # atc frequency for total column
  atc_freq_total <- atc_frequency(clust,
                                  only = {{ identifier }},
                                  clusters = NULL,
                                  q = 0,
                                  m = Inf) %>%
    dplyr::rename(col_var = clust$variables$atc) %>%
    dplyr::group_by(.data$cluster_name, .data$col_var) %>%
    dplyr::summarise(p = sum(.data$p_analysis), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$p)) %>%
    dplyr::slice(seq_len(m)) %>%
    dplyr::group_by(.data$cluster_name) %>%
    tidyr::complete(col_var = "Remaining") %>%
    dplyr::mutate(p = dplyr::if_else(is.na(.data$p),
                                     1 - sum(.data$p, na.rm = TRUE),
                                     .data$p),
                  facet = "ATC\nFrequency",
                  cluster = "Total") %>%
    dplyr::ungroup()

  # join the cluster specific and total atc frequencies
  atc_freq_comb <- dplyr::bind_rows(atc_freq, atc_freq_total) %>%
    dplyr::mutate(col_var = as.character(.data$col_var))

  # Timing avarage -------------------------------------------------------------
  # time average
  time_ave <- timing_average(clust,
                             only = {{ identifier }},
                             clusters = {{ clusters }})$average %>%
    dplyr::select(.data$cluster_name, .data$cluster, !!!rlang::syms(time_scale)) %>% # whare is the issue here
    tidyr::pivot_longer(cols = time_scale,
                        names_to = "timing",
                        values_to = "q") %>%
    dplyr::mutate(facet = "Average\nCluster\nTrajectory")

  # time average for total column
  time_ave_total <- timing_average(clust,
                                   only = {{ identifier }},
                                   clusters = NULL)$individual %>%
    dplyr::group_by(.data$cluster_name) %>%
    dplyr::summarise_at(names(clust$key$unique_timing)[-1],
                        ~sum(. * .data$n) / sum(.data$n),
                        .groups = "drop") %>%
    tidyr::pivot_longer(cols = time_scale,
                        names_to = "timing",
                        values_to = "q") %>%
    dplyr::mutate(cluster = "Total",
                  facet = "Average\nCluster\nTrajectory")

  # join the cluster specific and total time avarages
  time_ave_comb <- dplyr::bind_rows(time_ave, time_ave_total)


  # ATC group specific timings -------------------------------------------------
  if (is.character(atc_groups)) { # should we make more options in the future
    if (atc_groups == "most_frequent") {
      most_frequent <- unique(atc_freq_total$col_var)
      atc_groups <- data.frame(regex = most_frequent, atc_groups = most_frequent)
    } else if (atc_groups == "both") {
      most_frequent <- unique(atc_freq_total$col_var)
      atc_groups <- data.frame(regex = c(paste0("^", LETTERS), most_frequent),
                               atc_groups = c(LETTERS, most_frequent))
    }
  }


  # cluster specific timing for atc interaction groups
  time_atc <- timing_atc_interaction(
    clust,
    only = {{ identifier }},
    clusters = {{ clusters }},
    atc_groups = {{ atc_groups }}) %>%
    dplyr::rename(facet = .data$atc_groups) %>%
    dplyr::group_by(.data$cluster_name, .data$cluster, .data$facet) %>%
    dplyr::summarise(dplyr::across(clust$variables$timing,
                                   ~ sum(. * .data$n) / sum(.data$n)),
                     .groups = "drop") %>%
    tidyr::pivot_longer(cols = clust$variables$timing,
                        names_to = "timing",
                        values_to = "q")


  # avarage timing for atc interaction groups
  time_atc_total <- timing_atc_interaction(
    clust,
    only = {{ identifier }},
    clusters = NULL,
    atc_groups = {{ atc_groups }}) %>%
    dplyr::rename(facet = .data$atc_groups) %>%
    dplyr::group_by(.data$cluster_name, .data$facet) %>%
    dplyr::summarise(dplyr::across(clust$variables$timing,
                                   ~ sum(. * .data$n) / sum(.data$n)),
                     .groups = "drop") %>%
    tidyr::pivot_longer(cols = clust$variables$timing,
                        names_to = "timing",
                        values_to = "q") %>%
    dplyr::mutate(cluster = "Total")


  by_name <- "regex"
  names(by_name) <- clust$variables$atc

  text_data <- clust$data %>%
    dplyr::select(clust$variables$id, clust$variables$atc, !!rlang::sym(selected_methods)) %>%
    dplyr::mutate(dummy = "Total") %>%
    tidyr::pivot_longer(cols = c(selected_methods, "dummy"),
                        names_to = "rm", values_to = "cluster") %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::mutate(n_cluster = dplyr::n_distinct(!!rlang::sym(clust$variables$id))) %>%
    dplyr::ungroup() %>%
    fuzzyjoin::regex_inner_join(atc_groups, by = by_name) %>%
    dplyr::group_by(.data$cluster, .data$atc_groups) %>%
    dplyr::summarise(n_users = dplyr::n_distinct(!!rlang::sym(clust$variables$id)),
                     n_cluster = .data$n_cluster[1],
                     .groups = "drop") %>%
    dplyr::mutate(percent_text = paste0(
      .data$n_users, " (",
      formatC(100 * .data$n_users / .data$n_cluster, digits = 1, format = "f"),
      "%)")) %>%
    dplyr::mutate(timing = mid) %>%
    dplyr::rename(facet = atc_groups)


  # joining all the atc interaction group specific timing information
  time_atc_comb <- dplyr::bind_rows(time_atc, time_atc_total) %>%
    dplyr::left_join(text_data, by = c("cluster", "timing", "facet"))


  # cluster size and percent of total ------------------------------------------
  # cluster frequency
  freqs <- cluster_frequency(clust,
                             only = {{ identifier }},
                             clusters = {{ clusters }}) %>%
    dplyr::select(.data$cluster_name, .data$cluster, .data$n, .data$percent) %>%
    dplyr::arrange(.data$cluster) %>%
    dplyr::mutate(percent = paste0(formatC(.data$percent,
                                           digits = 1,
                                           format = "f"), "%"),
                  n = paste0("N = ", .data$n))

  # total size
  freqs_total <- cluster_frequency(clust,
                                   only = {{ identifier }},
                                   clusters = NULL) %>%
    dplyr::group_by(.data$cluster_name) %>%
    dplyr::summarise(n = paste0("N = ", sum(.data$n)),
                     percent = paste0(sum(.data$percent), "%")) %>%
    dplyr::mutate(cluster = "Total")

  # join cluster and total count information
  freqs_comb <- dplyr::bind_rows(freqs, freqs_total)


  # plot color helpers ---------------------------------------------------------


  # amount color helper
  unique_amounts <- sort(unique(med_amount$col_var))
  amount_col <- scales::viridis_pal()(length(unique_amounts))
  names(amount_col) <- unique_amounts

  # atc color helper
  unique_atc <- unique(atc_freq$col_var)
  unique_atc <- sort(unique_atc[!stringr::str_detect(unique_atc, "Remaining")])
  atc_col <- scales::hue_pal()(length(unique_atc))
  names(atc_col) <- unique_atc

  # making the color scale
  color_help <- c("**Medication**<br>**Amount**<br>" =  0, amount_col,
                  "<br><br>**ATC codes**<br>" =  0, atc_col,
                  "Remaining" = "#808080")

  # make plotting data ---------------------------------------------------------

  # combining all the data from above
  plot_data <- dplyr::full_join(med_amount_comb,
                                atc_freq_comb,
                                by = c("cluster_name", "cluster", "facet",
                                       "col_var", "p")) %>%
    dplyr::mutate(timing = mid) %>%
    dplyr::full_join(time_ave_comb,
                     by = c("cluster_name", "cluster", "timing", "facet")) %>%
    dplyr::full_join(time_atc_comb,
                     by = c("cluster_name", "cluster", "timing", "facet", "q")) %>%
    dplyr::full_join(freqs_comb, by = c("cluster_name", "cluster")) %>%
    dplyr::mutate(
      facet = factor(.data$facet,
                     levels = c("Medication\nAmount",
                                "ATC\nFrequency",
                                "Average\nCluster\nTrajectory",
                                atc_groups$atc_groups)), #unique(time_atc$facet)
      cluster = factor(.data$cluster,
                       levels = c("Total",
                                  as.character(unique(med_amount$cluster)))))

  # adding the empty placeholders for legend headlines
  plot_data <- plot_data[1:2,] %>%
    dplyr::mutate(col_var = c("**Medication**<br>**Amount**<br>",
                              "<br><br>**ATC codes**<br>"),
                  p = NA,
                  q = NA) %>%
    dplyr::bind_rows(plot_data) %>%
    dplyr::mutate(percent_text = ifelse(.data$timing == mid,
                                        percent_text,
                                        NA_character_))


  # make plot ------------------------------------------------------------------
  p <- plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = .data$timing,
                                   y = .data$p,
                                   fill = .data$col_var,
                                   group = .data$cluster_name),
                      width = width) +
    ggplot2::geom_line(ggplot2::aes(x = .data$timing,
                                    y = .data$q,
                                    group = .data$cluster_name)) +
    ggplot2::geom_text(ggplot2::aes(x = .data$timing, y = 0.08, label = percent_text),
                       size = 3.5) +
    ggplot2::facet_grid(.data$facet ~ .data$cluster + .data$n + .data$percent) +
    #ggh4x::force_panelsizes(rows = c(rep(2, 2), rep(1.3, nrow(time_atc_names) + 1))) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_discrete(breaks = time_scale) +
    ggplot2::scale_fill_manual(values = color_help) +
    ggplot2::theme(legend.text = ggtext::element_markdown(),
                   legend.title = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(fill = NA),
                   axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))

  return(p)
}

#' @export
cluster_summarising_plot <- cluster_summarizing_plot
