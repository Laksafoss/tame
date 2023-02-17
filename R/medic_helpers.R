#' Internal option constructor
#'
#' Given the input of the \code{medic} this function checks the
#' input and constructs a data frame with the analysis parameters specified by
#' the user.
#'
#' @inheritParams medic
#' 
#' @return 
#' A data.frame with the parameters for clustering. 
#' 
#' @examples
#' parameters_constructor(
#'    data = complications, 
#'    k = 3, 
#'    id = id,
#'    atc = atc
#' )
#' 
#' @export
parameters_constructor <- function(
    data, # CAN WE AVOID EXPORTING THIS ????<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<??
    id,
    k = 5,
    atc,
    timing,
    base_clustering,
    linkage = "complete",
    summation_method = "sum_of_minima",
    alpha = 1,
    beta = 1,
    gamma = 1,
    p = 1,
    theta = (5:0) / 5,
    ...) {

  na_cols <- data %>%
    dplyr::select({{ id }}, {{ atc }}, {{ timing }}, {{ base_clustering }}) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ any(is.na(.))))
  
  if (any(na_cols)) {
    stop("There are one or more columns in 'data' with NA's")
    # we know which columns it is - we can give the name if we want
  }
  

  if (missing(id)) {
    stop("An 'id' variable must be specified.")
  }

  if (missing(atc)) {
    stop("An 'atc' variable must be specified.")
  }

  if (any(unlist(theta) != 0) & missing(atc)) {
    stop("When 'theta' contains values != 0 then 'atc' must be specified.")
  }

  if (any(beta != 0) & missing(timing) & missing(atc)) {
    stop("When 'beta' != 0 then 'timing' or 'atc' must be specified.")
  }

  if (missing(timing)) {
    gamma <- 0
  }

  if (! missing(base_clustering)) {
    test <- data %>%
      dplyr::group_by({{ id }}) %>%
      dplyr::summarise(n = dplyr::n_distinct({{ base_clustering }}),
                       .groups = "drop") %>%
      dplyr::summarise(test = any(.data$n > 1), .groups = "drop") %>%
      dplyr::pull(.data$test)
    if (test) {
      stop(paste0("The specified 'base_clustering' assigns people to multiple",
                  " clusters!\nThis is not meaningfull."))
    }
  }

  # Should we warn people that they have specified timing but turned it off?
  # if (all(beta == 0) & (!missing(timing))) {}
  # if (all(gamma == 0) & (!missing(amount))) {}


  #   ===   checking clustering options   ======================================

  return(tryCatch({
    df <- tibble::tibble(
      linkage = linkage,
      summation_method = summation_method,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      p = p,
      theta_list = if (is.list(theta)) theta else list(theta)) %>%
      dplyr::mutate(theta = as.character(theta_list),
                    clustering = paste0("cluster_", dplyr::row_number())) %>%
      dplyr::relocate("clustering")
  }, error = function(cond) {
    message(paste0("The parameters 'alpha', 'beta', 'gamma', 'p', 'theta' and ",
                   "'linkage'\n should have length 1 or equal length."))
    stop(cond)
  }, finally = {df}))
}





#' Internal constructor of ATC, timing and amount keys
#'
#' Using the relevant columns from data to construct a list of keys for
#' identifying unique ATC codes, timing trajectories, and ATC-timing patterns.
#'
#' @inheritParams medic
#' @noRd
key_constructor <- function(data, id, base_clustering, atc, timing) {

  key <- data %>%
    dplyr::select(".internal_character_id",
                  {{ id }},
                  {{ base_clustering }},
                  {{ atc }},
                  {{ timing }})


  #   ===   Unique ATC codes   =================================================

  if (data %>% dplyr::select({{ atc }}) %>% ncol() != 0) {

    unique_atc <- data %>%
      dplyr::select({{ atc }}) %>%
      dplyr::distinct() %>%
      dplyr::mutate(unique_atc_key = dplyr::row_number()) %>%
      dplyr::relocate("unique_atc_key")

    key <- key %>%
      dplyr::left_join(unique_atc, by = names(unique_atc)[-1])

    out <- list(unique_atc = unique_atc)

  } else {

    out <- NULL

  }


  #   ===   Unique Timing codes   ==============================================

  if (data %>% dplyr::select({{ timing }}) %>% ncol() != 0) {

    unique_timing <- data %>%
      dplyr::select({{ timing }}) %>%
      dplyr::distinct() %>%
      dplyr::mutate(unique_timing_key = dplyr::row_number()) %>%
      dplyr::relocate("unique_timing_key")

    key <- key %>%
      dplyr::left_join(unique_timing, by = names(unique_timing)[-1])

    out <- c(out, list(unique_timing = unique_timing))

  }

  #   ===   Unique Exposure   ==================================================

  unique_exposure <- key %>%
    dplyr::select(dplyr::any_of(c("unique_atc_key", "unique_timing_key"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(unique_exposure_key = dplyr::row_number()) %>%
    dplyr::relocate("unique_exposure_key")

  key <- key %>%
    dplyr::left_join(unique_exposure, by = names(unique_exposure)[-1])

  out <- c(out, list(unique_exposure = unique_exposure))


  #   ===  Unique patterns   ===================================================


  nest_key <- key %>%
    tidyr::nest(pattern = unique(unlist(lapply(out, names)))) # should it be split into keys and actuall data?

  unique_patterns <- nest_key %>%
    dplyr::select("pattern") %>%
    dplyr::distinct() %>%
    dplyr::mutate(unique_pattern_key = dplyr::row_number(),
                  n_unique_exposures = sapply(.data$pattern, nrow)) %>%
    dplyr::relocate("unique_pattern_key", "n_unique_exposures")

  key <- nest_key %>%
    dplyr::left_join(unique_patterns, by = "pattern") %>%
    tidyr::unnest("pattern")

  out <- c(out, list(unique_patterns = unique_patterns))

  if (data %>% dplyr::select({{ base_clustering }}) %>% ncol() != 0) {
    base_clu <- key %>%
      dplyr::select({{ base_clustering }}, "unique_pattern_key") %>%
      dplyr::distinct()
    out <- c(out, list(base_clustering = base_clu))
  }

  #   ===   Key sanity check   =================================================

  if (nrow(key) != nrow(data)) {            # this error should be a unit test !
    stop("Something went wrong in the key construction")
  }


  #   ===   Construct Reduced Keys   ===========================================

  rms <- key %>%
    dplyr::select(".internal_character_id",
                  {{ id }}, # added recently - is this correct?
                  {{ base_clustering }},
                  {{ atc }},
                  {{ timing }}) %>%
    names()

  reduced_key <- key %>%
    dplyr::select(-dplyr::any_of(rms)) %>%
    dplyr::distinct()

  #   ===   Return Results   ===================================================

  return(c(list(key = key, reduced_key = reduced_key), out))
}



#' Internal metric look-up table constructors
#'
#' Takes the list of ATC, timing, and pattern keys and computes the named
#' matrixs for the ATC and timing metric and normalizer.
#'
#' @param keys list of keys from `key_constructor`
#' @noRd
lookup_constructor <- function(keys, parameters) {

  # atc lookup
  if (!is.null(keys$unique_atc)) {
    out <- list(atc_lookup_table = atc_metric_lookup_constructor(
      keys$unique_atc))
  } else {
    out <- NULL
  }

  # normalizer
  if (!is.null(keys$unique_patterns)) {
    out <- c(out, list(
      normalizing_factor = normalizing_lookup_constructor(
        keys$unique_patterns,
        unique(parameters$summation_method))))

  }

  # timing lookup
  if (!is.null(keys$unique_timing)) {
    out <- c(out, list(
      timing_lookup_table = timing_metric_lookup_constructor(
        keys$unique_timing, unique(parameters$p))))

  }

  return(out)
}



#' Internal atc metric look-up table constructors
#'
#' Takes a vector of ATC codes and computes the ATC-distance between all
#' combinations and outputs it as a names matrix.
#'
#' @param unique_atc data frame
#' @noRd
atc_metric_lookup_constructor <- function(unique_atc) {

  atc_codes <- unique_atc %>%
    dplyr::select(-"unique_atc_key") %>%
    dplyr::pull(1)

  ATC <- data.frame(level1 = stringr::str_sub(atc_codes, 1, 1),
                    level2 = stringr::str_sub(atc_codes, 1, 3),
                    level3 = stringr::str_sub(atc_codes, 1, 4),
                    level4 = stringr::str_sub(atc_codes, 1, 5),
                    level5 = stringr::str_sub(atc_codes, 1, 7))

  res <- apply(ATC, 1, function(x) {
    apply(ATC, 1, function(y) {
      max((x == y) * c(1:5))
    })
  })

  atc_names <- unique_atc %>% dplyr::pull(.data$unique_atc_key)
  dimnames(res) <- list(atc_names, atc_names)

  return(res + 1)
}






#' Internal metric look-up table constructors
#'
#' Takes a vector of unique patterns and the chosen summation methods and
#' calculates the named matrix(s) of normalizers.
#'
#' @param unique_patterns data frame
#' @param summation_methods character vector with the chosen 'summation_method's
#' @noRd
normalizing_lookup_constructor <- function(unique_patterns,
                                           summation_methods = "double_sum") {

  int_exposure_in_pattern <- unique_patterns %>%
    dplyr::pull(.data$n_unique_exposures, name = .data$unique_pattern_key)
  numeric_exposure_in_pattern <- as.numeric(int_exposure_in_pattern)

  res <- NULL

  if (any(summation_methods == "double_sum")) {
    multiplied <- Rfast::Outer(numeric_exposure_in_pattern,
                               numeric_exposure_in_pattern,
                               oper = "*")
    rownames(multiplied) <- colnames(multiplied) <- names(int_exposure_in_pattern)
    res <- c(res, list("double_sum" = 1 / multiplied))
  }

  if (any(summation_methods == "sum_of_minima")) { # nothing needs to be done here !?
    p <- length(numeric_exposure_in_pattern)
    repeated <- matrix(numeric_exposure_in_pattern,
                       ncol = p, nrow = p, byrow = TRUE)
    rownames(repeated) <- colnames(repeated) <- names(int_exposure_in_pattern)
    res <- c(res, list("sum_of_minima" = 1 / repeated))
  }


  return(res)
}





#' Internal timing metric look-up table constructors
#'
#' Takes the timing key and chosen minkowski powers and outputs a list of
#' matrixs of timing distances.
#'
#' @param unique_timing data frame
#' @param ps the unique 'p's chosen
#' @noRd
timing_metric_lookup_constructor <- function(unique_timing, ps) {
  res <- lapply(ps, function(p) {
    Rfast::Dist(
      dplyr::select(unique_timing, - "unique_timing_key"), # what is going on here is this correct ??
      method = "minkowski", p = p) / ((ncol(unique_timing) - 1) ^ p)
  })
  names(res) <- as.character(ps)
  return(res)
}



#' Internal method for translating the general look-ups to the specific
#'
#' Takes a specific method and creates method-specific lookup tables for the
#' context via the general lookup tables.
#'
#' @param method A 1-row data frame with chosen method parameters.
#' @param lookup_tables A list of general look-up tables from
#'    `lookup_constructor`.
#' @noRd
context_lookup <- function(method, lookup_tables) {

  # normalizer
  if (method$alpha == 1) {
    out <- list(
      normalizing_factor = lookup_tables$normalizing_factor[[method$summation_method]])
  } else if (method$alpha != 0) {
    out <- list(
      normalizing_factor = lookup_tables$normalizing_factor[[method$summation_method]] ^ method$alpha)
  } else {
    out <- list(normalizing_factor = 1)
  }


  if (method$beta != 0) {

    # atc metric
    if (all(method$theta != 0)) {
      out <- c(out, list(atc_table = matrix(
        method$theta_list[[1]][lookup_tables$atc_lookup_table],
        nrow(lookup_tables$atc_lookup_table),
        ncol(lookup_tables$atc_lookup_table))))
    } else {
      out <- NULL
    }


    # timing metric
    if (method$gamma == 1) {
      out <- c(out, list(
        timing_table = lookup_tables$timing_lookup_table[[as.character(method$p)]]))
    } else if (method$gamma != 0) {
      out <- c(out, list(
        timing_table = lookup_tables$timing_lookup_table[[as.character(method$p)]] * method$gamma))
    } else {
      out <- c(out, list(timing_table = 0))
    }
  }

  return(out)

}





#' Internal distance matrix constructor
#'
#' Takes all keys, the method in the current context and the method-specific
#' look-up tables and and computes the distance matrix for HCA.
#'
#' @param keys A list of data frames from `key_constructor`.
#' @param method A 1-row data frame with chosen method parameters.
#' @param cur_tables A list of method-specific look-up tables from
#'    `context_lookup`.
#' @param old_patterns A vector of pattern keys in the original clustered data.
#'    Must be specified when applying a clustering to new data otherwise it
#'    should be `NULL`.
#' @param new_patterns A vector of pattern keys in the new un-clustered data.
#'    Must be specified when applying a clustering to new data otherwise it
#'    should be `NULL`.
#' @noRd
distance_matrix_constructor <- function(keys,
                                        method,
                                        cur_tables,
                                        old_patterns = NULL,
                                        new_patterns = NULL) {

  if ((!is.null(old_patterns)) & (!is.null(new_patterns))) {
    rows <- keys$reduced_key %>%
      dplyr::filter(.data$unique_pattern_key %in% old_patterns)
    cols <- keys$reduced_key %>%
      dplyr::filter(.data$unique_pattern_key %in% new_patterns)
    calc <- "full"

    if (method["alpha"] != 0) {
      normalizer <- cur_tables$normalizing_factor[
        which(keys$unique_patterns$unique_pattern_key %in% old_patterns),
        which(keys$unique_patterns$unique_pattern_key %in% new_patterns)]
    }
  } else {
    rows <- cols <- keys$reduced_key
    calc <- "triangle"

    if (method["alpha"] != 0) {
      normalizer <- cur_tables$normalizing_factor
    }
  }

  if (method$beta != 0) {

    inner_terms <- 1

    # atc metric by pattern
    if (all(method$theta != 0)) {
      inner_terms_atc <- apply(rows, 1, function(r) {
        cur_tables$atc_table[r["unique_atc_key"],
                             cols$unique_atc_key]}) + 1
      inner_terms <- inner_terms * inner_terms_atc
    }

    # multiplied with timing metric by pattern
    if (method$gamma != 0) {
      inner_terms_timing <- apply(rows, 1, function(r) {
        cur_tables$timing_table[r["unique_timing_key"],
                                cols$unique_timing_key]}) + 1
      inner_terms <- inner_terms * inner_terms_timing
    }

    # remove the added ones
    if (method$beta != 1) {
      inner_terms <- (inner_terms - 1) ^ method$beta
    } else {
      inner_terms <- (inner_terms - 1)
    }

  } else {
    inner_terms <- 1
  }



  # sum term method
  if (method$summation_method == "sum_of_minima") {

    if (calc == "triangle") {
      min_sums <- rcpp_sum_of_minima_triangle(
        length(unique(rows$unique_pattern_key)),
        rows$unique_pattern_key,
        inner_terms)
    } else if (calc == "full") {

      min_sums <- rcpp_sum_of_minima_full(
        length(unique(rows$unique_pattern_key)),
        length(unique(cols$unique_pattern_key)),
        rows$unique_pattern_key,
        cols$unique_pattern_key,
        Rfast::transpose(inner_terms))
    }

    # normalizing
    if (method["alpha"] == 0) {
      distance_matrix <- min_sums
    } else {
      distance_matrix <- normalizer * min_sums
    }


  } else if (method$summation_method == "double_sum") {

    if (calc == "triangle") {
      sum_terms <- rcpp_double_sum_triangle(
        length(unique(rows$unique_pattern_key)),
        rows$unique_pattern_key,
        inner_terms)
    } else if (calc == "full") {
      sum_terms <- rcpp_double_sum_full(
        length(unique(rows$unique_pattern_key)),
        length(unique(cols$unique_pattern_key)),
        rows$unique_pattern_key,
        cols$unique_pattern_key,
        Rfast::transpose(inner_terms))
    }

    # normalizing
    if (method["alpha"] == 0) {
      distance_matrix <- sum_terms
    } else {
      distance_matrix <- normalizer * sum_terms
    }

  } else {
    stop(paste0("'summation_method' is not recognised.",
                "\nMust be either 'double_sum' or 'sum_of_minima'."))
  }

  return(distance_matrix)
}




#' Internal method for cluster assignment
#'
#' Takes the distance matrix and applies hierarchical clustering and cuts to
#' find the chosen number of clusters. Both the cluster assignment and distance
#' matrix are returned.
#'
#' @param keys A list of data frames from `key_constructor`.
#' @param method A 1-row data frame with chosen method parameters.
#' @param k The number of clusters to be calculated
#' @param distance_matrix The distance matrix from `distance_matrix_constructor`.
#' @noRd
hierarchical_clustering <- function(keys,
                                    method,
                                    k,
                                    distance_matrix,
                                    base_clustering) { # to be remved : base_clustering


  #   ===   Potential Pre-clusters   ===========================================
  if (!is.null(keys$base_clustering)) {

    #   ---   linkage for
    chosen_linkage <- function() {
      if (method$linkage == "complete") {
        return(max)
      } else if (method$linkage == "single") {
        return(min)
      } else if (method$linkage == "average") {
        return(mean)
      } else {
        stop(paste0(
          "The chosen 'linkage' can not be used with pre-clustering.",
          "\nThe 'complete', 'single' and 'average' linkages are allowed."))
      }
    }

    members <- table(keys$base_clustering[,1])
    listed <- lapply(names(members), function(nam) {
      keys$base_clustering[keys$base_clustering[,1] == nam, 2][[1]]
    })
    cs <- length(listed)

    #   ---  create new pre-cluster
    analysis_dist <- matrix(mapply(
      FUN = function(i, j) {
        chosen_linkage()(distance_matrix[listed[[i]], listed[[j]]])
      },
      i = rep(1:cs, each = cs),
      j = rep(1:cs, times = cs)),
      cs, cs)
    diag(analysis_dist) <- 0
    dimnames(analysis_dist) <- list(names(members), names(members))
    analysis_dist <- stats::as.dist(analysis_dist)

  } else {
    analysis_dist <- stats::as.dist(distance_matrix)
    members <- NULL
  }


  #   ===   Clustering   =======================================================

  dendogram <- stats::hclust(analysis_dist, method = method$linkage, members)

  cluster_names <- paste0(method$clustering, "_k=", k)

  pattern_clusters <- data.frame(stats::cutree(dendogram, k)) %>%
    dplyr::rename_at(dplyr::vars(tidyselect::everything()), ~cluster_names)


  if (is.null(members)) {
    pattern_clusters <- pattern_clusters %>%
      tibble::rownames_to_column(var = "unique_pattern_key") %>%
      dplyr::mutate(unique_pattern_key = as.numeric(.data$unique_pattern_key))
    joiner <- "unique_pattern_key"
  } else {
    pattern_clusters <- pattern_clusters %>%
      tibble::rownames_to_column(var = names(keys$base_clustering)[1])
    joiner <-  names(keys$base_clustering)[1]
  }


  #   ===   ORGANISING RESULTS   =============================================

  cluster_assignment <- keys$key %>%
    dplyr::select(".internal_character_id",
                  "unique_pattern_key",
                  dplyr::any_of(names(keys$base_clustering)[1])) %>%
    dplyr::distinct() %>%
    dplyr::left_join(pattern_clusters, by = joiner) %>%
    dplyr::select(".internal_character_id", dplyr::all_of(cluster_names)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(cluster_names)),
                     ~factor(.,
                             levels = names(sort(rank(-table(.),
                                                      ties.method = "first"))),
                             labels = as.roman(1:length(unique(.)))))

  return(list(cluster_assignment = cluster_assignment,
              distance_matrix = distance_matrix))
}


