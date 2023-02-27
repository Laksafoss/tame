#' Employ a Clustering to New Data
#'
#' Employ a clustering to new data
#'
#' @param object A `medic` clustering object for which employment is desired.
#' @param new_data A data frame in which to look for variables with
#' @param only <[`data-masking`][dplyr::dplyr_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the variables in
#'   `object` and/or `additional_data` and specifies which clusterings should be 
#'   employed to the new data.
#' @param additional_data A data frame with additional data that may be
#'   (left-)joined onto the `parameters` in `object`. This is often 
#'   used in conjuction with `only` to select specific clusterings based on 
#'   `additional_data`.
#' @param assignment_method A character naming the employment method. The
#'   default assignment method `"nearest_cluster"` matches people in `new_data`
#'   to their nearest cluster in the chosen clusterings from `object`. As
#'   finding exact matches (the next assignment method) is contained within this
#'   strategy the `"exact_only"` matches are also reported in additional columns
#'   in the output.
#'
#'   The assignment method `"exact_only"` only matches a person from `new_data`
#'   to a cluster if they are a perfect match to anyone in `object`. Thus,
#'   people from `new_data` are not guaranteed assignment to a cluster.
#' @param parallel A logical or an integer. If `FALSE`, the default, no
#'   parallelization is done.
#'
#'   If `TRUE` or an integer larger than 2L parallelization is implemented via
#'   [parLapply][parallel::parLapply] from the \strong{parallel} package. When
#'   `parallel` is \code{TRUE} the number of [clusters][parallel::makeCluster]
#'   is set to [detectCores][parallel::detectCores] - 1, and when `parallel` is
#'   an integer then the number of [clusters][parallel::makeCluster] is set to
#'   `parallel`. For more details on the parallelization method see
#'   [parallel::parLapply].
#' @param ... Additional arguments affecting the employment procedure.
#'
#' @return
#' `employ` returns a `medic` object.
#'
#' @examples
#' part1 <- complications[1:100,]
#' part2 <- complications[101:149,]
#'
#' clust <- medic(part1, id = id, atc = atc, k = 3)
#' 
#' # Nearest cluster matching
#' employ(clust, part2)
#'
#' # Only exact matching
#' employ(clust, part2, assignment_method = "exact_only")
#'
#' @export
employ <- function(
    object,
    new_data,
    only = NULL,
    additional_data = NULL,
    assignment_method = "nearest_cluster",
    parallel = FALSE,
    ...) {


  #   ===   Input Check   ======================================================
  
  #   ---  Are all variables present   -----------------------------------------
  variable_names_needed <- unlist(object$variables)[
    which(names(unlist(object$variables)) != "base_clustering")
  ]
  if (! all(variable_names_needed %in% names(new_data))) {
    diff <- setdiff(variable_names_needed, names(new_data))
    stop(
      paste0(
        "Not all necissary variables are avaible in 'new_data'.\n",
        if (length(diff) == 1) {
          paste("The variable", diff, "is missing.")
        } else if ((length(diff) < 6)) {
          paste("The variables", diff, "are missing.")
        } else {
          paste("The variables", diff[1:5], "... are missing.")
        }
      )
    )
  }

  #   ---   Are we only considering new people   -------------------------------
  overlap <- intersect(
    object$data[[object$variables$id]],
    new_data[[object$variables$id]]
  )
  if (length(overlap) > 0) {
    if (length(overlap) > 5) {
      overlap <- c(overlap[1:5], "...")
    }
    stop(
      paste0(
        "People who have already been clustered in 'object' can de found in ",
        "'new_data':\n     ",
        paste0(overlap, collapse = ", "),
        "\n   are in both data sets."
      )
    )
  }



  #   ===   Restrictions and Preparations   ====================================

  clust <- enrich(object, additional_data)
  selected_analyses <- method_selector(clust, {{ only }})
  selected_names <- selected_analyses$cluster_name

  parameters <- clust$parameters %>%
    dplyr::filter(.data$cluster_name %in% selected_names)

  old <- clust$data %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      !!rlang::sym(clust$variables$atc),
      dplyr::all_of(clust$variables$timing),
      !!!rlang::syms(selected_names)
    ) %>%
    tidyr::nest(pattern = c(clust$variables$atc, clust$variables$timing))

  old_distinct <- old %>%
    dplyr::select(-!!rlang::sym(clust$variables$id)) %>%
    dplyr::distinct()

  new <- new_data %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      !!rlang::sym(clust$variables$atc),
      dplyr::all_of(clust$variables$timing)
    ) %>%
    tidyr::nest(pattern = c(clust$variables$atc, clust$variables$timing))

  matching <- new %>%
    dplyr::full_join(old_distinct, by = "pattern")



  #   ===   Exact Matching   ===================================================

  if (assignment_method == "exact_only") {
    
    exact_clusters <- matching %>%
      dplyr::select(-"pattern") %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(selected_names), list("new_exact" = ~.))
      )
    
    old_clusters <- clust$clustering %>%
      dplyr::select(
        !!rlang::sym(clust$variables$id),
        ".analysis_order",
        !!!rlang::syms(selected_names)
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(selected_names), list("old" = ~.))
      ) %>%
      dplyr::arrange(
        match(
          !!rlang::sym(clust$variables$id),
          clust$data[[clust$variables$id]]
        )
      )
    
    all_clusterings <- dplyr::bind_rows(exact_clusters, old_clusters) %>%
      dplyr::relocate(dplyr::all_of(clust$variables$id), ".analysis_order")
   
    final_clusters <- all_clusterings %>%
      dplyr::select(
        !!rlang::sym(clust$variables$id), 
        !!!rlang::syms(selected_names)
      )
    
    joined_data <- clust$data %>% 
      dplyr::select(-dplyr::all_of(selected_names)) %>%
      dplyr::mutate(.origin = "old") %>%
      dplyr::bind_rows(new_data) %>%
      dplyr::mutate(.origin = tidyr::replace_na(.data$.origin, "new")) %>%
      dplyr::left_join(final_clusters, by = clust$variables$id) %>%
      dplyr::relocate(dplyr::all_of(clust$variables$id))
    
    return(
      structure(
        list(
          data = joined_data,
          clustering = all_clusterings,
          variables = clust$variables,
          parameters = clust$parameters,
          key = clust$keys,
          call = list(clust$call, match.call(expand.dots = FALSE))
        ),
        class = c("medic", "list")
      )
    )
  }


  #   ===   Non-exact Matching   ===============================================

  #   ---  Split new into exact and missed   -----------------------------------
  exact <- matching %>%
    dplyr::filter(
      !is.na(!!rlang::sym(selected_names[1])),
      !is.na(!!rlang::sym(clust$variables$id))
    ) %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      !!!rlang::syms(selected_names)
    )

  missed <- matching %>%
    dplyr::filter(
      is.na(!!rlang::sym(selected_names[1])),
      !is.na(!!rlang::sym(clust$variables$id))
    ) %>%
    dplyr::select(!!rlang::sym(clust$variables$id))

  #   ---  construct the new data set for pseudo-clustering   ------------------
  missed_full_info <- new_data %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      !!rlang::sym(clust$variables$atc),
      dplyr::all_of(clust$variables$timing)
    ) %>%
    dplyr::inner_join(missed, by = clust$variables$id) %>%
    dplyr::mutate(.origin = "new")

  missed_with_old <- clust$data %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      !!rlang::sym(clust$variables$atc),
      dplyr::all_of(clust$variables$timing),
      !!!rlang::syms(selected_names)
    ) %>%
    dplyr::mutate(.origin = "old") %>%
    dplyr::bind_rows(missed_full_info) %>%
    dplyr::mutate(
      .internal_character_id = paste0(
        .data$.origin, !!rlang::sym(clust$variables$id)
      )
    )

  #   ---   key construction for pseudo-clustering   ---------------------------
  keys <- key_constructor(
    missed_with_old,
    !!rlang::sym(clust$variables$id),
    dplyr::all_of(c(selected_names, ".origin")),
    !!rlang::sym(clust$variables$atc),
    dplyr::all_of(clust$variables$timing)
  )

  old_patterns <- keys$base_clustering %>% dplyr::filter(.data$.origin == "old")
  new_patterns <- keys$base_clustering %>% dplyr::filter(.data$.origin == "new")

  lookup_tables <- lookup_constructor(keys, parameters)

  #   ---   pseudo-clustering   ------------------------------------------------

  # prep for parallel if needed
  if (parallel) {
    n_cores <- min(
      nrow(parameters),
      parallel::detectCores() - 1,
      ifelse(is.logical(parallel), Inf, parallel)
    )
    if (n_cores == 1) {
      parallel <- FALSE
    } else {
      clust <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(clust))
    }
  }

  if (parallel) {
    #   ...   parallel pseudo-clustering   .....................................

    parallel::clusterExport(
      clust,
      varlist = c(
        "keys",
        "parameters",
        "lookup_tables",
        "k",
        "context_lookup",
        "distance_matrix_constructor",
        "hierarchical_clustering"
      ),
      envir = environment()
    )


    clusterings <- parallel::parLapply(
      clust, 1:nrow(parameters), function(i) {

        # current method
        method <- parameters[i,]

        # method specific atc, timing & amount metric tables
        cur_tables <- context_lookup(method, lookup_tables)

        # create method specific distance matrix
        distance_matrix <- distance_matrix_constructor(
          keys,
          method,
          cur_tables,
          old_patterns = old_patterns %>% dplyr::pull(.data$unique_pattern_key),
          new_patterns = new_patterns %>% dplyr::pull(.data$unique_pattern_key))

        chosen_linkage <- switch (
          method[["linkage"]],
          ward     = function() stop("'ward' linkage not implmented yet."),
          ward.D   = function() stop("'ward.D' linkage not implmented yet."),
          ward.D2  = function() stop("'ward.D2' linkage not implmented yet."),
          single   = min,
          complete = max,
          average  = mean,
          mcquitty = function() stop("'mcquitty' linkage not implmented yet."),
          median   = stats::median,
          centroid = function() stop("'centroid' linkage not implmented yet."))

        new_clusters <- apply(distance_matrix, 2, function(d) {
          clust_dist <- tapply(
            d,
            old_patterns %>% dplyr::pull(method[["cluster_name"]]),
            FUN = chosen_linkage
          )
          closest <- which(clust_dist == min(clust_dist))
          if (length(closest) > 1) {
            return(names(clust_dist)[sample(closest, 1)])
          } else {
            return(names(clust_dist)[closest])
          }
        })

        new_clusters <- new_patterns %>%
          dplyr::select("unique_pattern_key") %>%
          dplyr::mutate(!!method[["cluster_name"]] := new_clusters)

        return(new_clusters)
      })
  } else {

    #   ...  serial pseudo-clustering   ........................................

    clusterings <- lapply(1:nrow(parameters), function(i) {

      # current method
      method <- parameters[i,]

      # method specific atc, timing & amount metric tables
      cur_tables <- context_lookup(method, lookup_tables)

      # create method specific distance matrix
      distance_matrix <- distance_matrix_constructor(
        keys,
        method,
        cur_tables,
        old_patterns = old_patterns %>% dplyr::pull(.data$unique_pattern_key),
        new_patterns = new_patterns %>% dplyr::pull(.data$unique_pattern_key))

      chosen_linkage <- switch (
        method[["linkage"]],
        ward     = function() stop("'ward' linkage not implmented yet."),
        ward.D   = function() stop("'ward.D' linkage not implmented yet."),
        ward.D2  = function() stop("'ward.D2' linkage not implmented yet."),
        single   = min,
        complete = max,
        average  = mean,
        mcquitty = function() stop("'mcquitty' linkage not implmented yet."),
        median   = stats::median,
        centroid = function() stop("'centroid' linkage not implmented yet."))

      new_clusters <- apply(distance_matrix, 2, function(d) {
        clust_dist <- tapply(
          d,
          old_patterns %>% dplyr::pull(method[["cluster_name"]]),
          FUN = chosen_linkage
        )
        closest <- which(clust_dist == min(clust_dist))
        if (length(closest) > 1) {
          return(names(clust_dist)[sample(closest, 1)])
        } else {
          return(names(clust_dist)[closest])
        }
      })

      new_clusters <- new_patterns %>%
        dplyr::select("unique_pattern_key") %>%
        dplyr::mutate(!!method[["cluster_name"]] := new_clusters)

      return(new_clusters)
    })
  }

  all_new_closest_clusterings <- clusterings %>%
    purrr::reduce(dplyr::left_join, by = "unique_pattern_key")

  missed_clusters <- keys$key %>%
    dplyr::filter(.data$.origin == "new") %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      "unique_pattern_key"
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(all_new_closest_clusterings, by = "unique_pattern_key") %>%
    dplyr::select(-"unique_pattern_key") %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_names),
        list("new_closest" = ~.)
      )
    )

  exact_clusters <- exact %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_names), list("new_exact" = ~.))
    )

  old_clusters <- clust$clustering %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id),
      ".analysis_order",
      !!!rlang::syms(selected_names)
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_names), list("old" = ~.))
    ) %>%
    dplyr::arrange(
      match(
        !!rlang::sym(clust$variables$id),
        clust$data[[clust$variables$id]]
      )
    )


  all_new_clusterings <- dplyr::bind_rows(missed_clusters, exact_clusters) %>%
    dplyr::arrange(
      match(
        !!rlang::sym(clust$variables$id),
        new_data[[clust$variables$id]]
      )
    )
  all_clusterings <- dplyr::bind_rows(all_new_clusterings, old_clusters) %>%
    dplyr::relocate(dplyr::all_of(clust$variables$id), ".analysis_order")
  
  
  final_clusters <- all_clusterings %>%
    dplyr::select(
      !!rlang::sym(clust$variables$id), 
      !!!rlang::syms(selected_names)
    )
  
  
  joined_data <- clust$data %>% 
    dplyr::select(-dplyr::all_of(selected_names)) %>%
    dplyr::mutate(.origin = "old") %>%
    dplyr::bind_rows(new_data) %>%
    dplyr::mutate(.origin = tidyr::replace_na(.data$.origin, "new")) %>%
    dplyr::left_join(final_clusters, by = clust$variables$id) %>%
    dplyr::relocate(dplyr::all_of(clust$variables$id))

  names(keys)[which(names(keys) == "base_clustering")] <- "clustered_patterns"

  return(
    structure(
      list(
        data = joined_data,
        clustering = all_clusterings,
        variables = clust$variables,
        parameters = clust$parameters,
        key = keys,
        call = list(clust$call, match.call(expand.dots = FALSE))
      ),
      class = c("medic", "list")
    )
  )
}

