#' Medication clustering (based on ATC and timing)
#'
#' The \code{medic} method uses agglomerative hierarchical clustering with a 
#' bespoke distance measure based on medication ATC codes similarities, 
#' medication timing and medication amount or dosage.
#'
#' @param data A data frame containing all the variables for the clustering.
#' @param k a vector specifying the number of clusters to identify.
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select]> An unquoted
#'   expression naming the variable in \code{data} describing person id.
#' @param atc <[`tidy-select`][dplyr::dplyr_tidy_select]> An unquoted
#'   expression naming the variable in \code{data} containing ATC codes.
#' @param timing <[`tidy-select`][dplyr::dplyr_tidy_select]> An unquoted
#'   expression naming the variable or variables in \code{data} describing
#'   medication timing. Variable names can be used as if they were positions in
#'   the data frame, so expressions like x:y can be used to select a range of
#'   variables. Moreover, pattern matching selection helpers such as
#'   [`starts_with`][tidyselect::starts_with] or
#'   [`num_range`][tidyselect::starts_with] may also be used to select timing
#'   variables.
#' @param base_clustering <[`tidy-select`][dplyr::dplyr_tidy_select]> An 
#'   unquoted expression naming the variable in `data` that gives an initial
#'   clustering to start the `medic` from or `NULL`. 
#' @param linkage The agglomeration method to be used in the clustering. This
#'   should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2",
#'   "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median"
#'   (= WPGMC) or "centroid" (= UPGMC). See [stats::hclust] for more
#'   information. For a discussion of linkage criterion choice see
#'   \emph{detailes} below.
#' @param summation_method The summation method used in the distance measure.
#'    This  should be either "double_sum" or "sum_of_minima". See
#'   \emph{detailes} below for more information.
#' @param alpha A number giving the tuning of the normalization. See
#'   \emph{detailes} below for more information.
#' @param beta A number giving the power of the individual medication
#'   combinations. See \emph{detailes} below for more information.
#' @param gamma A number giving the weight of the timing terms. See
#'   \emph{detailes} below for more information.
#' @param p The power of the Minkowski distance used in the timing-specific
#'    distance. See \emph{detailes} below for more information.
#' @param theta A vector of length 6 specifying the tuning of the ATC measure.
#'   See \emph{detailes} below for more information.
#' @param parallel A logical or an integer. If `FALSE`, the default, no
#'   parallelization is done.
#'
#'   If `TRUE` or an integer larger than 2L parallelization is implemented via
#'   [parLapply][parallel::parLapply] from the \strong{parallel} package. When
#'   `parallel` is \code{TRUE} the number of [clusters][parallel::makeCluster]
#'   is set to [detectCores][parallel::detectCores] - 1, and when `parallel` is
#'   an integer then the number of [clusters][parallel::makeCluster] is set to
#'   `parallel`. For more detailes on the parallization method see
#'   [parallel::parLapply].
#' @param return_distance_matrix A logical.
#' @param set_seed A logical or an integer.
#' @param ... Additional arguments not currently in use.
#'
#' @details
#' The \code{medic} method uses agglomerative hierarchical
#' clustering with a bespoke distance measure based on medication ATC codes and
#' timing similarities to assign medication pattern clusters to people.
#'
#' Two versions of the distance measure are available: 
#' 
#' The \emph{double sum}:
#'
#' \deqn{%
#'   d(p_i, p_j) = N_{\alpha}(M_i \times M_j) \sum_{m\in M_i}\sum_{n \in M_j}%
#'   ((1 + D_{\theta}(m,n)) (1 + \gamma T_p(t_{im},t_{jn})) - 1)^{\beta}.%
#' }{%
#'   d(p_i, p_j) = N_\alpha(M_i  M_j) \sum{m in M_i}\sum{n in M_j}%
#'   ((1 + D_\theta(m,n)) (1+ \gamma T_p(t_{im},t_{jn})) - 1)^\beta.%
#' }
#'
#' and the \emph{sum of minima}:
#' \deqn{%
#'   d(p_i, p_j) = \frac{1}{2}(N_{\alpha}(M_i)\sum_{m\in M_i}\min_{n \in M_j}%
#'   ((1 + D_{\theta}(m,n)) (1 + \gamma T_p(t_{im},t_{jn})) - 1)^{\beta} +
#'   N_{\alpha}(M_j) \sum_{n\in M_j}\min_{m \in M_i}%
#'   ((1 + D_{\theta}(m,n)) (1 + \gamma T_p(t_{im},t_{jn})) - 1)^{\beta}).%
#' }{%
#'   d(p_i, p_j) = (1/2) *( N_\alpha(M_i)\sum{m in M_i}\min{n in M_j}%
#'   ((1 + D_\theta(m,n)) (1+ \gamma T_p(t_{im},t_{jn})) - 1)^\beta +
#'    (N_\alpha(M_j)\sum{n in M_j}\min{m in M_i}%
#'   ((1 + D_\theta(m,n)) (1+ \gamma T_p(t_{im},t_{jn})) - 1)^\beta).%
#' }
#'
#'
#' ## Normalization
#' \deqn{%
#'   N_{\alpha}(x) = |x|^{-\alpha}%
#' }{%
#'   N_\alpha(x) = |x|^-\alpha %
#' }
#'
#' If the normalization tuning, \code{alpha}, is 0, then no normalization is
#' preformed and the distance measure becomes highly dependent on the number of
#' distinct medications given. That is, people using more medication will have
#' larger distances to others. If the normalization tuning, \code{alpha}, is 1 -
#' the default - then the summation is normalized with the number of terms in
#' the sum, in other words, the average is calculated.
#'
#' ## ATC distance
#' The central idea of this method, namely the ATC distance, is given as
#' \deqn{%
#'   D_{\theta}(x, y) = \sum_{i=1,...,5}1\{x and y match on level i, but not level i + 1\}\theta_i%
#' }{%
#'   D_\theta(x, y) = \sum_{i=1,...,5}1\{x and y match on level i, but not level i + 1\}\theta_i%
#' }
#' The ATC distance is tuned using the vector \code{theta}. 
#' 
#' Note that two ATC codes are said to match at level i when they are identical 
#' at level i. E.g. the two codes N06AB01 and N06AA01 match on level 1, 2, and 3 
#' as they are both "N" at level 1, "N06" at level 2, and "N06A" at level 3, 
#' but at level 4 they differ ("N06AB" and "N06AA" are not the same). 
#'
#' ## Timing distance
#' The timing distance is a simple Minkowski distance:
#' \deqn{%
#'   T(x,y) =(\sum_{t \in T} |x_t - y_t|^p)^{1/p}.%
#' }{%
#'   T(x,y) =(\sum_{t in T} |x_t - y_t|^p)^{1/p}.%
#' }
#' When `p` is 1, the default, the Manhattan distance is used. 
#'
#'
#' @return
#' An object of class \emph{medic} which describes the clusters produced
#' the hierarchical clustering process. The object is a list with components:
#' \describe{
#'   \item{data}{the inputted data frame \code{data} with the cluster
#'      assignments appended at the end.}
#'   \item{clustering}{a data frame with the person id as given by \code{id}, 
#'      the `.analysis_order` and the clusters found.}
#'   \item{variables}{a list of the variables used in the clustering.}
#'   \item{parameters}{a data frame with all the inputted clustering
#'      parameters and the corresponding method names. These method names
#'      correspond to the column names for each cluster in the \code{clustering}
#'      data frame described right above.}
#'   \item{key}{a list of keys used internally in the function to keep track of
#'      simplified versions of the data.}
#'   \item{distance_matrix}{the distance matrices for each method if 
#'      `return_distance_matrix` is `TRUE` otherwise `NULL`.}
#'   \item{call}{the matched call.}
#' }
#'
#'
#' @seealso
#' [summary.medic] for summaries and plots.
#' 
#' [employ] for employing an existing clustering to new data.
#' 
#' [enrich] for enriching the meta data in the `medic` object with additional 
#' data. 
#' 
#' [bind] for binding together two comparable lists of clusterings.
#'
#'
#' @examples
#' # A simple clustering based only on ATC
#' clust <- medic(complications, id = id, atc = atc, k = 3)
#'
#' # A simple clustering with both ATC and timing
#' clust <- medic(
#'   complications,
#'   id = id,
#'   atc = atc,
#'   timing = first_trimester:third_trimester,
#'   k = 3
#' )
#'
#'
#' @export
medic <- function(
    data,
    k = 5,
    id,
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
    parallel = FALSE,
    return_distance_matrix = FALSE,
    set_seed = FALSE,
    ...) {


  #   ===   Preparations   =====================================================


  # checking input
  input_check <- match.call(expand.dots = TRUE)
  input_check[[1]] <- quote(tame::parameters_constructor)
  parameters <- eval(input_check, parent.frame())

  input_variables <- list(
    "id" = names(dplyr::select(data, {{ id }})),
    "atc" = names(dplyr::select(data, {{ atc }})),
    "timing" = names(dplyr::select(data, {{ timing }})),
    "base_clustering" = names(dplyr::select(data, {{ base_clustering }})))

  # create character id key - saves us some pain when naming
  if (is.numeric(set_seed)) { set.seed(set_seed) }
  data <- data %>%
    dplyr::mutate(.original_order = dplyr::row_number(),
                  .analysis_order = sample(1:(dplyr::n()),
                                           size = dplyr::n(),
                                           replace = FALSE)) %>%
    dplyr::arrange(.data$.analysis_order) %>%
    dplyr::mutate(.internal_character_id = as.character({{ id }}))

  # make keys
  keys <- key_constructor(data,
                          {{ id }},
                          {{ base_clustering }},  # preclustering should be on unique pattern !!!!
                          {{ atc }},
                          {{ timing }})



  # Calculate comparisons for look-up tables
  # these look-up tables are valid for all clustering options
  lookup_tables <- lookup_constructor(keys, parameters)


  #   ===   Clustering   =======================================================

  # prep for parallel if needed
  if (parallel) {
    n_cores <- min(nrow(parameters),
                   parallel::detectCores() - 1,
                   ifelse(is.logical(parallel), Inf, parallel))
    if (n_cores == 1) {
      parallel <- FALSE
    } else {
      clust <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(clust))
    }
  }


  if (parallel) {
    #   ---   parallel clustering   --------------------------------------------

    parallel::clusterExport(clust,
                            varlist = c("keys",
                                        "parameters",
                                        "lookup_tables",
                                        "k",
                                        "context_lookup",
                                        "distance_matrix_constructor",
                                        "hierarchical_clustering"),
                            envir = environment())


    clusterings <- parallel::parLapply(
      clust, 1:nrow(parameters), function(i) {

        # current method
        method <- parameters[i,]

        # method specific atc, timing & amount metric tables
        cur_tables <- context_lookup(method, lookup_tables)

        # create method specific distance matrix
        distance_matrix <- distance_matrix_constructor(keys, method, cur_tables)

        # hierarchical clustering
        clusted <- hierarchical_clustering(keys,
                                           method,
                                           k,
                                           distance_matrix,
                                           {{ base_clustering }})
        cluster_assignment <- clusted$cluster_assignment

        # output
        if (return_distance_matrix) {
          return(list(cluster_assignment = cluster_assignment,
                      distance_matrix = distance_matrix))
        } else {
          return(list(cluster_assignment = cluster_assignment))
        }
      })


  } else {
    #   ---   serial clustering   ----------------------------------------------

    clusterings <- lapply(1:nrow(parameters), function(i) {

      # current method
      method <- parameters[i,]

      # method specific atc, timing & amount metric tables
      cur_tables <- context_lookup(method, lookup_tables)

      # create method specific distance matrix
      distance_matrix <- distance_matrix_constructor(keys, method, cur_tables)

      # hierarchical clustering
      clusted <- hierarchical_clustering(keys, method, k, distance_matrix)
      cluster_assignment <- clusted$cluster_assignment
      distance_matrix <- clusted$distance_matrix

      # output
      # output
      if (return_distance_matrix) {
        return(list(cluster_assignment = cluster_assignment,
                    distance_matrix = distance_matrix))
      } else {
        return(list(cluster_assignment = cluster_assignment))
      }
    })
  }


  #   ===   Reformatting Results   =============================================

  # The output of the clustering loop is by clustering method.
  # We would like it to be by out_data and

  # clustering assignments
  cluster_assignment <- purrr::reduce(
    lapply(clusterings, function(d) d$cluster_assignment),
    dplyr::left_join,  by = ".internal_character_id")

  # for nice output data
  cluster_data <- data %>%
    dplyr::arrange(.data$.original_order) %>%
    dplyr::select({{ id }}, ".analysis_order", ".internal_character_id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(cluster_assignment, by = ".internal_character_id") %>%
    dplyr::select(-".internal_character_id")

  # for nice output data
  out_data <- data %>%
    dplyr::arrange(.data$.original_order) %>%
    dplyr::left_join(cluster_assignment, by = ".internal_character_id") %>%
    dplyr::select(-".internal_character_id", -".original_order")

  distance_matrix <- lapply(clusterings, function(d) d$distance_matrix)

  expanded_options <- parameters %>%
    dplyr::cross_join(data.frame(k = k)) %>%
    dplyr::mutate(cluster_name = paste0(.data$clustering, "_k=", .data$k)) %>%
    dplyr::relocate("cluster_name")



  #   ===   Return Results   ===================================================
  
  return(
    structure(
      list(
        data = out_data %>% dplyr::select(-".analysis_order"),
        clustering = cluster_data,
        variables = input_variables,
        parameters = expanded_options,
        key = keys,
        distance_matrix = distance_matrix,
        call = match.call(expand.dots = FALSE)
      ),
      "class" = "medic")
  )
}

