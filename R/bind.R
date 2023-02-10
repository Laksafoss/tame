#' Binding multiple clustering objects into one
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Cluster `medic` objects to 
#'    combine. These `medic` objects should come from the same data set, as they
#'    are otherwise uncomparable. 
#'
#' @return
#' A `medic` object with all the
#'
#' @examples
#' # Create two different clusterings
#' clust1 <- medic(
#'   tiny_example_data[1:100,],
#'   id = id,
#'   atc = atc,
#'   k = 3
#' )
#' clust2 <- medic(
#'   tiny_example_data[101:149,],
#'   id = id,
#'   atc = atc,
#'   k = 3
#' )
#'
#' # Bind the two clustering objects into one
#' bind(clust1, clust2)
#'
#' @export
bind <- function(...) {

  clusters <- rlang::dots_list(..., .named = TRUE)
  
  cat("Sorry to inform you that this function has been\n")
  cat("delibaratly released before it is done.")
  cat("It is not yet functional")
  return(1)

  #   ===   Input Check   ======================================================

  if (length(clusters) == 1 && rlang::is_bare_list(clusters[[1]])) {
    clusters <- clusters[[1]]
    if (is.null(names(clusters))) {
      names(clusters) <- as.character(seq_along(clusters))
    }
  } 
  clusters <- purrr::discard(clusters, is.null)


  # identical variabels names
  identical_info <- all(
    sapply(clusters, function(clust) {
      c(
        #is.medic(clust), # to be uncommented as soon as is.medic works
        identical(clust$variables, clusters[[1]]$variables)
      )
    })
  )

  if (!identical_info) {
    stop("'bind' requires that the custerings are based on the same variables.")
  }


  renamers <- lapply(seq_along(clusters), function(i) {
    nam <- clusters[[i]]$data |>
      dplyr::select(".analysis_order", dplyr::matches("cluster_\\d+_k=\\d+")) |>
      names()
    names(nam) <- nam
    nam <- paste0(names(clusters)[i], nam)
  })

  return(renamers)

  shared <- lapply(clusters, function(cl) names(cl$data)) |>
    purrr::reduce()


  out <- list(
    data = lapply(clusters, function(cl) cl$data) |> # ændre navne på .analysis_order og cluster_xxx
      dplyr::recode(dplyr::full_join, by = shared), # find shared
    variables = clusters[[1]]$variables,
    clustering = 1,
    parameters = 1,
    key = 1,
    distance_matrix = NULL,
    call = lapply(clusters, function(cl) cl$call)
  )

  out <- clusters[[1]]


  return(
    structure(
      out,
      class = "medic"
    )
  )
}
