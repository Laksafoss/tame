#' Test if an object is a medic-object
#'
#' @param object Any object.
#'
#' @return
#' `TRUE` is the object inherits from the `medic` class and has the required
#' elements.
#'
#' @examples
#' clust <- medic(tiny_example_data, id = id, atc = atc, k = 3)
#' is.medic(clust)
#'
#' @export
is.medic <- function(object) {

  #   1   Class   ==============================================================
  if (!inherits(object, "medic")) { return(FALSE) }

  #   2   Slot Names   =========================================================
  if (is.null(names(object))) { return(FALSE) }
  required <- c(
    "data",
    "variables",
    "clustering",
    "clustering_parameters",
    "key"
  )
  if (! all(required %in% names(object))) { return(FALSE) }

  #   3   Data Slot   ==========================================================
  if (!inherits(object$data, "data.frame")) { return(FALSE) }
  # TODO: check if variables are in data ?  Or should that be done below ?

  #   4   Variables Slot   =====================================================
  if (!inherits(object$variables, "list")) { return(FALSE) }
  if (is.null(names(object$variables))) { return(FALSE) }
  possible <- c("id", "atc", "timing", "base_clustering")
  if (! all(names(object$variables)) %in% possible) { return(FALSE) }

  #   5   Clustering Slot   ====================================================
  if (!inherits(object$clustering, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  #   6   Parameters Slot   ====================================================
  if (!inherits(object$clustering_parameters, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  #   7   Key Slot   ===========================================================
  if (!inherits(object$key, "list")) { return(FALSE) }

  ##   7.1   Key Slot Names   --------------------------------------------------
  if (is.null(names(object$key))) { return(FALSE) }
  required <- c(
    "key",
    "reduced_key",
    "unique_exposure",
    "unique_patterns",
    "clustered_patterns"
  )
  if (! all(required %in% names(object$key))) { return(FALSE) }

  ##   7.2   Key Slot : Key slot   ---------------------------------------------
  if (!inherets(object$key$key, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  ##   7.3   Key Slot : Reduced slot   -----------------------------------------
  if (!inherets(object$key$reduced_key, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  ##   7.4   Key Slot : Exposure slot   ----------------------------------------
  if (!inherets(object$key$unique_exposure, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  ##   7.5   Key Slot : Pattern slot   -----------------------------------------
  if (!inherets(object$key$unique_patterns, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  ##   7.6   Key Slot : Clustered slot   ---------------------------------------
  if (!inherets(object$key$clustered_patterns, "data.frame")) { return(FALSE) }
  # TODO : Check Columns

  return(TRUE)
}
