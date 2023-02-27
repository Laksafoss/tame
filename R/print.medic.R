#' @describeIn medic Print method for medic-objects
#' @param x A `medic` object for printing.
print.medic <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Parameters:\n")
  x$parameters %>% 
    dplyr::select(-"clustering", -"theta_list") %>% 
    print.data.frame(max = 10 * (ncol(x$parameters) - 1))
  cat("\n\nClusterings:\n")
  x$clustering %>%
    dplyr::select(-dplyr::any_of(".analysis_order")) %>%
    print(max = 10 * (ncol(x$clustering) - 1))
  cat("\n")
  invisible(x)
}
