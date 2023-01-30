

expect_medic <- function(object) {

  #   0   Capture objects and label   ==========================================
  act <- quasi_label(rlang::enquo(object), arg = "object")
  act$class <- class(act$val)

  #   1   Class   ==============================================================
  expect(
    inherits(act$val, "medic"),
    sprintf("%s inherits from %s not %s.", act$lab, act$class, "medic")
  )


  #   2   Correct slots   ======================================================
  required <- c(
    "data",
    "variables",
    "clustering",
    "clustering_parameters",
    "key"
  )
  if (is.null(names(act$val))) {
    fail(sprintf("%s does not have any names", act$lab))
  } else {
    expect(
      all(required %in% names(act$val)),
      sprintf("%s i missing %s.", act$lab, setdiff(required, names(act$val)))
    )
  }


  #   3   Data   ===============================================================
  expect(
    inherits(act$val$data, "data.frame"),
    sprintf("%s$data is not a data.frame.", act$lab)
  )
  # TODO MAKE MORE REQUREMENTS   <-<-<--<-<-<-<-<-<-<--<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-


  #   4   Variables   ==========================================================
  expect(
    inherits(act$val$variables, "list"),
    sprintf("%s$variables is not a list.", act$lab)
  )
  # TODO MAKE MORE REQUREMENTS   <-<-<--<-<-<-<-<-<-<--<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-



  #   5   Clustering   =========================================================
  expect(
    inherits(act$val$clustering, "data.frame"),
    sprintf("%s$clustering is not a data.frame.", act$lab)
  )
  # TODO MAKE MORE REQUREMENTS   <-<-<--<-<-<-<-<-<-<--<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-


  #   6   Clustering Parameters   ==============================================
  expect(
    inherits(act$val$clustering, "data.frame"),
    sprintf("%s$clustering is not a data.frame.", act$lab)
  )
  # TODO MAKE MORE REQUREMENTS   <-<-<--<-<-<-<-<-<-<--<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-



  #   7   All Keys   ===========================================================
  required <- c(
    "key",
    "reduced_key",
    "unique_exposure",
    "unique_patterns",
    "clustered_patterns"
  )
  if (is.null(names(act$val$key))) {
    fail(sprintf("%s does not have any names in key", act$lab))
  } else {
    expect(
      all(required %in% names(act$val$key)),
      sprintf(
        "The following names are missing from %s$key:\n %s.",
        act$lab,
        setdiff(required, names(act$val$key))
      )
    )

    expect(
      any(c("unique_atc", "unique_timing") %in% names(act$val$key)),
      sprintf(
        "Either 'unique_atc' or 'unique_timing' must be present in %s$key.",
        act$lab
      )
    )

  }

  #   8   Correct Keys   =======================================================

  #   8.1   key   --------------------------------------------------------------
  # should we require the cluster assignments here !?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!?!
  required <- c(
    ".internal_character_id",
    "unique_pattern_key",
    "n_unique_exposures"
  )
  expect(
    all(required %in% names(act$val$key$key)),
    sprintf(
      "The following names are missing from %s$key$key:\n%s.",
      act$lab,
      setdiff(required, names(act$val$key$key))
    )
  )

  #   8.2   reduced key   ------------------------------------------------------
  required <- c(
    "unique_atc_key",
    "unique_exposure_key",
    "unique_pattern_key",
    "n_unique_exposures"
    )
  expect(
    all(required %in% names(act$val$key$reduced_key)),
    sprintf(
      "The following names are missing from %s$key$reduced_key:\n%s.",
      act$lab,
      setdiff(required, names(act$val$key$reduced_key))
    )
  )

  #   8.3   unique atc key   ---------------------------------------------------
  if (!is.null(act$val$key$unique_atc)) {
    expect(
      ncol(act$val$key$unique_atc) == 2,
      sprintf(
        "%s should have exactly 2 columns, it has %s.",
        act$lab,
        ncol(act$val$key$unique_atc)
      )
    )
    expect(
      "unique_atc_key" %in% names(act$val$key$unique_atc),
      sprintf(
        "unique_atc_key is missing from %s$key$unique_atc.",
        act$lab
      )
    )
  }


  #   8.4   unique timing key   ------------------------------------------------
  if (!is.null(act$val$key$unique_timing)) {
    expect(
      ncol(act$val$key$unique_timing) >= 2,
      sprintf(
        "%s should have at least 2 columns, it has %s.",
        act$lab,
        ncol(act$val$key$unique_timing)
      )
    )
    expect(
      "unique_timing_key" %in% names(act$val$key$unique_timing),
      sprintf(
        "unique_timing_key is missing from %s$key$unique_timing",
        act$lab
      )
    )
  }

  #   8.5   unique exposure key   ----------------------------------------------
  expect(
    ncol(act$val$key$unique_exposure) >= 2,
    sprintf(
      "%s should have at least 2 columns, it has %s.",
      act$lab,
      ncol(act$val$key$unique_exposure)
    )
  )
  expect(
    "unique_exposure_key" %in% names(act$val$key$unique_exposure),
    sprintf(
      "unique_exposure_key is missing from %s$key$unique_exposure",
      act$lab
    )
  )

  expect(
    any(c("unique_atc_key", "unique_timing_key") %in%
          names(act$val$key$unique_exposure)),
    sprintf(
      "Either %s must be present in %s$key$unique_exposure.",
      "'unique_atc_key' or 'unique_timing_key'",
      act$lab
    )
  )


  #   8.6   unique patterns key   ----------------------------------------------
  required <- c("unique_pattern_key", "n_unique_exposures", "pattern")
  expect(
    ncol(act$val$key$unique_patterns) == 3,
    sprintf(
      "%s should have exactly 3 columns, it has %s.",
      paste0(act$lab, "$key$unique_patterns"),
      ncol(act$val$key$unique_patterns)
    )
  )
  expect(
    all(required %in% names(act$val$key$unique_patterns)),
    sprintf(
      "%s is missing from %s$key$unique_patterns",
      required,
      act$lab
    )
  )

  #   9   Return invisible   ===================================================
  invisible(act$val)
}
