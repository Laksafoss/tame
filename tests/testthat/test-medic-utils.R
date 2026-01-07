

# A few prerequisites for the tests ---------------------------------------
a <- 1
b <- 1
g <- 0
p <- 1
l <- "complete"
s <- "sum_of_minima"
t <- (5:0) / 5
params <- parameters_constructor(
  data = complications,
  k = 3,
  id = id,
  atc = atc,
  linkage = l,
  summation_method = s,
  alpha = a,
  beta = b,
  gamma = g,
  p = p,
  theta = t
)

faux_complications <- complications |>
  dplyr::mutate(.internal_character_id = as.character(id))



# Parameter constructor -------------------------------------------------
keys <- key_constructor(
  data = faux_complications,
  id = id,
  base_clustering = id,
  atc = atc,
  timing = first_trimester:third_trimester
)
test_that("parameters_constructor", {
  df <- dplyr::tibble(
    clustering = "cluster_1",
    linkage = l,
    summation_method = s,
    alpha = a,
    beta = b,
    gamma = g,
    p = p,
    theta_list = list(t),
    theta = as.character(list(t))
  )
  expect_equal(params, df)
  expect_equal(
    nrow(
      parameters_constructor(
        data = complications,
        k = 3,
        id = id,
        atc = atc,
        alpha = 1:3
      )
    ),
    3L
  )
  expect_error(
    parameters_constructor(
      data = complications,
      k = 3,
      id = id,
      atc = atc,
      summation_method = c("double_sum", "sum_of_minima"),
      alpha = 1:3
    )
  )
})

test_that("key_constructor", {
  expect_type(keys, "list")
  expect_length(keys, 7)
  expect_equal(
    names(keys),
    c(
      "key",
      "reduced_key",
      "unique_atc",
      "unique_timing",
      "unique_exposure",
      "unique_patterns",
      "base_clustering"
    )
  )
  expect_all_true(
    vapply(keys, \(x) is.data.frame(x), logical(1))
  )
})

# Lookup constructor -----------------------------------------------------
looks <- lookup_constructor(keys, params)
test_that("lookup_constructor", {
  expect_type(looks, "list")
  expect_length(looks, 3)
  expect_equal(
    names(looks),
    c(
      "atc_lookup_table",
      "normalizing_factor",
      "timing_lookup_table"
    )
  )
})

# ATC metric lookup -----------------------------------------------------
test_that("atc_metric_lookup_constructor", {
  expect_type(
    atc_metric_lookup_constructor(keys$unique_atc),
    "double"
  )
  expect_equal(
    diff(dim(atc_metric_lookup_constructor(keys$unique_atc))),
    0L
  )
  expect_equal(
    nrow(atc_metric_lookup_constructor(keys$unique_atc)),
    nrow(keys$unique_atc)
  )

  
  tmp <- atc_metric_lookup_constructor(
    data.frame(
      unique_atc_key = 1:6, 
      atc = c("N06AB03", "N06AB02", "N06AA03", "N06BA03", "N05AB03", "A06AB03")
    )
  )
  expect_equal(nrow(tmp), 6L)
  expect_equal(ncol(tmp), 6L)
  expect_equal(
    tmp,
    matrix(
      c(
        6, 5, 4, 3, 2, 1, 
        5, 6, 4, 3, 2, 1, 
        4, 4, 6, 3, 2, 1, 
        3, 3, 3, 6, 2, 1, 
        2, 2, 2, 2, 6, 1, 
        1, 1, 1, 1, 1, 6
      ),
      nrow = 6,
      dimnames = list(1:6, 1:6)
    )
  )
})

# Normalizing factor lookup -----------------------------------------------------
test_that("normalizing_lookup_constructor", {
  expect_type(normalizing_lookup_constructor(keys$unique_patterns), "list")
  tmp <- normalizing_lookup_constructor(
    keys$unique_patterns, 
    c("double_sum", "sum_of_minima")
  )
  expect_type(tmp, "list")
  expect_type(tmp[[1]], "double")
  expect_length(tmp, 2)
})

# Timing metric lookup ----------------------------------------------------------
test_that("timing_metric_lookup_constructor", {
  expect_type(timing_metric_lookup_constructor(keys$unique_timing), "list")
  tmp <- timing_metric_lookup_constructor(keys$unique_timing, ps = 1:2)
  expect_type(tmp, "list")
  expect_length(tmp, 2)
  expect_equal(names(tmp), c("1", "2"))
  expect_type(tmp[[1]], "double")
})

# Context lookup   --------------------------------------------------------------
cont <- context_lookup(params, looks)
test_that("context_lookup", {
  expect_type(cont, "list")
  expect_equal(names(cont), c("normalizing_factor", "atc_table", "timing_table"))
})

# Distance matrix constructor ---------------------------------------------------
dist <- distance_matrix_constructor(keys, params, cont)
test_that("distance_matrix_constructor", {
  expect_type(dist, "double")
  expect_equal(nrow(dist), nrow(keys$unique_patterns))
})

# hierarchical clustering -------------------------------------------------------
hierarchical_clustering(keys, params, k = 3, dist)
test_that("hierarchical_clustering", {
  expect_equal(2 * 2, 4)
})

# is.medic ----------------------------------------------------------------------
test_that("is.medic", {
  expect_equal(is.medic(data.frame()), FALSE)
  expect_equal(is.medic(structure(data.frame(), class = "medic")), FALSE)
  clust <- medic(complications, id = id, atc = atc, k = 3)
  expect_equal(is.medic(clust), TRUE)
})
