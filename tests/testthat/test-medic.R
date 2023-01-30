


test_that("medic basic input", {
  expect_s3_class(
    medic(
      tiny_example_data,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester
    ),
    "medic"
  )
})

test_that("medic with all valid inputs", {
  expect_s3_class(
    medic(
      tiny_example_data,
      k = 3:5,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester,
      base_clustering = id,
      summation_method = c("double_sum", "sum_of_minima"),
      linkage = c("complete", "average"),
      alpha = c(1.2, 1),
      beta = c(1.2, 1),
      gamma = c(1.2, 1),
      p = c(1, 2),
      theta = list((5:0)/5, (5:0)/4),
      parallel = TRUE,
      return_distance_matrix = TRUE,
      set_seed = 1
    ),
    "medic"
  )
})

