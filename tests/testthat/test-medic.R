


test_that("medic basic input", {
  expect_s3_class(
    medic(
      complications,
      id = id,
      atc = atc
    ),
    "medic"
  )
  expect_s3_class(
    medic(
      complications,
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
      complications,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester,
      alpha = 1.1,
      beta = 0.5,
      gamma = 0.2,
      p = 2,
      theta = (5:0) / 5 + 0.1,
      k = 4,
      linkage = "complete",
      summation_method = "sum_of_minima"
    ),
    "medic"
  )
  expect_s3_class(
    medic(
      complications,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester,
      alpha = 1.1,
      beta = 0.5,
      gamma = 0.2,
      p = 2,
      theta = (5:0) / 5 + 0.1,
      k = 4:5,
      linkage = "complete",
      summation_method = "sum_of_minima"
    ),
    "medic"
  )
  expect_s3_class(
    medic(
      complications,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester,
      summation_method = c("sum_of_minima", "double_sum")
    ),
    "medic"
  )
  expect_s3_class(
    medic(
      complications,
      id = id,
      atc = atc,
      timing = first_trimester:third_trimester,
      linkage = c(
        "complete", 
        "average", 
        "single", 
        "Ward", 
        "Ward.D2", 
        "mcquitty", 
        "centroid", 
        "median"
      )
    ),
    "medic"
  )
})
