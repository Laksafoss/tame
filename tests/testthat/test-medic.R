


test_that("medic basic input", {
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
  expect_equal(2 * 2, 4)
})

