

test_that("recode_clustering", {
  expect_s3_class(
    recode_clustering(
      medic(
        tiny_example_data,
        k = 4,
        id = id,
        atc = atc
      ),
      `cluster_1_k=4` = dplyr::recode(`cluster_1_k=4`, IV = "III")
    ),
    "medic"
  )
})
