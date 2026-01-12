

# This example should always produce these three clusters if the medic function is working
# remotly correctly -- it's basically a degenerated example
df <- data.frame(
  id = c(1, 1, 2, 2, 3, 4, 5, 5),
  atc = c("N06AB03", "N05AH01", "N06AB03", "N05AH02", "A10BA02", "A10BA02", "J02AC01", "J02AC02"),
  t1 = c(1, 2, 1, 2, 10, 10, 0, 0),
  t2 = c(3, 4, 3, 4, 12, 12, 0, 0),
  expected_cluster = c("A", "A", "A", "A", "B", "B", "C", "C")
)

df2 <- data.frame(
  id = c(6, 6, 7, 8, 8),
  atc = c("N06AB03", "N05AH01", "A10BA02", "J02AC01", "J02AC02"),
  t1 = c(1, 2, 10, 0, 0),
  t2 = c(3, 4, 12, 0, 0),
  expected_cluster = c("A", "A", "B", "C", "C")
)

res <- medic(
  df,
  id = id,
  atc = atc,
  timing = t1:t2,
  k = 3
)
res2 <- employ(res, new_data = df2)

# Test that clusters correspond to expected clusters
test_that("medic clustering correctness", {
  expect_equal(
    nrow(dplyr::distinct(res$data, expected_cluster, `cluster_1_k=3`)),
    length(unique(df$expected_cluster))
  )
  expect_equal(
    nrow(dplyr::distinct(res2$data, expected_cluster, `cluster_1_k=3`)),
    length(unique(df$expected_cluster))
  )
})