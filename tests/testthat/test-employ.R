
test_that("employ", {
  expect_medic(
    employ(
      medic(
        tiny_example_data[1:100,], k = 4, id = id, atc = atc
      ),
      tiny_example_data[101:149,]
    )
  )
})
