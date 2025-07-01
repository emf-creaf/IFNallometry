data(example_tree_data)
example_tree_data <- example_tree_data |>
   dplyr::mutate(Province = substr(ID, 1, nchar(ID)-4))
test_that("volumes can be calculated",{
  expect_s3_class(IFNvolume(example_tree_data, provinceFromID = TRUE), "data.frame")
  expect_s3_class(IFNvolume(example_tree_data, provinceFromID = FALSE), "data.frame")
  expect_s3_class(IFNvolume(example_tree_data, provinceFromID = TRUE,
                            DBHclasses = seq(0, 120, by=5)), "data.frame")
  expect_s3_class(IFNvolume(example_tree_data, provinceFromID = FALSE,
                            DBHclasses = seq(0, 120, by=5)), "data.frame")
})

test_that("volumes can be calculated from species names",{
  v <- IFNvolume(example_tree_data, provinceFromID = TRUE)
  example_tree_data2 <- example_tree_data
  example_tree_data2$Species <- v$Name
  example_tree_data2$Species[18] <- "Acer spp."
  expect_s3_class(IFNvolume(example_tree_data2, provinceFromID = TRUE), "data.frame")
  expect_s3_class(IFNvolume(example_tree_data2, provinceFromID = FALSE), "data.frame")
})

test_that("volumes can be calculated for medfate",{
  testthat::skip_if_not_installed("medfate")
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(IFNvolume_medfate(medfate::exampleforest, medfate::SpParamsMED, province = "8"), "double")
  expect_type(IFNvolume_medfate(medfate::exampleforest, medfate::SpParamsMED, province = "8", level = "stand"), "double")
})
