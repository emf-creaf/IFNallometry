data(example_tree_data)
test_that("biomass can be calculated",{
  expect_s3_class(IFNbiomass(example_tree_data, as.CO2 = TRUE), "data.frame")
  expect_s3_class(IFNbiomass(example_tree_data, as.CO2 = FALSE), "data.frame")
  expect_s3_class(IFNbiomass(example_tree_data, as.CO2 = TRUE,
                             DBHclasses = seq(0, 120, by=5)), "data.frame")
  expect_s3_class(IFNbiomass(example_tree_data, as.CO2 = FALSE,
                             DBHclasses = seq(0, 120, by=5)), "data.frame")
})

test_that("biomass can be calculated from species names",{
  v <- IFNbiomass(example_tree_data)
  example_tree_data2 <- example_tree_data
  example_tree_data2$Species <- v$Name
  example_tree_data2$Species[18] <- "Acer spp."
  expect_s3_class(IFNbiomass(example_tree_data2), "data.frame")
  expect_s3_class(IFNbiomass(example_tree_data2), "data.frame")
})
test_that("biomass can be calculated for medfate",{
  testthat::skip_if_not_installed("medfate")
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED), "double")
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED, fraction = "above"), "double")
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED, fraction = "below"), "double")
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED, level = "stand"), "double")
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED, level = "stand", fraction = "above"), "double")
  expect_type(IFNbiomass_medfate(medfate::exampleforest, medfate::SpParamsMED, level = "stand", fraction = "below"), "double")
})
