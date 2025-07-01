data(exampleTreeData)
exampleTreeData <- exampleTreeData |>
   dplyr::mutate(Province = substr(ID, 1, nchar(ID)-4))
test_that("volumes can be calculated",{
  expect_s3_class(IFNvolume(exampleTreeData, provinceFromID = TRUE), "data.frame")
  expect_s3_class(IFNvolume(exampleTreeData, provinceFromID = FALSE), "data.frame")
  expect_s3_class(IFNvolume(exampleTreeData, provinceFromID = TRUE,
                            DBHclasses = seq(0, 120, by=5)), "data.frame")
  expect_s3_class(IFNvolume(exampleTreeData, provinceFromID = FALSE,
                            DBHclasses = seq(0, 120, by=5)), "data.frame")
})

test_that("volumes can be calculated from species names",{
  v <- IFNvolume(exampleTreeData, provinceFromID = TRUE)
  exampleTreeData2 <- exampleTreeData
  exampleTreeData2$Species <- v$Name
  exampleTreeData2$Species[18] <- "Acer spp."
  expect_s3_class(IFNvolume(exampleTreeData2, provinceFromID = TRUE), "data.frame")
  expect_s3_class(IFNvolume(exampleTreeData2, provinceFromID = FALSE), "data.frame")
})

test_that("volumes can be calculated for medfate",{
  testthat::skip_if_not_installed("medfate")
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(IFNvolume_medfate(medfate::exampleforest, medfate::SpParamsMED, province = "8"), "double")
  expect_type(IFNvolume_medfate(medfate::exampleforest, medfate::SpParamsMED, province = "8", level = "stand"), "double")
})
