data(exampleTreeData)
test_that("biomass can be calculated",{
  expect_s3_class(IFNbiomass(exampleTreeData, as.CO2 = TRUE), "data.frame")
  expect_s3_class(IFNbiomass(exampleTreeData, as.CO2 = FALSE), "data.frame")
  expect_s3_class(IFNbiomass(exampleTreeData, as.CO2 = TRUE,
                             DBHclasses = seq(0, 120, by=5)), "data.frame")
  expect_s3_class(IFNbiomass(exampleTreeData, as.CO2 = FALSE,
                             DBHclasses = seq(0, 120, by=5)), "data.frame")
})

test_that("biomass can be calculated from species names",{
  v <- IFNbiomass(exampleTreeData)
  exampleTreeData2 <- exampleTreeData
  exampleTreeData2$Species <- v$Name
  exampleTreeData2$Species[18] <- "Acer spp."
  expect_s3_class(IFNbiomass(exampleTreeData2), "data.frame")
  expect_s3_class(IFNbiomass(exampleTreeData2), "data.frame")
})
