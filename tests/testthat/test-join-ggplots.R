#READ THE TEST DATA
test_data_001<-readRDS("../testdata/test_data_001.rds")

test_that("Plotting returns a grob",{
  p <- forestplot(test_data_001, return_ggplots = TRUE)
  p <- join_ggplots(p[[1]],p[[2]])

  expect_true(grid::is.grob(p))

})
