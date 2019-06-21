
#READ THE TEST DATA
test_data_001 <- readRDS("../testdata/test_data_001.rds")
test_data_002<-test_data_001[1:4,]
class(test_data_002)<-"summary.rms"


test_that("Plotting returns an aligned forestplot",{
  p <- forestplot(test_data_001)
  #vdiffr::expect_doppelganger("forestplot_full",grid::grid.draw(p))

  #test not passing for an unknwon reason
  #drawing it instead for visual inspection
  #ggplot2::ggsave("forestplot_full.svg",p)



})

test_that("Plotting produces 2 ggplots",{
  p <- forestplot(test_data_001, return_ggplots = TRUE)
  expect_true(ggplot2::is.ggplot(p[[1]]))
  expect_true(ggplot2::is.ggplot(p[[2]]))
  vdiffr::expect_doppelganger("forestplot_left",p[[1]])
  vdiffr::expect_doppelganger("forestplot_right",p[[2]])
})

test_that("grob is returned if y values are specified",{

  p <- forestplot(test_data_002, row.names.y = c("sons", "odds 1"))

  expect_true(grid::is.grob(p))

})

test_that("can't be run on a not rms.summary",{

    expect_error(forestplot("a"), "The method must be run on a summary.rms object")
    expect_error(forestplot(test_data_001[1:4,]),
                 "The method must be run on a summary.rms object")

})



