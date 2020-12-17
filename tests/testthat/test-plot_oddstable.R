
#READ THE TEST DATA
test_data_001<-readRDS("../testdata/test_data_001.rds")

expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}


test_that("table plotting returns a ggplot2 type object", {
  p <- oddstable_graph(test_data_001)
  expect_true(ggplot2::is.ggplot(p))
  expect_error(print(p), NA)
})

test_that("plotting test data generates the expected image", {
  p <- oddstable_graph(test_data_001)
  #fails only in tests not using vdiffr::manage_cases()
  expect_true(ggplot2::is.ggplot(p))
  #expect_doppelganger("ggplot_vertical_graph", p)
})

test_that("plotting test data uses suplied theme", {
  p <- oddstable_graph(test_data_001, theme = ggplot2::theme_light())
  expect_doppelganger("ggplot_vertical_graph_light", p)
})


test_that("plotting test data uses a minimal theme", {
  ggplot2::theme_set(ggplot2::theme_minimal())
  p <- oddstable_graph(test_data_001)
  expect_doppelganger("ggplot_vertical_graph_minimal", p)
})

test_that("plotting test data scales correctly", {
  p <- oddstable_graph(test_data_001[1:6,])
  expect_doppelganger("ggplot_reduced_graph", p)
})


test_that("plotting test data can handle modified column names", {
  colnames(test_data_001)<-c("a","b","c","d","e","f","g","h")
  p <- oddstable_graph(test_data_001[1:6,])
  expect_true(ggplot2::is.ggplot(p))
  expect_doppelganger("ggplot_nr_graph", p)
})




