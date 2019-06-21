#READ THE TEST DATA
test_data_001 <- readRDS("../testdata/test_data_001.rds")

test_that("returns a ggplot object", {
  p <- orm_graph(test_data_001)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("new row names are correctly alligned", {
  p <- orm_graph(test_data_001[1:4,], row.names.y = c("sons",
                                                     "odds 1",
                                                     "head",
                                                     "odds 2"))
  expect_true(ggplot2::is.ggplot(p))
  vdiffr::expect_doppelganger("orm_graph_4",p)
})

test_that("aborts when new row names are not the same length", {
  expect_error(orm_graph(test_data_001, row.names.y = c("A","B")),
               "The provided variable names are not covering all options:*")
})

test_that("plotting test data can handle modified column names", {
  colnames(test_data_001)<-c("a","b","c","d","e","f","g","h")
  p <- orm_graph(test_data_001[1:6,])
  expect_true(ggplot2::is.ggplot(p))
  vdiffr::expect_doppelganger("ggplot_letters_orm_graph", p)
})

test_that("x axis limits can be changed", {
  p <- orm_graph(test_data_001[1:6,],limits=c(0.5,2))
  expect_true(ggplot2::is.ggplot(p))
  vdiffr::expect_doppelganger("ggplot_limits_orm_graph", p)
})

test_that("x axis breaks can be changed", {
  p <- orm_graph(test_data_001[1:6,],breaks=c(0.5,2))
  expect_true(ggplot2::is.ggplot(p))
  vdiffr::expect_doppelganger("ggplot_breaks_orm_graph", p)
})

test_that("x axis tickmarks can be changed", {
  p <- orm_graph(test_data_001[1:6,],shape=22)
  expect_true(ggplot2::is.ggplot(p))
  vdiffr::expect_doppelganger("ggplot_shape_orm_graph", p)
})



