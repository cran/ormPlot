
## load the data

test_data<-educ_data
dd<<-rms::datadist(test_data,q.effect = c(0.5, 0.75))
options(datadist="dd")

#create the test model

test_model_002<-rms::lrm(educ_3 ~ Rural + sex + max_SEP_3 + n_siblings +
                           cran_rzs + height_rzs +  FW_rzs + YOBc +
                           (YOBc * sex) + (YOBc * Rural), data = test_data)

expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

test_that("returns a ggplot object", {
  p <- plot.lrm(test_model_002, cran_rzs, "Rural")
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plotting test data generates the expected image", {
  p <- plot.lrm(test_model_002, "cran_rzs", "max_SEP_3",  c("Rural", "sex"))
  expect_doppelganger("prediction_ggplot_article_lrm", p)
})

test_that("can plot with only one value", {
  p <- plot.lrm(test_model_002, cran_rzs)
  expect_doppelganger("prediction_ggplot_simplest", p)
})




test_that("plotting test data changes element names and order", {
  p <- plot.lrm(test_model_002, "cran_rzs",
                      plot_cols = c("max_SEP_3"),
                      plot_rows = c("Rural", "sex"),
                      xlab = "Cranial volume (residuals to age an birth date)",
                      facet_labels = list(Rural = c("Urban", "Rural"),
                                          sex = c("Boys","Girls"),
                                          max_SEP_3 = c("Unskilled manual",
                                                      "Skilled manual",
                                                      "Non-manual"))
  )
  expect_doppelganger("prediction_ggplot_article_edited_lrm", p)
})

test_that("plotting test data accepts no vectors", {
  p <- plot.lrm(test_model_002, cran_rzs,
                plot_cols = "max_SEP_3",
                plot_rows = c("Rural", "sex"),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  expect_doppelganger("prediction_ggplot_article_edited_lrm", p)
})

test_that("plotting test data accepts no quotes", {
  p <- plot.lrm(test_model_002, cran_rzs,
                plot_cols = max_SEP_3,
                plot_rows = c(Rural, sex),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  expect_doppelganger("prediction_ggplot_article_edited_lrm", p)

  p <- plot.lrm(test_model_002, cran_rzs,
                plot_cols = c(max_SEP_3),
                plot_rows = c(Rural, sex),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  expect_doppelganger("prediction_ggplot_article_edited_lrm", p)
})


