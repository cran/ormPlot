
## load the data

test_data<-educ_data
dd<<-rms::datadist(test_data,q.effect = c(0.5, 0.75))
options(datadist="dd")

#create the test model

test_model_001<-rms::orm(educ_3 ~ Rural + sex + max_SEP_3 + n_siblings +
                           cran_rzs + height_rzs +  FW_rzs + YOBc +
                           (YOBc * sex) + (YOBc * Rural), data = test_data)

test_that("returns a ggplot object", {
  p <- plot.orm(test_model_001, cran_rzs, "Rural")
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plotting test data generates the expected image", {
  p <- plot.orm(test_model_001, "cran_rzs", "max_SEP_3",  c("Rural", "sex"))
  vdiffr::expect_doppelganger("prediction_ggplot_article", p)
})

test_that("can plot with only one value", {
  p <- plot.orm(test_model_001, cran_rzs)
  vdiffr::expect_doppelganger("prediction_ggplot_simplest", p)
})




test_that("plotting test data changes element names and order", {
  p <- plot.orm(test_model_001, "cran_rzs",
                      plot_cols = c("max_SEP_3"),
                      plot_rows = c("Rural", "sex"),
                      xlab = "Cranial volume (residuals to age an birth date)",
                      facet_labels = list(Rural = c("Urban", "Rural"),
                                          sex = c("Boys","Girls"),
                                          max_SEP_3 = c("Unskilled manual",
                                                      "Skilled manual",
                                                      "Non-manual"))
  )
  vdiffr::expect_doppelganger("prediction_ggplot_article_edited", p)
})

test_that("plotting test data accepts no vectors", {
  p <- plot.orm(test_model_001, cran_rzs,
                plot_cols = "max_SEP_3",
                plot_rows = c("Rural", "sex"),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  vdiffr::expect_doppelganger("prediction_ggplot_article_edited", p)
})

test_that("plotting test data accepts no quotes", {
  p <- plot.orm(test_model_001, cran_rzs,
                plot_cols = max_SEP_3,
                plot_rows = c(Rural, sex),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  vdiffr::expect_doppelganger("prediction_ggplot_article_edited", p)

  p <- plot.orm(test_model_001, cran_rzs,
                plot_cols = c(max_SEP_3),
                plot_rows = c(Rural, sex),
                xlab = "Cranial volume (residuals to age an birth date)",
                facet_labels = list(Rural = c("Urban", "Rural"),
                                    sex = c("Boys","Girls"),
                                    max_SEP_3 = c("Unskilled manual",
                                                  "Skilled manual",
                                                  "Non-manual"))
  )
  vdiffr::expect_doppelganger("prediction_ggplot_article_edited", p)
})


test_that("object names get converted to strings", {
  res<-convert_arg(c(a,b))
  expect_equal(c("a","b"),res)
  res<-convert_arg(a)
  expect_equal("a",res)
  res<-convert_arg("a")
  expect_equal("a",res)

})
