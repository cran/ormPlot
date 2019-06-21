## load the data

test_data<-ormPlot::educ_data
dd<<-rms::datadist(test_data,q.effect = c(0.5, 0.75))
options(datadist="dd")

#create the test model

test_model_001<-rms::orm(educ_3 ~ Rural + sex + max_SEP_3 + n_siblings +
                           cran_rzs + height_rzs +  FW_rzs + YOBc +
                           (YOBc * sex) + (YOBc * Rural), data = test_data)

test_that("object has the right class", {

  a<-summary(test_model_001)
  testthat::expect_s3_class(a,"summary.rms")
})
