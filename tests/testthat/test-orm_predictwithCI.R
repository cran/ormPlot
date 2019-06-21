## load the data

test_data<-ormPlot::educ_data
dd<<-rms::datadist(test_data,q.effect = c(0.5, 0.75))
options(datadist="dd")

#create the test model

test_model_001<-rms::orm(educ_3 ~ Rural + sex + max_SEP_3 + n_siblings +
                           cran_rzs + height_rzs +  FW_rzs + YOBc +
                           (YOBc * sex) + (YOBc * Rural), data = test_data)

test_model_002<-rms::orm(educ_3 ~ Rural + sex + n_siblings + cran_rzs + height_rzs +
                      FW_rzs + YOBc + (YOBc * sex) + (YOBc * Rural), data = test_data)

test_model_003 <- rms::orm(educ_3 ~ Rural + sex + max_SEP_3 + cran_rzs, data = test_data)


test_that("result is a data frame", {
  p <- predict_with_ci(test_model_001, cran_rzs, Rural, max_SEP_3, sex)
  expect_true(is.data.frame(p))
  p <- predict_with_ci(test_model_002, cran_rzs, Rural, sex)
  expect_true(is.data.frame(p))
  p3 <- predict_with_ci(test_model_003, cran_rzs)
  expect_true(is.data.frame(p3))
})

test_that("result has expected prediction data in it", {
  n = 10
  df <- predict_with_ci(test_model_001, cran_rzs, Rural, max_SEP_3, sex, np = n)

  ocran_pred_frame<-rms::Predict(test_model_001, cran_rzs, Rural, max_SEP_3, sex, type = "model.frame",np=n)

  #generate the probabilties for each level of education using a built in function that deos not give CI's
  df_fitted<-rms:::predict.orm(test_model_001,ocran_pred_frame, type="fitted.ind")


  for(i in 1:length(df_fitted)){
    expect_equal(0, (df$Probability[i]-df_fitted[i]), tolerance = 0.00000001)
  }

})

test_that("result has expected column names", {
  p <- predict_with_ci(test_model_001, cran_rzs, Rural, sex)
  pcols<- colnames(p[(ncol(p)-3):ncol(p)])
  expcols<-c("Probability","lower","upper","dependent")
  expect_equal(pcols, expcols)

  p <- predict_with_ci(test_model_002, cran_rzs, Rural, sex)
  pcols<- colnames(p[(ncol(p)-3):ncol(p)])
  expect_equal(pcols, expcols)

})


