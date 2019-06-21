set.seed(123)
#load the libraries
library(rms)
library(ormPlot)

#make the datadist
dd<-rms::datadist(educ_data)
options(datadist="dd")

#create the model
cran_model <- orm(educ_3 ~ Rural + sex + max_SEP_3 + cran_rzs, data = educ_data)

#get the predictions of the orm model with confidence intervals for all levels
predictiondf<-predict_with_ci(cran_model, cran_rzs, Rural, sex, max_SEP_3)
#show the predictions head
head(predictiondf)

#get the predictions of the orm model with confidence intervals for sex only
predictiondf_sex<-predict_with_ci(cran_model,  sex)
#show the predictions head
head(predictiondf_sex)
