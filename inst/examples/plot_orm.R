#load the libraries
library(rms)
library(ormPlot)

#make the datadist
dd<-datadist(educ_data)
options(datadist='dd')

#create the model
cran_model <- orm(educ_3 ~ Rural + sex + max_SEP_3 + cran_rzs, data = educ_data)

#plot the predictions of the model for varying one variable only
plot(cran_model, cran_rzs)

#customize the plotting varying all variables
plot(cran_model, cran_rzs,
      plot_cols = max_SEP_3,
      plot_rows = c(Rural, sex),

      #setting new x-label (optional)
     xlab = "Cranial volume (residuals to age an birth date)",

     #setting new facet labels (optional)
     facet_labels = list(Rural = c("Urban", "Rural"),
                          sex = c("Boys","Girls"))
     )
