set.seed(123)
#load the libraries
library(rms)
library(ormPlot)

#make the datadist
dd<-rms::datadist(educ_data)
options(datadist="dd")

#create the model
cran_model <- orm(educ_3 ~ YOBc + sex + height_rzs + n_siblings  + cran_rzs, data = educ_data)

#the antilog true produces odd ratios (default value for orm and lrm)
s<-summary(cran_model, antilog = TRUE)

#set the plotting default theme (optional)
theme_set(theme_classic())

#show simply the result
forestplot(s)

#return modifiable ggplots
forestplot(s, return_ggplots = TRUE )

#new row names and header
newnames <- c("Year of birth", "Height", "Number of children", "Cranial volume", "Sex" )
newhead <- c("Odds Ratio", "CI 5%", "CI 95%" )

#adjust also the relative plot widths and change the color and shape
newtheme <- theme_classic() + theme(text = element_text(color = "red", size = 12),
                                    line = element_line(color= "red"),
                                    rect = element_rect(color="red"))

forestplot(s, row.names.y = newnames, header = newhead,
           plot.widths = c(0.6,0.4), shape = 17,
           theme = newtheme)
