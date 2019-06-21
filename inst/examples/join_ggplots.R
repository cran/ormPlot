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

#return modifiable ggplots
plots<-forestplot(s, return_ggplots = TRUE )

#modify like any ggplot2 object
table<-plots[[1]] +  theme(axis.text=element_text(size = 12),
                           axis.line.x = element_line(color = "red", size = 1),
                           axis.text.y = element_blank())

graph<-plots[[2]] + theme(axis.line = element_line(color = "red", size = 1),
                          axis.text.y = element_text())

#join the graphs
join_ggplots(  graph, table, title = "", plot.widths = c(0.6,0.4))


