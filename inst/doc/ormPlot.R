## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warnings = FALSE,
  fig.width = 6,
  fig.height = 4
)
library(rms)

## ----setup---------------------------------------------------------------
#the regression modelling by F. Harrell
library(rms)

#the extension for plotting the rms summary objects
library(ormPlot)

#to modify plots
library(ggplot2)

## ----eval=FALSE----------------------------------------------------------
#  #show first 6 rows
#  head(educ_data)

## ----echo=FALSE, results="asis"------------------------------------------
pander::pandoc.table(head(educ_data), split.tables=Inf)


## ---- eval = FALSE-------------------------------------------------------
#  #show variable explanation
#  help(educ_data)

## ------------------------------------------------------------------------
#q.effect determines for what range the odds ratios are given on plots
dd <- datadist(educ_data, q.effect = c(0.5, 0.75))
#set it also to options
options(datadist="dd")

## ------------------------------------------------------------------------
#see help(orm) for further info
orm_model<-orm(educ_3 ~ Rural + sex + n_siblings + cran_rzs + height_rzs + 
    FW_rzs + YOBc + (YOBc * sex) + (YOBc * Rural), data = educ_data)

## ------------------------------------------------------------------------
plot(orm_model, cran_rzs)

## ------------------------------------------------------------------------
plot(orm_model, cran_rzs,  Rural, sex)


## ------------------------------------------------------------------------

p<-plot(orm_model, cran_rzs,  Rural, sex,
        xlab = "Cranial volume (residuals to age an birth date)",
        facet_labels = list(Rural = c("Urban", "Rural"),sex=c("Male","Female")))



colors <- c("#4a9878", "#0a191e", "#d8b65c")
educ_names <- c("Primary", "Secondary", "Tertiary")

# further modifing like any other ggplot
final_plot<-p + labs(color = "Education", fill = "Education") + 
  scale_color_manual(values = colors, labels = educ_names) +
  scale_fill_manual(values = colors, labels = educ_names) 
 
final_plot 

## ----eval = FALSE--------------------------------------------------------
#  ggsave("educ_cran.svg",final_plot, height = 8 ,width = 8)

## ----fig.width=7, fig.height=4-------------------------------------------
forestplot(summary(orm_model))

## ----fig.width=6---------------------------------------------------------
# you can use also use plot instead of forestplot
plots<-forestplot(summary(orm_model), return_ggplots=T)
plots[[1]]
plots[[2]]

## ---- fig.width=7--------------------------------------------------------

p1 <- plots[[1]] + 
  scale_x_discrete(labels=c("Mean", "Lower CI", "Upper CI"), 
                   position = "top",
                   name = NULL)

# the x axis is actually y axis because the cordinates are flipped with coord_flip()
p2 <- plots[[2]] + scale_y_continuous(breaks = c(0.5, 0.7, 0.9, 1.1),
                                position = "right")
  

forestplot<-join_ggplots(p1,p2)


## ---- eval=FALSE---------------------------------------------------------
#  ggsave("forestplot.svg",forestplot)

