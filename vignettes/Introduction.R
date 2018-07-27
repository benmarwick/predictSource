## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 7,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
head(ObsidianSources)
names(ObsidianSources)

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CheckData<-fn.CheckData(data=ObsidianSources,CheckDupVars=c("Code","ID"),GroupVar="Code",Groups="All",AnalyticVars=analyticVars)
CheckData$Nvalues


