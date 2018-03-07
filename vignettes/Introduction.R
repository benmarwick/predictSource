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
data(ObsidianData)
head(ObsidianData)
names(ObsidianData)

## ------------------------------------------------------------------------
Els <-c("Rb","Sr","Y","Zr","Nb")  # define the elements used in the analysis
Folder <- paste0(here::here(), "/vignettes/")
checks <- fn.CheckData(data = ObsidianData, 
                     CheckDupVars = c("Code","ID"),
                     GroupVar = "Code", 
                     Groups = "All", 
                     AnalyticVars = Els, 
                     folder = Folder, 
                     ds.duplicates = "Duplicates.csv", 
                     ds.NegValues = "NegValues.csv", 
                     ds.Nsamples = "Nsamples", 
                     ds.summary = "SummaryStats")

str(checks)

## ------------------------------------------------------------------------
checks$Nvalues

## ------------------------------------------------------------------------
# windows()
Els <- c("Rb", "Sr")
boxplots <- 
fn.BoxPlots(data = ObsidianData, 
            GroupVar = "Code", 
            Groups = c("AW", "CC"), 
            AnalyticVars = Els,
            Nrow = 1,
            Ncol = 2)

## ------------------------------------------------------------------------
pairsplots <- 
fn.PairsPlot(data = ObsidianData,
             GroupVar = " ",
             Groups = " ",
             AnalyticVars = Els)

## ------------------------------------------------------------------------
Els <- c("Rb","Sr","Y","Zr","Nb")  # define the elements used in the analysis
cvcorr <- 
  fn.CV.corr(data = ObsidianData, 
           GroupVar = "Code", 
           Groups = "All", 
           AnalyticVars = Els, 
           Transpose = T, 
           folder = Folder,
           ds.CV = "CV.csv", 
           ds.corr = "corr.csv")

## ------------------------------------------------------------------------
head(cvcorr$CV)

## ------------------------------------------------------------------------
head(cvcorr$corr)

