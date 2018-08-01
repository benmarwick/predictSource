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
knitr::kable(ObsidianSources[1:6,], caption="Table 1.  Sample of the data in the file ObsidianSources.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CheckData<-fn.CheckData(data=ObsidianSources,CheckDupVars=c("Code","ID"),GroupVar="Code",Groups="All",AnalyticVars=analyticVars)
knitr::kable(CheckData$Nvalues, caption="Table 2.  Number of values for each element from each Jemez source.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CheckData<-fn.CheckData(data=ObsidianSources,CheckDupVars=c("Code","ID"),GroupVar="Code",Groups="All",AnalyticVars=analyticVars)
knitr::kable(CheckData$Summary[1:6,], caption="Table 3.  Sample of the descriptive statistics for the data frame ObsidianSources.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr")
boxPlots<-fn.BoxPlots(data=ObsidianSources, GroupVar="Code",                Groups="All",AnalyticVars=analyticVars,Nrow=2,Ncol=2)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
ObsidianSources<-ObsidianSources[,c("Code",analyticVars)]
Artifacts <- ObsidianArtifacts[,c("Code",analyticVars)]
SourcesCode <- as.vector(ObsidianSources[,"Code"], mode="character")
ArtifactsCode <- as.vector(paste(Artifacts[,"Code"],"A",sep="."),mode="character")
Codes <- c(SourcesCode, ArtifactsCode)
SourcesArtifacts <- data.frame(rbind(ObsidianSources,Artifacts)[,analyticVars], Code = Codes)
boxPlots<-fn.BoxPlots(data=SourcesArtifacts, GroupVar="Code", Groups="All",AnalyticVars=c("Rb","Nb"),Nrow=2,Ncol=1)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr")
pairsPlot <- fn.PairsPlot(data=ObsidianSources, GroupVar="Code", Groups="A", AnalyticVars=analyticVars)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar="Code", Groups = "All", AnalyticVars=analyticVars)
knitr::kable(CV.corr$corr, caption="Table 4.  Spearman correlations among pairs of elements from the Jemez sources.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar="Code", Groups = "All", AnalyticVars=analyticVars)
knitr::kable(CV.corr$CV, caption="Table 5.  Coefficients of variation of the elements from the Jemez sources.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "A",
          AnalyticVars = rbind(analyticVars[1:2],analyticVars[c(1,3)]), PlotEllipses=T, LowessLine=T)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "A",
          AnalyticVars = rbind(analyticVars[1:2],analyticVars[c(1,3)]), PlotEllipses=T, KernelSmooth=T)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "All", 
          AnalyticVars = analyticVars[1:2], PlotByGroup=F, PlotPoints=F, PlotMedians=T, PlotHulls=T)


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "All",
          AnalyticVars = analyticVars[1:2], PlotByGroup = F, PlotColors=T, PlotEllipses=T, LowessLine=T, Identify=T)
data("sources.data.check")
knitr::kable(sources.data.check,caption="Data generating identified points")
#

