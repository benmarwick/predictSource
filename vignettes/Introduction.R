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


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CheckData<-fn.CheckData(data=ObsidianSources,CheckDupVars=c("Code","ID"),GroupVar="Code",Groups="All",AnalyticVars=analyticVars)
head(CheckData$Summary)


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
CV.corr$corr


## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar="Code", Groups = "All", AnalyticVars=analyticVars)
CV.corr$CV


