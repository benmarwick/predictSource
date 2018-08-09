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

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr")
boxPlots<-fn.BoxPlots(data=ObsidianSources, GroupVar="Code",                Groups="All",AnalyticVars=analyticVars,Nrow=2,Ncol=2)


## ----echo=FALSE, results='hide',message=FALSE----------------------------
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


## ----echo=FALSE, results='hide',message=FALSE----------------------------
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

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = c("A","B"),
                AnalyticVars = rbind(analyticVars[c(1,4)],analyticVars[c(1,5)]), PlotEllipses=T, LowessLine=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = c("A", "B"),
          AnalyticVars = rbind(analyticVars[c(1,4)], analyticVars[c(1,5)]), PlotEllipses=T, KernelSmooth=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "All", 
          AnalyticVars = analyticVars[1:2], PlotByGroup=F, PlotPoints=F, PlotMedians=T, PlotHulls=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", labID = "ID", Groups = "All",
          AnalyticVars = rbind(analyticVars[c(1,4)], analyticVars[c(1,5)]), PlotByGroup = F, PlotColors=T, PlotEllipses=T, LowessLine=T, Identify=T)

## ------------------------------------------------------------------------
data("sources.data.check")
knitr::kable(sources.data.check,caption="Table 6. Data generating identified points.")
#

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot3d<-fn.3dPlot(data=ObsidianSources, GroupVar="Code", Groups=c("A","B","C"), AnalyticVars=analyticVars,
                   Selections=rbind(analyticVars[1:3],analyticVars[2:4]))

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", labID="ID", Groups="A",
    AnalyticVars=c("Rb","Zr"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", labID="ID", Groups="B",
    AnalyticVars=c("Rb","Zr"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", labID="ID", Groups="A",
    AnalyticVars=c("Rb","Nb"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", labID="ID", Groups="B",
    AnalyticVars=c("Rb","Nb"))

## ------------------------------------------------------------------------
data(RbNb.pvalues)
data(RbZr.pvalues)
knitr::kable(RbZr.pvalues,caption="Table 7.a.  P-values from test statistics for a bivariable Rb/Zr distribution.")
knitr::kable(RbNb.pvalues,caption="Table 7.b.  P-values from test statistics for a bivariable Rb/Nb distribution.")
#

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.pca <- fn.pca(data=ObsidianSources, labID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars, pcPlot=F, ScreePlot=T)
save.pca <- fn.pca(data=ObsidianSources, labID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars, pcPlot=F, BoxPlots=T)
save.pca <- fn.pca(data=ObsidianSources, labID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars) 

## ------------------------------------------------------------------------
save.pca <- fn.pca(data=ObsidianSources, labID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                   pcPlot=F)
knitr::kable(save.pca$Summary$importance, caption="Table 8.  Proportions of variance explained.")
knitr::kable(save.pca$weights, caption="Table 9.  Weights for each principal component.")
knitr::kable(head(save.pca$DataPlusPredicted), caption="Table 10.  Original data and principal components.")
#

