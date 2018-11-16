## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 7,
  comment = "#>"
)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotCp=F)

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
knitr::kable(CheckData$statistics[1:6,], caption="Table 3.  Sample of the descriptive statistics for the data frame ObsidianSources.")
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
knitr::kable(CV.corr$corr, caption="Table 4a.  Spearman correlations among pairs of elements from the Jemez sources,
by group.")
#

## ------------------------------------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar=" ", Groups=" ", AnalyticVars=analyticVars)
knitr::kable(CV.corr$corr, caption="Table 4b.  Spearman correlations among pairs of elements from the Jemez sources, all groups combined.")
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
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = c("A","B"),
                AnalyticVars = rbind(analyticVars[c(1,4)],analyticVars[c(1,5)]), PlotEllipses=T, LowessLine=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = c("A", "B"),
          AnalyticVars = rbind(analyticVars[c(1,4)], analyticVars[c(1,5)]), PlotEllipses=T, KernelSmooth=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = "All", 
          AnalyticVars = analyticVars[1:2], PlotByGroup=F, PlotPoints=F, PlotMedians=T, PlotHulls=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = "All",
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
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="A",
    AnalyticVars=c("Rb","Zr"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="B",
    AnalyticVars=c("Rb","Zr"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="A",
    AnalyticVars=c("Rb","Nb"))
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="B",
    AnalyticVars=c("Rb","Nb"))

## ------------------------------------------------------------------------
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="All",
    AnalyticVars=c("Rb","Zr"),qqPlot=F)
knitr::kable(plot.2d.Gauss$pvalues,caption="Table 7.a.  P-values from test statistics for a bivariable Rb/Zr distribution.")
#

## ------------------------------------------------------------------------
plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups="All",
    AnalyticVars=c("Rb","Nb"),qqPlot=F)
knitr::kable(plot.2d.Gauss$pvalues,caption="Table 7.b.  Samples sizes and p-values from test statistics for a bivariable Rb/Nb distribution.")
#

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.pca <- fn.pca(data=ObsidianSources, ID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars, pcPlot=F, ScreePlot=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.pca <- fn.pca(data=ObsidianSources, ID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars, pcPlot=F, BoxPlots=T)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.pca <- fn.pca(data=ObsidianSources, ID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars) 

## ------------------------------------------------------------------------
save.pca <- fn.pca(data=ObsidianSources, ID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                   pcPlot=F)
knitr::kable(save.pca$variances, caption="Table 8.  Proportions of variance explained.")
knitr::kable(save.pca$weights, caption="Table 9.  Weights for each principal component.")
knitr::kable(head(save.pca$DataPlusPredicted), caption="Table 10.  Original data and principal components.")
#

## ------------------------------------------------------------------------
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
pca.Gauss <- fn.pca.Gauss(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, qqPlot=F)
knitr::kable(pca.Gauss$p.values, caption="Table 11. P-values from test statistics for a bivariate Gaussian distribution of the first two principal components at each obsidian source.")
#

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotCp=F)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotTree=F)

## ----echo=FALSE, message=FALSE-------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotCp=F, plotTree=F)
tree$Tree

## ---- echo=FALSE, message=FALSE------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotTree=F, plotCp = F)
knitr::kable(tree$classification, caption = "Table y.  Classification tree results for the obsidian sources.")
#

## ---- echo=FALSE, message=FALSE------------------------------------------
library(karon)
data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
 tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotTree=F, plotCp = F)
knitr::kable(tree$CpTable, caption = "Table z.  Cp table from the classification tree for the obsidian sources.")
#

## ---- echo=FALSE, message=FALSE------------------------------------------
library(karon)
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
   Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb", predictSources=T, predictData=ObsidianArtifacts, ID="ID",
   plotTree=F, plotCp=F)
knitr::kable(save.tree$predictedSources[1:5,], caption = "Table zz.  An example of the data frame containing predicted sources of obsidian artifacts from the classification tree for the obsidian sources.")

## ---- echo=FALSE, message=FALSE------------------------------------------
library(karon)
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
   Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb", predictSources=T, predictData=ObsidianArtifacts, ID="ID",
   plotTree=F, plotCp=F)
knitr::kable(save.tree$predictedTotals, caption = "Table zz1.  Predicted number of obsidian artifacts
from each source based on the classification tree for the obsidian sources.")

## ---- echo=FALSE, message=FALSE------------------------------------------
library(karon)
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                ID="ID", Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", plotTree=F, plotCp = F, predictSources=T,
                predictData=ObsidianArtifacts)
predictedSources<-tree$predictedSources[tree$predictedSources[,"source"]!="A",]
save.pca <- fn.pca(data=predictedSources, ID="ID", GroupVar="source",Groups="All", AnalyticVars=analyticVars, PlotEllipses=F)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
data(ObsidianSources)
 analyticVars<-c("Rb","Sr","Y","Zr","Nb")
 save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",   AnalyticVars=analyticVars, NvarUsed=3, plotImportance=F)

## ----echo=FALSE, results='hide',message=FALSE----------------------------
data(ObsidianSources)
 analyticVars<-c("Rb","Sr","Y","Zr","Nb")
 save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",   AnalyticVars=analyticVars, NvarUsed=3, plotErrorRate=F)

## ----echo=FALSE, message=FALSE-------------------------------------------
data(ObsidianSources)
 analyticVars<-c("Rb","Sr","Y","Zr","Nb")
 save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, NvarUsed=3, plotImportance=F, plotErrorRate=F)
knitr::kable(save.randomForest$importance, caption="Table z. Relative variable importance in a random forest model for the Jemez obsidian source data.")
#

## ----echo=FALSE, message=FALSE-------------------------------------------
data(ObsidianSources)
 analyticVars<-c("Rb","Sr","Y","Zr","Nb")
 save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, NvarUsed=3, plotImportance=F, plotErrorRate=F)
knitr::kable(save.randomForest$confusion, caption="Table z. The confusion matrix from a random forest model for the Jemez obsidian source data: accuracy of classification.")
#

## ----echo=FALSE, message=FALSE-------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, NvarUsed=3, plotErrorRate=F, plotImportance=F, predictSources=T,predictData=ObsidianArtifacts, plotSourceProbs=F)
knitr::kable(save.randomForest$predictedSources[1:5,], caption="Table z1. The predicted source and predicted 
  source probabilities from a random forest model for the Jemez obsidian artifact data.")
knitr::kable(save.randomForest$predictedTotals, caption="Table z2. The predicted number of artifacts from 
  each Jemez source from a random forest model for the Jemez obsidian artifact data.")
#

## ----echo=FALSE, results='hide',message=FALSE----------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, NvarUsed=3, plotErrorRate=F, plotImportance=F, predictSources=T,predictData=ObsidianArtifacts, plotSourceProbs=T)

## ----echo=FALSE, message=FALSE-------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars, ID="ID", NvarUsed=3, plotErrorRate=F, plotImportance=F, predictSources=T,predictData=ObsidianArtifacts, plotSourceProbs=F)
check<-save.randomForest$predictedSources
rows<-((check[,"C"]>0.14)&(check[,"C"]<0.35))
checkC<-check[rows,]
orderedRows<-order(checkC[,"C"])
knitr::kable(checkC[orderedRows,], caption="Table z3.  Rows with relative large probabilities of
             misclassification to source C.")

## ----echo=FALSE, results='hide', message=FALSE---------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", ID="ID", AnalyticVars=analyticVars, NvarUsed=3, plotErrorRate=F, plotImportance=F, predictSources=T,predictData=ObsidianArtifacts, plotSourceProbs=F)
predictedSources<-save.randomForest$predictedSources[save.randomForest$predictedSources[,"source"]!="A",]
save.pca <- fn.pca(data=predictedSources, ID="ID", GroupVar="source",Groups="All", AnalyticVars=analyticVars, PlotEllipses=F)

## ----echo=FALSE, results='hide', message=FALSE---------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
sources <- unique(ObsidianSources[,"Code"])
save.tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
  Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb", predictSources=T, predictData=ObsidianArtifacts, ID="ID",
  plotTree=F, plotCp=F)
pca.eval <- fn.pca.evaluation(SourceData=ObsidianSources, ArtifactData=save.tree$predictedSources,
  SourceGroup= "Code", ArtifactGroup="source",known.sources=sources, predicted.sources=sources,
  AnalyticVars=analyticVars, plotAllPoints=T, plotHullsOutsidePoints=T, plotOutsidePoints=T)

## ----echo=FALSE, message=FALSE-------------------------------------------
data(tree.data.check)
knitr::kable(tree.data.check, caption="Table zz. Artifacts which may have misidentified sources identified by using Identify=T in fn.pca.evaluation() after fitting a tree model.")

## ----echo=FALSE, results='hide', message=FALSE---------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
sources <- unique(ObsidianSources[,"Code"])
save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
    AnalyticVars=analyticVars, ID="ID", NvarUsed=3, plotErrorRate=F, plotImportance=F,
    predictSources=T, predictData=ObsidianArtifacts, plotSourceProbs=F)
  pca.eval <- fn.pca.evaluation(SourceData=ObsidianSources,
    ArtifactData=save.randomForest$predictedSources, SourceGroup= "Code", ArtifactGroup="source",
    known.sources=sources, predicted.sources=sources, AnalyticVars=analyticVars,
    plotAllPoints=T, plotHullsOutsidePoints = T, plotOutsidePoints = T)

