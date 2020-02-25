## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 7,
  comment = "#>"
)

## ----results='hide',message=FALSE, fig.cap="Figure 1.1: Classification tree analysis for the Jemez obsidian sources."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotCp = FALSE
  )


## ----results='hide', message=FALSE, fig.keep='last', fig.cap="Figure 1.2 Principal components plot the sources of Jemez obsidian artifacts predicted from a classification tree with points outside the convex hull of the source samples."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
save.tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = FALSE,
    predictUnknowns = TRUE,
    unknownData = ObsidianArtifacts,
    unknownID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
pca.eval <-
  ps_pcaEvaluation(
    SourceData = ObsidianSources,
    unknownData = save.tree$predictedSourceUnknowns,
    SourceGroup = "Code",
    unknownGroup = "predicted",
    known_sources = sources,
    predicted_sources = sources,
    AnalyticVars = analyticVars,
    plotAllPoints = TRUE,
    plotHullsOutsidePoints = TRUE,
    plotOutsidePoints = TRUE
  )


## -----------------------------------------------------------------------------
library(predictSource)
library(magrittr)
library(kableExtra)
data(ObsidianSources)
knitr::kable(ObsidianSources[1:6,],
             caption = "Table 2.1: Sample of the data in the file ObsidianSources.") %>%
  kable_styling(full_width = F,
                position = "center")
#

## ----eval = FALSE-------------------------------------------------------------
#  object_name <- read.csv(file = 'file name')

## ----eval = FALSE-------------------------------------------------------------
#  new_object_name <- ps_createData(data=object_name)$dataOut

## ----eval = FALSE-------------------------------------------------------------
#  new_object_name <-list_name$dataOut

## ---- eval = FALSE------------------------------------------------------------
#  ps_combine <- function()
#  rbind(data1, data2, ...)

## ----eval = FALSE-------------------------------------------------------------
#  analysisFile <- ps_combine()

## ----eval = FALSE-------------------------------------------------------------
#  ps_checkData(data=analysisFile, CheckDupVars, GroupVar, Groups, AnalyticVars)

## ----echo=FALSE, message=FALSE------------------------------------------------
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
checkData <-
  ps_checkData(
    data = ObsidianSources,
    CheckDupVars = c("Code", "ID"),
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars
  )
knitr::kable(checkData$Nvalues,
             caption = "Table 3.1: Number of values for each element from each Jemez source.") %>%
  kable_styling(full_width = F, position = "center")


## -----------------------------------------------------------------------------
library(predictSource)
data(ObsidianSources)
checkData <-
  ps_checkData(
    data = ObsidianSources,
    CheckDupVars = c("Code", "ID"),
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = c("Sr", "Rb", "Y")
  )
knitr::kable(checkData$statistics, caption = "Table 3.2: Descriptive statistics for the elements in the Jemez obsidian sources used in the classification tree in Figure 1.1.") %>%
  kable_styling(full_width = F, position = "center")


## -----------------------------------------------------------------------------
library(predictSource)
data(ObsidianArtifacts)
checkData <-
  ps_checkData(
    data = ObsidianArtifacts,
    CheckDupVars = c("Code", "ID"),
    GroupVar = "Code",
    ByGroup = FALSE,
    AnalyticVars = c("Sr", "Rb", "Y")
  )
knitr::kable(checkData$statistics, caption = "Table 3.3: Descriptive statistics for the elements in the obsidian artifacts used in the classification tree in Figure 1.1.") %>%
  kable_styling(full_width = F, position = "center")


## ----results='hide',message=FALSE, fig.cap="Figure 4.1: Box plots for the Jemez obsidian source data."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Nb")
boxPlots <-
  ps_boxPlots(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Nrow = 2,
    Ncol = 2
  )


## ----results='hide',message=FALSE, fig.cap="Figure 4.2: Box plots for rubidium and niobium comparing the Jemez obsidian sources and the obsidian artifacts assigned to those sources by Steve Shackley.  x.a labels the artifacts assigned to source x."----
library(predictSource)
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
ObsidianSources <- ObsidianSources[, c("Code", analyticVars)]
Artifacts <- ObsidianArtifacts[, c("Code", analyticVars)]
sourcesCode <- as.vector(ObsidianSources[, "Code"], mode = "character")
artifactsCode <-
  as.vector(paste(Artifacts[, "Code"], "a", sep = "."), mode = "character")
codes <- c(sourcesCode, artifactsCode)
sourcesArtifacts <-
  data.frame(rbind(ObsidianSources, Artifacts)[, analyticVars], Code = codes)
boxPlots <-
  ps_boxPlots(
    data = sourcesArtifacts,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = c("Rb", "Nb"),
    Nrow = 2,
    Ncol = 1
  )


## ----results='hide',message=FALSE, fig.cap="FIgure 4.3: Pairs plots of the Jemez obsidian source data."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
pairsPlot <-
  ps_pairsPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "A",
    AnalyticVars = analyticVars
  )


## -----------------------------------------------------------------------------
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
CV_corr <-
  ps_CV_corr(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    plotCorrs = FALSE
  )
knitr::kable(CV_corr$corr, caption = "Table 4.1: Spearman correlations among pairs of elements from the Jemez sources,
by group.") %>%
  kable_styling(full_width = F, position = "center")
#

## -----------------------------------------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
CV_corr <-
  ps_CV_corr(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    ByGroup = FALSE,
    plotCorrs = FALSE
  )
knitr::kable(CV_corr$corr, caption = "Table 4.2: Spearman correlations among pairs of elements from the Jemez obsidian sources, all groups combined.") %>%
  kable_styling(full_width = F, position = "center")
#

## ----results='hide',message=FALSE, fig.cap="Figure 4.4: Graphical display of the correlations between the Jemez obsidian source analysis variables with all groups combined."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
CV_corr <-
  ps_CV_corr(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    ByGroup = FALSE,
    plotCorrs = TRUE
  )

## -----------------------------------------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
CV_corr <-
  ps_CV_corr(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    ByGroup = TRUE,
    plotCorrs = FALSE
  )
knitr::kable(CV_corr$CV, caption = "Table 4.3:  Coefficients of variation of the elements from the Jemez sources.") %>%
  kable_styling(full_width = F, position = "center")
#

## ----results='hide',message=FALSE, fig.cap="Figure 4.5: Example of scatterplots for the obsidian Jemez source data with confidence ellipses and robust lowess smoothing lines."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot_2d <-
  ps_2dPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = c("A", "B", "C", "D"),
    AnalyticVars = analyticVars,
    VariablePairs = analyticVars[c(1, 4)],
    PlotEllipses = TRUE,
    LowessLine = TRUE,
    ByGroup = TRUE
  )

## ----results='hide',message=FALSE, fig.cap="Figure 4.6: Example of a scatterplot for obsidian Jemez source data with confidence ellipses and kernel smoothing lines."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot_2d <-
  ps_2dPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = c("A", "B", "C", "D"),
    AnalyticVars = analyticVars,
    VariablePairs = analyticVars[c(1, 4)],
    PlotEllipses = TRUE,
    KernelSmooth = TRUE,
    LowessLine = FALSE,
    ByGroup = TRUE
  )

## ----results='hide',message=FALSE, fig.cap="Figure 4.7: Example of a scatterplot for obsidian Jemez source data with the median values of and convex hulls of the points at each source."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot_2d <-
  ps_2dPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "All",
    AnalyticVars = analyticVars,
    VariablePairs = analyticVars[1:2],
    ByGroup = FALSE,
    PlotPoints = FALSE,
    PlotMedians = TRUE,
    PlotHulls = TRUE,
    LowessLine = FALSE
  )

## ----results='hide',message=FALSE, fig.cap="Figure 4.8: Example of a scatterplot for obsidian Jemez source data with data for all sources, with confidence ellipses and robust lowess smoothing lines."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot_2d <-
  ps_2dPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "All",
    AnalyticVars = analyticVars,
    VariablePairs = analyticVars[c(1, 4)],
    ByGroup = FALSE,
    PlotEllipses = TRUE,
    LowessLine = TRUE,
    PlotAllGroups = TRUE
  )

## -----------------------------------------------------------------------------
data("sources.data.check")
knitr::kable(sources.data.check, caption = "Table 4.4: Data generating identified points.") %>%
  kable_styling(full_width = F, position = "center")
#

## ----results='hide',message=FALSE, fig.cap="Figure 4.9: Example of a three-dimensional plot showing data from three Jemez obsidian sources."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot3d <-
  ps_3dPlot(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = c("A", "B", "C"),
    AnalyticVars = analyticVars,
    Selections = rbind(analyticVars[1:3], analyticVars[2:4])
  )

## ----results='hide',message=FALSE, fig.cap="Figure 4.10a: Q-Q plots of rubidium and zirconium, including the bivariate Q-Q plot, for the Jemez obsidian source data from source A."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot2dGauss <-
  ps_2dPlotGauss(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "A",
    AnalyticVars = analyticVars,
    variablePair = c("Rb", "Zr"),
    QQPlot = TRUE
  )


## ----results='hide',message=FALSE, fig.cap="Figure 4.10b: Q-Q plots of rubidium and zirconium, including the bivariate Q-Q plot, for the Jemez obsidian source data from source B."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot2dGauss <-
  ps_2dPlotGauss(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "B",
    AnalyticVars = analyticVars,
    variablePair = c("Rb", "Zr")
  )


## ----results='hide',message=FALSE, fig.cap="Figure 4.10c: Q-Q plots of rubidium and niobium for the Jemez obsidian source data from sources A and B."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot2dGauss <-
  ps_2dPlotGauss(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = c("A", "B"),
    AnalyticVars = analyticVars,
    variablePair = c("Rb", "Nb"),
    QQPlot = FALSE
  )


## ----echo=FALSE, message=FALSE------------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
plot2dGauss <-
  ps_2dPlotGauss(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "All",
    AnalyticVars = analyticVars,
    variablePair = c("Rb", "Zr"),
    QQPlot = FALSE,
    scatterplot = FALSE
  )
knitr::kable(plot2dGauss$pvalues, caption = "Table 4.5:  P-values from test statistics for a bivariate Rb/Zr distribution at the Jemez obsidian sources.") %>%
  kable_styling(full_width = F, position = "center")


## ----echo=FALSE, message=FALSE------------------------------------------------
plot2dGauss <-
  ps_2dPlotGauss(
    data = ObsidianSources,
    GroupVar = "Code",
    ID = "ID",
    Groups = "All",
    AnalyticVars = analyticVars,
    variablePair = c("Rb", "Nb"),
    QQPlot = FALSE,
    scatterplot = FALSE
  )
knitr::kable(plot2dGauss$pvalues, caption = "Table 4.6:  Samples sizes and p-values from test statistics for a bivariate Rb/Nb distribution at the Jemez obsidian sources.") %>%
  kable_styling(full_width = F, position = "center")


## ----results='hide',message=FALSE, fig.cap="Figure 5.1: The scree plot from the principal components analysis of the Jemez obsidian source data."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
save_pca <-
  ps_pca(
    data = ObsidianSources,
    ID = "ID",
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    pcPlot = FALSE,
    ScreePlot = TRUE
  )

## ----results='hide',message=FALSE, fig.cap="Figure 5.2: Box plots of the first two principal components from the principal components analysis of the Jemez obsidian source data."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
save_pca <-
  ps_pca(
    data = ObsidianSources,
    ID = "ID",
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    pcPlot = FALSE,
    BoxPlots = TRUE
  )

## ----results='hide',message=FALSE---------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
save_pca <-
  ps_pca(
    data = ObsidianSources,
    ID = "ID",
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars
  ) 

## -----------------------------------------------------------------------------
save_pca <-
  ps_pca(
    data = ObsidianSources,
    ID = "ID",
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    pcPlot = FALSE
  )
knitr::kable(save_pca$variances, caption = "Table 5.1:  Proportions of the variance explained in the principal components analysis of the Jemez obsidian source data.") %>%
  kable_styling(full_width = F, position = "center")
knitr::kable(save_pca$weights, caption = "Table 5.2:  Weights for each principal component in the principal components analysis of the Jemez obsidian source data.") %>%
  kable_styling(full_width = F, position = "center")
knitr::kable(head(save_pca$DataPlusPredicted), caption = "Table 5.3:  Original data and principal components in the principal components analysis of the Jemez obsidian source data.") %>%
  kable_styling(full_width = F, position = "center") %>%
  kable_styling(full_width = F, position = "center")


## ----echo=FALSE, message=FALSE------------------------------------------------
data(ObsidianSources)
analVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
pca_Gauss <-
  ps_pcaGaussian(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    analyticVars = analVars,
    qqPlot = FALSE
  )

knitr::kable(pca_Gauss$p_values, caption = "Table 5.4: P-values from test statistics for a bivariate Gaussian distribution of the first two principal components at each Jemez obsidian source.") %>%
  kable_styling(full_width = F, position = "center")


## ----results='hide',message=FALSE, fig.cap="Figure 6.1: The classification tree for the Jemez obsidian source data based on five potential elements as predictors."----
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotCp = FALSE
  )

## ----results='hide',message=FALSE, fig.cap="Figure 6.2: Plot of the reduction in the crossvalidation error with an increasing number of nodes in the classification tree for the Jemez obsidian source data based on five potential elements as predictors."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotTree = FALSE
  )

## ----message=FALSE, fig.cap="Display 6.1: Full description of the classification tree for the Jemez obsidian source data using five elements as predictors."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotCp = FALSE,
    plotTree = FALSE
  )
tree$Tree


## ---- message=FALSE-----------------------------------------------------------
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotTree = FALSE,
    plotCp = F
  )
knitr::kable(tree$classification, caption = "Table 6.2:  Classification tree predictions for the Jemez obsidian sources using five elements as potential predictors.") %>%
  kable_styling(full_width = F, position = "center")

## ---- message=FALSE-----------------------------------------------------------
library(predictSource)
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotTree = FALSE,
    plotCp = F
  )
knitr::kable(tree$CpTable, caption = "Table 6.3:  Cp table from the classification tree for the Jemez obsidian sources using five elements as potential predictors.") %>%
  kable_styling(full_width = F, position = "center")


## ---- message=FALSE-----------------------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
save_tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = TRUE,
    unknownData = ObsidianArtifacts,
    unknownID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
knitr::kable(save_tree$predictedSource[1:5, ], caption = "Table 6.4: An example of the data frame containing predicted sources of obsidian artifacts from the classification tree for the Jemez obsidian sources using five elements as potential predictors.") %>%
  kable_styling(full_width = F, position = "center")


## ---- message=FALSE, fig.cap="Figure 6.3: Principal components plot of the locations of artifact sources from classification tree model predictions."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
tree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    ID = "ID",
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    plotTree = FALSE,
    plotCp = F,
    predictSources = TRUE,
    unknownData = ObsidianArtifacts
  )
predictedSource <-
  tree$predictedSource[tree$predictedSource[, "source"] != "A", ]
save_pca <-
  ps_pca(
    data = predictedSource,
    ID = "ID",
    GroupVar = "source",
    Groups = "All",
    AnalyticVars = analyticVars,
    PlotEllipses = FALSE
  )


## ----results='hide',message=FALSE, fig.cap="Figure 7.1: Error rates from a random forest analysis of the Jemez obsidian source data."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotImportance = FALSE
  )
 

## ----results='hide',message=FALSE, fig.cap="Figure 7.2: Variable importance for separating the Jemez obsidian sources using a random forest analysis."----
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotErrorRate = FALSE
  )
 

## ----message=FALSE------------------------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotImportance = FALSE,
    plotErrorRate = FALSE
  )
knitr::kable(saveRandomForest$importance, caption = "Table 7.1: Relative variable importance in a random forest model for the Jemez obsidian source data.") %>%
  kable_styling(full_width = F, position = "center")


## ----message=FALSE------------------------------------------------------------
data(ObsidianSources)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotImportance = FALSE,
    plotErrorRate = FALSE
  )
knitr::kable(saveRandomForest$confusion, caption = "Table 7.2: The confusion matrix from a random forest model for the Jemez obsidian source data: accuracy of classification.") %>%
  kable_styling(full_width = F, position = "center")


## ----message=FALSE------------------------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = FALSE
  )
knitr::kable(
  saveRandomForest$predictedSources[1:5, ],
  caption = "Table 7.3: The predicted source and predicted
  source probabilities from a random forest model for the Jemez obsidian artifact data."
) %>%
  kable_styling(full_width = F, position = "center")
knitr::kable(
  saveRandomForest$predictedTotals,
  caption = "Table 7.4: The predicted number of artifacts from
  each Jemez source from a random forest model for the Jemez obsidian artifact data."
) %>%
  kable_styling(full_width = F, position = "center")


## ----results='hide',message=FALSE, fig.cap="Figure 7.3a: Box plots of the estimated probabilities for the predicted sources for the obsidian artifacts.", fig.keep='first'----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = TRUE
  )


## ----results='hide',message=FALSE, fig.cap="Figure 7.3b: Box plots of the estimated probabilities of sources other than the predicted sources for the obsidian artifacts.", fig.keep='last'----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = TRUE
  )


## ----message=FALSE------------------------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    sourceID = "ID",
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = FALSE
  )
check <- saveRandomForest$predictedSources
rows <- ((check[, "C"] > 0.14) & (check[, "C"] < 0.35))
checkC <- check[rows, ]
orderedRows <- order(checkC[, "C"])
knitr::kable(checkC[orderedRows, ], caption = "Table 7.3:  Rows with relative large probabilities of
             misclassification to source C.") %>%
  kable_styling(full_width = F, position = "center")

## ----results='hide', message=FALSE, fig.cap="Figure 7.4: Principal components plot of the predicted sources of the obsidian artifacts from a random forests analysis."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    sourceID = "ID",
    AnalyticVars = analyticVars,
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = FALSE
  )
predictedSources <-
  saveRandomForest$predictedSources[saveRandomForest$predictedSources[, "source"] !=
                                      "A", ]
save_pca <-
  ps_pca(
    data = predictedSources,
    ID = "source",
    GroupVar = "source",
    Groups = "All",
    AnalyticVars = analyticVars,
    PlotEllipses = FALSE
  )


## ----results='hide',message=FALSE, fig.cap="Figure 8.1: Box plots for yttrium and niobium comparing the Jemez obsidian sources and the obsidian artifacts assigned to those sources by a classification tree analysis.  The plot for x.a is the artifacts assigned to source x."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveTree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = TRUE,
    predictUnknowns = TRUE,
    unknownData = ObsidianArtifacts,
    unknownID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
ObsidianSources <- ObsidianSources[, c("Code", analyticVars)]
Artifacts <-
  saveTree$predictedSourceUnknowns[, c("predicted", analyticVars)]
SourcesCode <- as.vector(ObsidianSources[, "Code"], mode = "character")
ArtifactsCode <-
  as.vector(paste(Artifacts[, "predicted"], "a", sep = "."), mode = "character")
Sources <- c(SourcesCode, ArtifactsCode)
SourcesArtifacts <-
  data.frame(rbind(ObsidianSources[, analyticVars], Artifacts[, analyticVars]))
SourcesArtifacts <- data.frame(SourcesArtifacts, Source = Sources)
boxPlots <-
  ps_boxPlots(
    data = SourcesArtifacts,
    GroupVar = "Source",
    Groups = "All",
    AnalyticVars = c("Y", "Nb"),
    Nrow = 2,
    Ncol = 1
  )


## ----results='hide', message=FALSE, fig.keep='first', fig.cap="Figure 8.2a: Principal components plots of the Jemez obsidian source convex hulls with the source points (left panel) and obsidian artifact points (right panel).  The artifacts are labeled with their predicted source from a classification tree analysis."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveTree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = TRUE,
    unknownData = ObsidianArtifacts,
    ID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
pcaEval <-
  ps_pcaEvaluation(
    SourceData = ObsidianSources,
    unknownData = saveTree$predictedSource,
    SourceGroup = "Code",
    unknownGroup = "predicted",
    known_sources = sources,
    predicted_sources = sources,
    AnalyticVars = analyticVars,
    plotAllPoints = TRUE,
    plotHullsOutsidePoints = TRUE,
    plotOutsidePoints = FALSE
  )


## ----results='hide', message=FALSE, fig.keep='last', fig.cap="Figure 8.2b: Principal components plots of the Jemez obsidian source convex hulls with the source point medians (left panel) and obsidian artifact points outside the predicted source hulls (right panel).  The artifacts are labeled with their predicted source from a classification tree analysis."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveTree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = TRUE,
    unknownData = ObsidianArtifacts,
    ID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
pcaEval <-
  ps_pcaEvaluation(
    SourceData = ObsidianSources,
    unknownData = saveTree$predictedSource,
    SourceGroup = "Code",
    unknownGroup = "predicted",
    known_sources = sources,
    predicted_sources = sources,
    AnalyticVars = analyticVars,
    plotAllPoints = TRUE,
    plotHullsOutsidePoints = TRUE,
    plotOutsidePoints = FALSE
  )


## ----results='hide', message=FALSE, fig.keep='last', fig.cap="Figure 8.2c: Principal components plot with Jemez obsidian source convex hulls and obsidian artifacts with points outside the convex hull predicted from a classification tree model."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveTree <-
  ps_tree(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    Model = "Sr"+"Nb"+"Rb"+"Y"+"Zr",
    ModelTitle = "Sr + Nb + Rb + Y + Zr",
    predictSources = TRUE,
    unknownData = ObsidianArtifacts,
    ID = "ID",
    plotTree = FALSE,
    plotCp = FALSE
  )
pcaEval <-
  ps_pcaEvaluation(
    SourceData = ObsidianSources,
    unknownData = saveTree$predictedSource,
    SourceGroup = "Code",
    unknownGroup = "predicted",
    known_sources = sources,
    predicted_sources = sources,
    AnalyticVars = analyticVars,
    ID = "ID",
    plotAllPoints = TRUE,
    plotHullsOutsidePoints = TRUE,
    plotOutsidePoints = TRUE
  )


## ----message=FALSE------------------------------------------------------------
data(tree.data.check)
knitr::kable(tree.data.check, caption = "Table 8.1: Artifacts which may have misidentified sources identified by using Identify=TRUE in ps_pcaEvaluation() after fitting a tree model.  The variable group is the predicted source from the model.") %>%
  kable_styling(full_width = F, position = "center")


## ----results='hide', message=FALSE, fig.keep='last', fig.cap="Figure 8.3: Principal components plot of the Jemez obsidian source convex hulls and sources outside of those hulls using predictions from a random forests analysis."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    sourceID = "ID",
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    plotSourceProbs = FALSE
  )
pcaEval <- ps_pcaEvaluation(
  SourceData = ObsidianSources,
  unknownData = saveRandomForest$predictedSources,
  SourceGroup = "Code",
  unknownGroup = "source",
  known_sources = sources,
  predicted_sources = sources,
  AnalyticVars = analyticVars,
  plotAllPoints = TRUE,
  plotHullsOutsidePoints = TRUE,
  plotOutsidePoints = TRUE
)


## ----results='hide', message=FALSE, fig.keep='last', fig.cap="Figure 8.4: Principal components plot with Jemez obsidian source convex hulls and obsidian artifacts with points outside the convex hull labeled with Steve Shackley's source predictions."----
data(ObsidianSources)
data(ObsidianArtifacts)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
pcaEval <-
  ps_pcaEvaluation(
    SourceData = ObsidianSources,
    unknownData = ObsidianArtifacts,
    SourceGroup = "Code",
    unknownGroup = "Code",
    known_sources = sources,
    predicted_sources = sources,
    AnalyticVars = analyticVars,
    ID = "ID",
    plotAllPoints = TRUE,
    plotHullsOutsidePoints = TRUE,
    plotOutsidePoints = TRUE
  )


## ----message=FALSE------------------------------------------------------------
data(ObsidianSources)
data(ObsidianArtifacts)
data(tree.data.check)
analyticVars <- c("Rb", "Sr", "Y", "Zr", "Nb")
sources <- unique(ObsidianSources[, "Code"])
saveRandomForest <-
  ps_randomForest(
    data = ObsidianSources,
    GroupVar = "Code",
    Groups = "All",
    AnalyticVars = analyticVars,
    sourceID = "ID",
    NvarUsed = 3,
    plotErrorRate = FALSE,
    plotImportance = FALSE,
    predictSources = TRUE,
    predictData = ObsidianArtifacts,
    unknownID = "ID",
    plotSourceProbs = FALSE
  )
treeDataCheck <- tree.data.check[order(tree.data.check[, "ID"]), ]
saveRF <-
  saveRandomForest$predictedSources[order(saveRandomForest$predictedSources[, "source"]), ]
obsCheck <- merge(
  x = saveRF,
  y = tree.data.check[, c("ID", "pc1", "pc2")],
  by = "ID",
  x.all = FALSE,
  y.all = FALSE
)
knitr::kable(obsCheck, caption = "Table 8.2: Random forest probability estimates for artifacts which may have  been misidentified as from source C using the tree model identified by using Identify=TRUE in ps_pcaEvaluation().  source is the predicted source from the random forest procedure.") %>%
  kable_styling(full_width = F, position = "center")


