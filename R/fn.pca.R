#'  fn.pca
#'
#'  Compute principal components using standardized data
#'
#' @param doc: documentation in the list returned, default is the function name
#' @param data: R matrix or data frame containing the data to be analyzed
#' @param GroupVar: name for variable defining grouping; if " ", no grouping
#' @param Groups: vector of values of group variable for which plots are to be done. if "All": use all groups; if " ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Ellipses: value or vector of proportions for confidence ellipses default is c(.95,.99) to produce 95\% and 99\% confidence ellipses
#' @param legendLoc: character, location of legend for a plot with points default is "topright", alternatives are combinations of "top", "bottom", "right", "left"
#' @param PlotColors: if T, use list of colors in Colors for points; if F, plot points as black
#' @param Colors: vector of color names
#' @param Identify: if T, the user can identify points of interest in plots; information on these points is saved to the file; default is F
#' @param folder: folder in which excel files are to be stored
#' @param ds.weights: excel file with principal component loadings, extension.csv
#' @param ds.importance: excel file with percent of variation explained, extension.csv
#'
#' @section Details:
#' If Identify=T, the user must interact with each plot (or pane, if there is more than one pane on a plot).  To identify a point, place the cursor as close as possible to the point and left click;  repeat if desired.  To go to the next pane, right click and select "Stop" in base R; click on "Finish" in the plot pane in Rstudio.
#'
#' @return The function produces a plot of the first two principal components, the contents of which are defined by the arguments PlotPoints, PlotEllipses, PlotHull, and PlotMedians. A scree plot and box plots are produced if requested.  The function returns a list with the following components:
#' \itemize{
#'   \item{"usage"}{a vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{"dataUsed"}{the contents of the argument data restricted to the groups used}
#'   \item{"params.grouping"}{a list with the values of the arguments GroupVar and Groups}
#'   \item{"x"}{params.logical: a vector with the values of the arguments ScreePlot,BoxPlots,PlotPoints,PlotEllipses,PlotHull,PlotMedians,PlotColors}
#'   \item{"analyticVars"}{a vector with the value of the argument AnalyticVars}
#'   \item{"ellipse.pct"}{the value of the argument Ellipses}
#'   \item{"Summary"}{a list including the percent of variation explained by each principal component and the cumulative percent explained}
#'   \item{"weights"}{a data frame with the principal component weights for each observation}
#'   \item{"Predicted"}{Predicted}
#'   \item{"DataPlusPredicted"}{DataPlusPredicted}
#'   \item{"data.check"}{if Identify=T, a data frame with the observations in dataUsed identified as of interest}
#'   \item{"files"}{if folder != " ", a list with path and data set names to the excel files containing weights, importance, and, if Identify=T, data for the points of interest}
#'  }
#'
#' @import  MASS ellipse
#'#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.pca <- fn.pca(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars)
#'
#' @export
#'
fn.pca <-  function(doc = "fn.pca", data, GroupVar, Groups, AnalyticVars, ScreePlot = F, BoxPlots = F, PlotPoints = T,
                    PlotEllipses = T, legendLoc="topright", PlotHull = F, PlotMedians = F, Ellipses = c(.95, .99),
                    PlotColors = T, Colors = c("red","black","blue","green","purple"), Identify = F,
                    folder = " ", ds.weights, ds.importance) {
   #
  #  define functions to plot convex hulls and ellipses
    fn.convexhull <- function(Code) {
      temp <- Predicted[Predicted[, "group"] == Code, c("group",
                                                        "PC1", "PC2")]
      hull.pts <- chull(temp[, c("PC1", "PC2")])
      hull.pts <- c(hull.pts, hull.pts[1])
      lines(temp[hull.pts, c("PC1", "PC2")])
    }
    fn.ellipses <- function() {
      for (i in 1:length(groups)) {
        temp <- Predicted[Predicted[, "group"] == groups[i],
                          c("PC1", "PC2")]
        Covar <- var(temp)
        for (j in 1:length(Ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(temp,
                                                       2, mean, na.rm = T), level = Ellipses[j], npoints = 200)
          lines(Ellipse)
        }
      }
    }
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows,]
    }
    else  data.Used <- data[, ]
    #
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    if ((PlotColors) & (length(groups)>length(Colors)))  stop("number of colors smaller than number of groups")
    #
    pca <- prcomp(data.Used[, AnalyticVars], scale = TRUE)
    if (ScreePlot == T) {
      plot(pca, main = " ", xlab = "Principal component")
      browser()
    }
    importance.pca <- summary(pca)$importance
    #
    weights <- matrix(NA, length(AnalyticVars), length(AnalyticVars))
    dimnames(weights) <- list(paste("pc", 1:length(AnalyticVars),
                                    sep = "."), AnalyticVars)
    for (i in 1:length(AnalyticVars)) {
      weights[i, ] <- pca$rotation[, i]
    }
    #
    predict.pc1 <- predict(pca)[, 1]
    predict.pc2 <- predict(pca)[, 2]
    if (GroupVar[1] == " ") {
      plot(predict.pc1, predict.pc2, xlab = "Component 1",
           ylab = "Component 2")
      Predicted <- predict(pca)
      browser()
    }
    if (GroupVar[1] != " ") {
      GroupIndex <- rep(NA, nrow(data.Used))
      for (i in 1:nrow(data.Used)) {
        for (j in 1:length(groups)) if (data.Used[i, GroupVar] ==
                                        groups[j])
          GroupIndex[i] <- j
      }
      Predicted <- data.frame(group = as.character(data.Used[,
                                                             GroupVar]), GroupIndex = GroupIndex, predict(pca))
      if (BoxPlots == T) {
        par(mfrow = c(1, 2))
        plot(Predicted[, c("group", "PC1")], notch = T, main = "first PC")
        plot(Predicted[, c("group", "PC2")], notch = T, main = "second PC")
        browser()
      }
      plot.new()
      par(mfrow=c(1,1))
      if (PlotEllipses == F)
        plot(x = range(Predicted[, "PC1"]), y = range(Predicted[,
                                                                "PC2"]), type = "n", xlab = "first PC", ylab = "second PC")
      if (PlotEllipses == T)
        plot(x = c(min(Predicted[, "PC1"], na.rm = T) - 1,
                   max(Predicted[, "PC1"], na.rm = T) + 1), y = c(min(Predicted[,
                                                                                "PC2"], na.rm = T) - 1, max(Predicted[, "PC2"],
                                                                                                            na.rm = T) + 1), type = "n", xlab = "first PC",
             ylab = "second PC")
      if (PlotPoints == T) {
        if (PlotColors == T)
          points(x = Predicted[, "PC1"], y = Predicted[,
                                                       "PC2"], pch = (Predicted[, "GroupIndex"] -
                                                                        1), col = Colors[Predicted[, "GroupIndex"]])
        else points(x = Predicted[, "PC1"], y = Predicted[,
                                                          "PC2"], pch = (Predicted[, "GroupIndex"] - 1))
        if (Identify==T)  {
          index<-identify(x = Predicted[,"PC1"], y = Predicted[,"PC2"])
          data.check<-data.Used[index,]
        }
      }
      if (PlotHull == T) {
        for (i in 1:length(groups)) fn.convexhull(Code = groups[i])
      }
      if (PlotEllipses == T)
        fn.ellipses()
      if (PlotMedians == F) {
        if (PlotColors == T)
          legend(x = legendLoc, legend = groups, pch = 0:(length(groups) -
                                                            1), col = Colors, bty = "n")
        else legend(x = legendLoc, legend = groups, pch = 0:(length(groups) -
                                                               1), bty = "n")
      }
      if (PlotMedians == T) {
        medians <- matrix(NA, nrow = length(groups), ncol = 2)
        for (i in 1:length(groups)) {
          temp <- Predicted[Predicted[, "group"] == groups[i],
                            c("PC1", "PC2")]
          medians[i, 1] <- median(temp[, 1], na.rm = T)
          medians[i, 2] <- median(temp[, 2], na.rm = T)
        }
        medians <- cbind(1:length(groups), medians)
        colnames(medians) <- c("group", "PC1", "PC2")
        text(x = medians[, "PC1"], y = medians[, "PC2"],
             labels = groups, cex = 0.75, adj = 0.5)
      }
    }
    if (GroupVar[1] == " ")
      DataPlusPredicted <- data.frame(data.Used, Predicted)
    else DataPlusPredicted <- data.frame(data.Used, Predicted[,-1])
    #
    if (substr(folder,1,1) != " ")  {
      write.csv(importance.pca, file = paste(folder, ds.importance, sep=""))
      write.csv(t(weights), file = paste(folder, ds.weights, sep = ""))
      write.csv(data.check, file = paste(folder, ds.identified, sep = ""))
    }
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(ScreePlot,BoxPlots,PlotPoints,PlotEllipses,PlotHull,PlotMedians,PlotColors)
    names(params.logical)<-c("ScreePlot","BoxPlots","PlotPoints","PlotEllipses","PlotHull","PlotMedians","PlotColors")
    #
    if ((substr(folder,1,1) == " ") & (!Identify))
      out<-list(usage=fcn.date.ver,
                dataUsed = data.Used,
                analyticVars = AnalyticVars,
                params.logical = params.logical,
                params.grouping = params.grouping,
                ellipse.pct = Ellipses,
                Summary = summary(pca),
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted)
    if ((substr(folder,1,1) != " ") & (!Identify))
      out<-list(usage = fcn.date.ver,
                dataUsed = data.Used,
                analyticVars = AnalyticVars,
                params.logical = params.logical,
                params.grouping = params.grouping,
                ellipse.pct = Ellipses,
                Summary = summary(pca),
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted,
                files=list(paste(folder,ds.weights,sep=""),paste(folder,ds.importance,sep="")))
    if ((substr(folder,1,1) == " ") & (Identify))
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                ellipse.pct=Ellipses,
                Summary = summary(pca),
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted,
                data.check=data.check)
    if ((substr(folder,1,1) != " ") & (Identify))
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                ellipse.pct=Ellipses,
                Summary = summary(pca),
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted,
                data.check=data.check,
                files=list(paste(folder,ds.identified,sep=""),paste(folder,ds.weights,sep=""),
                                 paste(folder,ds.importance,sep="")))
    out
  }
