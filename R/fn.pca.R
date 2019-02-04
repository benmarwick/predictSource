#'  fn.pca
#'
#'  Compute and plot principal components after standardizing the data
#'
#' @param doc A string with documentation in the list returned, default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param ID  An optional name for an ID, default is " " if no ID
#' @param GroupVar The name for variable defining grouping; a variable name is required
#' @param Groups Character-valued defining the values of the group variable for which plots are
#'  to be done.  Options are a vector of values; "All" (use all groups).  One of these is required
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param ScreePlot  Logical, if T create a scree plot, default is F
#' @param BoxPlots  Logical, if T, create box plots of the first two components, default is F
#' @param pcPlot  Logical, if T (the default), create the plot of the first two components
#' @param PlotPoints  Logical, if T (the default) and pcPlot=T, plot the points for the first two components
#' @param PlotEllipses Logical, if T (the default), plot the confidence ellipse or ellipses for each group
#' @param PlotHull  Logical, if T, plot the convex hull for each group, default is F
#' @param PlotMedians  Logical, if T, plot the symbol for each group at the median point for that group, default is F
#' @param Ellipses A value or vector of proportions for confidence ellipses; default is c(.95,.99) to produce 95\% and 99\% confidence ellipses
#' @param legendLoc Character, location of legend for a plot with points;
#'  default is "topright", alternatives are combinations of "top", "bottom", "right", "left"
#' @param PlotColors Logical.  If T, use list of colors in Colors for points; if F, plot points as black
#' @param Colors A vector of color names; default is a vector with five names
#' @param Identify Logical.  If T, the user can identify points of interest in plots; information on these points is saved to a file; default is F
#' @param digits The number of significant digits to return in objects in data frames, default is 3
#' @param Seed  If not NA, the seed for the random number generator used if missing data are
#' imputed; default is 11111
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section Details:
#' If Identify=T, the user must interact with each plot (or pane, if there is more than one pane on a plot).
#' To identify a point, place the cursor as close as possible to the point and left click;  repeat if desired.
#' To go to the next pane, right click and select "Stop" in base R; click on "Finish" in the plot pane in Rstudio.
#'
#' @return The function produces a plot of the first two principal components, the contents of which are defined by the arguments PlotPoints, PlotEllipses, PlotHull, and PlotMedians. A scree plot and box plots are produced if requested.  The function returns a list with the following components:
#' \itemize{
#'   \item{usage:}{  A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{  A list with the values of the arguments for grouping, logical parameters,
#'   Ellipses, and Colors}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{ellipse.pct:}{  The value of the argument Ellipses}
#'   \item{variances:}{  A data frame including the percent of variation explained by each
#'   principal component and the cumulative percent explained}
#'   \item{weights:}{  A data frame with the principal component weights for each observation}
#'   \item{Predicted:}{  A data frame with the predicted values for each principal component,
#'    plus the value of Groups and an integer GroupIndex (with values 1:number of Groups)}
#'   \item{DataPlusPredicted:}{  A data frame with the data used to compute the principal components,
#'    plus GroupIndex (as defined above) and predicted values for each principal component}
#'   \item{data.check:}{  If Identify=T, a data frame with the observations in dataUsed
#'    identified as of interest}
#'   \item{location:}{  The value of the parameter folder}
#'  }
#'
#' @import  MASS ellipse  randomForest
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.pca <- fn.pca(data=ObsidianSources, ID="ID", GroupVar="Code",Groups="All", AnalyticVars=analyticVars)
#'
#' @export
#'
fn.pca <-  function(doc = "fn.pca",
                    data,
                    ID=" ",
                    GroupVar,
                    Groups,
                    AnalyticVars,
                    ScreePlot = F,
                    BoxPlots = F,
                    pcPlot = T,
                    PlotPoints = T,
                    PlotEllipses = T,
                    PlotHull = F,
                    PlotMedians = F,
                    Ellipses = c(.95, .99),
                    PlotColors = T,
                    legendLoc="topright",
                    Colors = c("red","black","blue","green","purple"),
                    Identify = F,
                    digits=3,
                    Seed=11111,
                    folder = " ")
{
   #
  #  define functions to plot convex hulls and ellipses
    fn.convexhull <- function(Code) {
      temp <- Predicted[Predicted[, "group"] == Code, c("group",
                                                        "PC1", "PC2")]
      hull.pts <- chull(temp[, c("PC1", "PC2")])
      hull.pts <- c(hull.pts, hull.pts[1])
      lines(temp[hull.pts, c("PC1", "PC2")])
    }
    #
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
    #  restrict data if specified
    #
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows,]
    }
    else  data.Used <- data[, ]
    #
    dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with no missing data
    for (i in 1:nrow(data.Used)) {
      for (j in 1:length(AnalyticVars))
        if (is.na(data.Used[,AnalyticVars][i,j]))  dataKeep[i] <- F
    }
    #
    #  redefine data.Keep if some analysis variables are missing by imputing missing values
    #
    if (sum(dataKeep) < nrow(data.Used)) {
      if (!na(Seed))  set.seed(Seed)
      dataNA <- data.Used[!dataKeep,]
      temp<-rfImpute(data.Used[,GroupVar] ~ ., data.Used[,AnalyticVars])
      if (ID == " ") data.Used <- data.frame(data.Used[,GroupVar],temp)
      else  data.Used <- data.frame(data.Used[,c(GroupVar, ID)],temp)
    }
    else dataNA <- NA
    #
    #  sort on GroupVar and ID within GroupVar if specified
    #
    data.Used <- data.Used[order(data.Used[,GroupVar]),]
    if (ID[1] != " ")
      data.Used <- data.Used[order(data.Used[,GroupVar],data.Used[,ID]),]
    #
    #  define vector with unique group IDs
    #
    if (Groups[1] == "All")
      groups <- as.character(unique(data.Used[, GroupVar]))
    else   groups <- as.character(Groups)
    #
    if ((PlotColors) & (length(groups)>length(Colors)))  stop("number of colors smaller than number of groups")
    #
    #  principal components analysis
    #
    pca <- prcomp(data.Used[, AnalyticVars], scale = TRUE)
    if (ScreePlot == T) {
      plot(pca, main = "Scree plot", xlab = "Principal component")
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
           ylab = "Component 2", main = "Principal component plot for all groups combined")
      Predicted <- predict(pca)
      browser()
    }
    #
    #  define group index for plotting symbols and colors
    #
    GroupIndex <- rep(NA, nrow(data.Used))
      for (i in 1:nrow(data.Used)) {
        for (j in 1:length(groups)) if (data.Used[i, GroupVar] ==
                                        groups[j])
          GroupIndex[i] <- j
      }
    #
    Predicted <- data.frame(group = as.character(data.Used[,
                                 GroupVar]), GroupIndex = GroupIndex, predict(pca))
      #
      if (Identify == F) data.check <- c(NA, NA) #  dummy value if no points identified
      #
      if (BoxPlots == T) {
        par(mfrow = c(1, 2))
        plot(Predicted[, c("group", "PC1")], notch = T, main = "Box plots by group: first PC")
        plot(Predicted[, c("group", "PC2")], notch = T, main = "Box plots by group: second PC")
        browser()
      }
      if (pcPlot == T) {
      par(mfrow=c(1,1))
      #  set up plot with axis ranges
      if (PlotEllipses == F)
        plot(x = range(Predicted[, "PC1"]), y = range(Predicted[,"PC2"]), type = "n", xlab = "first PC",
             ylab = "second PC", main = "Principal components plot")
      if (PlotEllipses == T) {
        subtext <- paste("Ellipsoid content:", Ellipses[1])
        if (length(Ellipses) > 1) {
          for (i in 2:length(Ellipses))
            subtext<-paste(subtext,"; ",Ellipses[i],sep="")
        }
        plot(x = c(min(Predicted[, "PC1"], na.rm = T) - 1, max(Predicted[, "PC1"], na.rm = T) + 1),
             y = c(min(Predicted[,"PC2"], na.rm = T) - 1, max(Predicted[, "PC2"], na.rm = T) + 1),
             type = "n", xlab = "first PC", ylab = "second PC", main = "Principal components plot",
             sub=subtext)
      }  # end of code for PlotEllipses=T
      #
      if (PlotPoints == T) {
        if (PlotColors == T)
          points(x = Predicted[, "PC1"], y = Predicted[,
                                                       "PC2"], pch = (Predicted[, "GroupIndex"] -
                                                                        1), col = Colors[Predicted[, "GroupIndex"]])
        else points(x = Predicted[, "PC1"], y = Predicted[,
                                            "PC2"], pch = (Predicted[, "GroupIndex"] - 1))
        }  # end of code for PlotPoints=T
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
      }  # end of code for PlotMedians=F
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
      } # end of code for PlotMedians=T
        if ((Identify==T) & ((PlotPoints==T) | (PlotMedians==T)))  {
          index<-identify(x = Predicted[,"PC1"], y = Predicted[,"PC2"])
          data.check<-data.Used[index,]
        }
    }
    DataPlusPredicted <- data.frame(data.Used, Predicted[,-1])
    #

    Predicted <- Predicted[,-2]  # remove GroupIndex
    if (ID != " ") DataPlusPredicted <- DataPlusPredicted[,-(3+length(analyticVars))]
      else  DataPlusPredicted <- DataPlusPredicted[,-(2+length(analyticVars))]#  remove GroupIndex
    pcNames <- colnames(Predicted)[-1]
    Predicted[,pcNames] <- round(Predicted[,pcNames], dig = digits)
    DataPlusPredicted[,pcNames] <- round(DataPlusPredicted[,pcNames], dig = digits)
    #
    weights <- round(weights,dig = digits)
    variances <- round(importance.pca, dig = digits)
    #
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep,]
    else dataNA <- NA
    #
    if (Identify==T) {
      if (ID != " ") data.check<-data.check[order(data.check[,"Code"],data.check[,"ID"]),]
      else  data.check<-data.check[order(data.check[,"Code"]),][,c("Code",AnalyticVars)]
    }
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    #
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(ScreePlot,BoxPlots,PlotPoints,PlotEllipses,PlotHull,PlotMedians,PlotColors)
    names(params.logical)<-c("ScreePlot","BoxPlots","PlotPoints","PlotEllipses","PlotHull","PlotMedians","PlotColors")
    params<-list(grouping=params.grouping,logical=params.logical,ellipses=Ellipses,Seed=Seed,
                 colors=Colors)
    #
    out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params=params,
                variances = variances,
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted,
                data.check=data.check,
                location=folder)
    out
  }
