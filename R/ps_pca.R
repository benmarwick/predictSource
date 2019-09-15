#'  ps_pca
#'
#'  Compute and plot principal components after standardizing the data
#'
#' @param doc A string with documentation in the list returned, default
#' is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param ID  An optional name for an ID, default is " " if no ID
#' @param GroupVar The name for variable defining grouping; a variable
#' name is required
#' @param Groups Character-valued defining the values of the group variable
#'  for which plots are to be done.  Options are a vector of values;
#'   "All" (use all groups).  One of these is required
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param ScreePlot  Logical, if TRUE create a scree plot, default is FALSE
#' @param BoxPlots  Logical, if TRUE, create box plots of the first two components,
#'  default is FALSE
#' @param pcPlot  Logical, if TRUE (the default), create the plot of the first
#' two components
#' @param PlotPoints  Logical, if TRUE (the default) and pcPlot=TRUE,
#' plot the points for the first two components
#' @param PlotEllipses Logical, if TRUE (the default), plot the confidence ellipse
#'  or ellipses for each group
#' @param PlotHull  Logical, if TRUE, plot the convex hull for each group,
#'  default is FALSE
#' @param PlotMedians  Logical, if TRUE, plot the symbol for each group at the
#' median point for that group, default is FALSE
#' @param Ellipses A value or vector of proportions for confidence ellipses;
#'  default is c(.95,.99) to produce 95\% and 99\% confidence ellipses
#' @param legendLoc Character, location of legend for a plot with points;
#'  default is "topright", alternatives are combinations of "top", "bottom",
#'   "right", "left"
#' @param PlotColors Logical.  If TRUE, use list of colors in Colors for points;
#'  if F, plot points as black
#' @param Colors A vector of color names; default is a vector with five names
#' @param Identify Logical.  If TRUE, the user can identify points of interest in plots;
#'  information on these points is saved to a file; default is FALSE
#' @param digits The number of significant digits to return in objects in data frames,
#'  default is 3
#' @param Seed  If not NA, the seed for the random number generator used if
#'  missing data are imputed; default is 11111
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section Details:
#' If Identify=TRUE, the user must interact with each plot (or pane,
#'  if there is more than one pane on a plot). To identify a point,
#'  place the cursor as close as possible to the point and left click;  repeat if desired.
#' To go to the next pane, right click and select "Stop" in base R;
#' click on "Finish" in the plot pane in Rstudio.
#'
#' @return The function produces a plot of the first two principal components,
#' the contents of which are defined by the arguments PlotPoints, PlotEllipses, PlotHull,
#'  and PlotMedians. A scree plot and box plots are produced if requested.
#'  The function returns a list with the following components:
#' \itemize{
#'   \item{usage:}{  A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{  A list with the values of the arguments for grouping, logical parameters,
#'   Ellipses, and Colors}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{ellipse_pct:}{  The value of the argument Ellipses}
#'   \item{variances:}{  A data frame including the percent of variation explained by each
#'   principal component and the cumulative percent explained}
#'   \item{weights:}{  A data frame with the principal component weights for each observation}
#'   \item{Predicted:}{  A data frame with the predicted values for each principal component,
#'    plus the value of Groups and an integer GroupIndex (with values 1:number of Groups)}
#'   \item{DataPlusPredicted:}{  A data frame with the data used to compute the principal components,
#'    plus GroupIndex (as defined above) and predicted values for each principal component}
#'   \item{dataCheck:}{  If Identify=TRUE, a data frame with the observations in dataUsed
#'    identified as of interest}
#'   \item{location:}{  The value of the parameter folder}
#'  }
#'
#' @import  MASS ellipse  randomForest graphics stats assertthat
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save_pca <- ps_pca(data=ObsidianSources, ID="ID", GroupVar="Code",
#' Groups="All", AnalyticVars=analyticVars)
#'
#' @export
#'
ps_pca <-  function(doc = "ps_pca",
                    data,
                    ID=" ",
                    GroupVar,
                    Groups,
                    AnalyticVars,
                    ScreePlot = FALSE,
                    BoxPlots = FALSE,
                    pcPlot = TRUE,
                    PlotPoints = TRUE,
                    PlotEllipses = TRUE,
                    PlotHull = FALSE,
                    PlotMedians = FALSE,
                    Ellipses = c(.95, .99),
                    PlotColors = T,
                    legendLoc="topright",
                    Colors = c("red","black","blue","green","purple"),
                    Identify = FALSE,
                    digits=3,
                    Seed=11111,
                    folder = " ")
{
  #
  #  check for valid parameters
  #
  assert_that(is.data.frame(data), msg="parameter data not a data frame")
  assert_that(is.character(GroupVar), msg="parameter GroupVar not character")
  assert_that(is.character(Groups), msg="parameter Groups not character")
  assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
              msg="parameter AnalyticVars not a character vector")
  assert_that(is.character(ID), msg="parameter ID not a character name")
  assert_that(is.logical(ScreePlot), msg="type of parameter ScreePlot not logical")
  assert_that(is.logical(BoxPlots), msg="type of parameter BoxPlots not logical")
  assert_that(is.logical(pcPlot), msg="type of parameter pcPlot not logical")
  assert_that(is.logical(PlotPoints), msg="type of parameter PlotPoints not logical")
  assert_that(is.logical(PlotMedians), msg="type of parameter PlotMedians not logical")
  assert_that(is.logical(PlotEllipses), msg="type of parameter PlotEllipses not logical")
  assert_that(is.numeric(Ellipses), msg="parameter Ellipses not numeric")
  assert_that((min(Ellipses) > 0) & (max(Ellipses) < 1), msg="values of parameter Ellipses not between 0 and 1")
  assert_that(is.logical(PlotHull), msg="type of parameter PlotHull not logical")
  assert_that(is.logical(PlotColors), msg="type of parameter PlotColors not logical")
  assert_that((round(digits,0)==digits)&(digits>= 1), msg="parameter digits is not a positive integer")
  assert_that(is.na(Seed) | ((round(Seed,0)==Seed)&(Seed>= 1)),
              msg="parameter Seed is not a positive integer or is not NA")
  assert_that(is.character(Colors), msg="parameter Colors not character")
  assert_that(is.character(legendLoc), msg="parameter legendLoc not character")
  assert_that(is.logical(Identify), msg="type of parameter Identify is not logical")
   #
  #  define functions to plot convex hulls and ellipses
    fnConvexHull <- function(Code) {
      temp <- Predicted[Predicted[, "group"] == Code, c("group",
                                                        "PC1", "PC2")]
      hull_pts <- chull(temp[, c("PC1", "PC2")])
      hull_pts <- c(hull_pts, hull_pts[1])
      lines(temp[hull_pts, c("PC1", "PC2")])
    }
    #
    fnEllipses <- function() {
      for (i in 1:length(groups)) {
        temp <- Predicted[Predicted[, "group"] == groups[i],
                          c("PC1", "PC2")]
        Covar <- var(temp)
        for (j in 1:length(Ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(temp,
                                                       2, mean, na_rm = T), level = Ellipses[j], npoints = 200)
          lines(Ellipse)
        }
      }
    }
    #
    #  restrict data if specified
    #
    if (Groups[1] != "All") {
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows,]
    }
    else  dataUsed <- data[, ]
    #
    dataKeep <- rep(T, nrow(dataUsed)) # will contain indices for observations with no missing data
    for (i in 1:nrow(dataUsed)) {
      for (j in 1:length(AnalyticVars))
        if (is.na(dataUsed[,AnalyticVars][i,j]))  dataKeep[i] <- F
    }
    #
    #  redefine data_Keep if some analysis variables are missing by imputing missing values
    #
    if (sum(dataKeep) < nrow(dataUsed)) {
      if (!is.na(Seed))  set.seed(Seed)
      dataNA <- dataUsed[!dataKeep,]
      temp<-rfImpute(dataUsed[,GroupVar] ~ ., dataUsed[,AnalyticVars])
      if (ID == " ") dataUsed <- data.frame(dataUsed[,GroupVar],temp)
      else  dataUsed <- data.frame(dataUsed[,c(GroupVar, ID)],temp)
    }
    else dataNA <- NA
    #
    #  sort on GroupVar and ID within GroupVar if specified
    #
    dataUsed <- dataUsed[order(dataUsed[,GroupVar]),]
    if (ID[1] != " ")
      dataUsed <- dataUsed[order(dataUsed[,GroupVar],dataUsed[,ID]),]
    #
    #  define vector with unique group IDs
    #
    if (Groups[1] == "All")
      groups <- as.character(unique(dataUsed[, GroupVar]))
    else   groups <- as.character(Groups)
    #
    if ((PlotColors) & (length(groups)>length(Colors)))  stop("number of colors smaller than number of groups")
    #
    #  principal components analysis
    #
    pca <- prcomp(dataUsed[, AnalyticVars], scale = TRUE)
    if (ScreePlot) {  # TRUE
      plot(pca, main = "Scree plot", xlab = "Principal component")
 #     browser()
    }
    importance_pca <- summary(pca)$importance
    #
    weights <- matrix(NA, length(AnalyticVars), length(AnalyticVars))
    dimnames(weights) <- list(paste("pc", 1:length(AnalyticVars),
                                    sep = "_"), AnalyticVars)
    for (i in 1:length(AnalyticVars)) {
      weights[i, ] <- pca$rotation[, i]
    }
    #
    predict_pc1 <- predict(pca)[, 1]
    predict_pc2 <- predict(pca)[, 2]
    #
    if (GroupVar[1] == " ") {
      plot(predict_pc1, predict_pc2, xlab = "first PC",
           ylab = "second PC", main = "Principal component plot for all groups combined")
      Predicted <- predict(pca)
#      browser()
    }
    #
    #  define group index for plotting symbols and colors
    #
    GroupIndex <- rep(NA, nrow(dataUsed))
      for (i in 1:nrow(dataUsed)) {
        for (j in 1:length(groups)) if (dataUsed[i, GroupVar] ==
                                        groups[j])
          GroupIndex[i] <- j
      }
    #
    Predicted <- data.frame(group = as.character(dataUsed[,
                                 GroupVar]), GroupIndex = GroupIndex, predict(pca))
      #
      if (Identify == F) dataCheck <- c(NA, NA) #  dummy value if no points identified
      #
      if (BoxPlots) {  # TRUE
        par(mfrow = c(1, 2))
        plot(Predicted[, c("group", "PC1")], notch = T, main = "Box plots by group: first PC")
        plot(Predicted[, c("group", "PC2")], notch = T, main = "Box plots by group: second PC")
 #       browser()
      }
      if (pcPlot) {  #  TRUE
      par(mfrow=c(1,1))
      #  set up plot with axis ranges
      if (!PlotEllipses)  #  FALSE
        plot(x = range(Predicted[, "PC1"]), y = range(Predicted[,"PC2"]), type = "n", xlab = "first PC",
             ylab = "second PC", main = "Principal components plot")
      if (PlotEllipses) {  #  TRUE
        subtext <- paste("Ellipsoid content:", Ellipses[1])
        if (length(Ellipses) > 1) {
          for (i in 2:length(Ellipses))
            subtext<-paste(subtext,"; ",Ellipses[i],sep="")
        }
        plot(x = c(min(Predicted[, "PC1"], na_rm = T) - 1, max(Predicted[, "PC1"], na_rm = T) + 1),
             y = c(min(Predicted[,"PC2"], na_rm = T) - 1, max(Predicted[, "PC2"], na_rm = T) + 1),
             type = "n", xlab = "first PC", ylab = "second PC", main = "Principal components plot",
             sub=subtext)
      }  # end of code for PlotEllipses=T
      #
      if (PlotPoints) {   #  TRUE
        if (PlotColors)
          points(x = Predicted[, "PC1"], y = Predicted[,
                                                       "PC2"], pch = (Predicted[, "GroupIndex"] -
                                                                        1), col = Colors[Predicted[, "GroupIndex"]])
        else points(x = Predicted[, "PC1"], y = Predicted[,
                                            "PC2"], pch = (Predicted[, "GroupIndex"] - 1))
        }  # end of code for PlotPoints=T
      if (PlotHull) {  #  TRUE
        for (i in 1:length(groups)) fnConvexHull(Code = groups[i])
      }
      if (PlotEllipses)
        fnEllipses()
      if (!PlotMedians) {
        if (PlotColors)
          legend(x = legendLoc, legend = groups, pch = 0:(length(groups) -
                                                            1), col = Colors, bty = "n")
        else legend(x = legendLoc, legend = groups, pch = 0:(length(groups) -
                                                               1), bty = "n")
      }  # end of code for PlotMedians=F
      if (PlotMedians) {
        medians <- matrix(NA, nrow = length(groups), ncol = 2)
        for (i in 1:length(groups)) {
          temp <- Predicted[Predicted[, "group"] == groups[i],
                            c("PC1", "PC2")]
          medians[i, 1] <- median(temp[, 1], na_rm = T)
          medians[i, 2] <- median(temp[, 2], na_rm = T)
        }
        medians <- cbind(1:length(groups), medians)
        colnames(medians) <- c("group", "PC1", "PC2")
        text(x = medians[, "PC1"], y = medians[, "PC2"],
             labels = groups, cex = 0.75, adj = 0.5)
      } # end of code for PlotMedians=T
        if ((Identify) & ((PlotPoints) | (PlotMedians)))  {
          index<-identify(x = Predicted[,"PC1"], y = Predicted[,"PC2"])
          dataCheck<-dataUsed[index,]
        }
    }
    DataPlusPredicted <- data.frame(dataUsed, Predicted[,-1])
    #

    Predicted <- Predicted[,-2]  # remove GroupIndex
    if (ID != " ") DataPlusPredicted <- DataPlusPredicted[,-(3+length(AnalyticVars))]
      else  DataPlusPredicted <- DataPlusPredicted[,-(2+length(AnalyticVars))]#  remove GroupIndex
    pcNames <- colnames(Predicted)[-1]
    Predicted[,pcNames] <- round(Predicted[,pcNames], digits = digits)
    DataPlusPredicted[,pcNames] <- round(DataPlusPredicted[,pcNames], digits = digits)
    #
    weights <- round(weights,digits = digits)
    variances <- round(importance_pca, digits = digits)
    #
    if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
    else dataNA <- NA
    #
    if (Identify==T) {
      if (ID != " ") dataCheck<-dataCheck[order(dataCheck[,"Code"],dataCheck[,"ID"]),]
      else  dataCheck<-dataCheck[order(dataCheck[,"Code"]),][,c("Code",AnalyticVars)]
    }
    fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
    #
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-c(ScreePlot,BoxPlots,PlotPoints,PlotEllipses,PlotHull,PlotMedians,PlotColors)
    names(params_logical)<-c("ScreePlot","BoxPlots","PlotPoints","PlotEllipses","PlotHull","PlotMedians","PlotColors")
    params<-list(grouping=params_grouping,logical=params_logical,ellipses=Ellipses,Seed=Seed,
                 colors=Colors)
    #
    out<-list(usage=fcnDateVersion,
                dataUsed=dataUsed,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params=params,
                variances = variances,
                weights = weights,
                Predicted = Predicted,
                DataPlusPredicted = DataPlusPredicted,
                dataCheck=dataCheck,
                location=folder)
    out
  }
