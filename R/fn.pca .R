#'  fn.PCA
#'
#'  compute principal components using standardized data
#'
#'
#' @param data: R matrix or data frame containing the data to be analyzedfn
#' @param GroupVar: name for variable defining grouping, " " if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done
#'    "All": use all groups
#'    " ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Ellipses: value or vector of proportions for confidence ellipses (must be given if PlotEllipses=T)
#' @param PlotColors: T, use list of colors in Colors for points
#'              F, plot points as black
#' @param Colors: vector of color names
#' @param folder: folder in which excel files are to be stored
#' @param ds.weights: excel file with principal component loadings, extension.csv
#' @param ds.importance: excel file with percent of variation explained, extension.csv
#'
#'  save the results of the computations as [object name]<-fn.PCA()
#'
#' @import  MASS ellipse
#'
#' @export
#'

fn.pca <-
  function(doc = "fn.pca",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           ScreePlot = F,
           BoxPlots = F,
           PlotPoints = F,
           PlotEllipses = F,
           PlotHull = T,
           PlotMedians = T,
           Ellipses = c(.95, .99),
           PlotColors = T,
           Colors,
           folder,
           ds.weights,
           ds.importance) {
    #

    #
    #  define functions to plot convex hulls, text at medians, ellipses
    #
    fn.convexhull <- function(Code) {
      temp <- Predicted[Predicted[, "group"] == Code, c("group", "PC1", "PC2")]
      hull.pts <- chull(temp[, c("PC1", "PC2")])
      hull.pts <- c(hull.pts, hull.pts[1])
      lines(temp[hull.pts, c("PC1", "PC2")])
    }
    fn.ellipses <- function() {
      for (i in 1:length(groups)) {
        temp <-
          Predicted[Predicted[, "group"] == groups[i], c("PC1", "PC2")]  # data from group i
        Covar <- var(temp)
        for (j in 1:length(Ellipses)) {
          Ellipse <-
            ellipse(
              x = Covar,
              centre = apply(temp, 2, mean, na.rm = T),
              level = Ellipses[j],
              npoints = 200
            )
          lines(Ellipse)
        }
      }
    }
    #
    #  create dataset data.Used based on grouping
    #
    if ((Groups[1] != " ") &
        (Groups[1] != "All")) {
      # restrict to desired set of groups
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    }
    else  if (GroupVar[1] == " ")
      data.Used <- data[, AnalyticVars]
    else
      data.Used <- data[, c(GroupVar, AnalyticVars)]
    #
    #  define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") &
        (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    pca <- prcomp(data.Used[, AnalyticVars], scale = TRUE)
    if (ScreePlot == T) {
      plot(pca, main = " ", xlab = "Principal component")  #  scree plot
      browser() #  pause to save scree plot
    }
    #
    importance.pca <- summary(pca)$importance
    write.csv(importance.pca, file = paste(folder, ds.importance, sep = ""))
    #
    #  weights
    #
    weights <-
      matrix(NA, length(AnalyticVars), length(AnalyticVars)) # principal component loadings
    dimnames(weights) <-
      list(paste("pc", 1:length(AnalyticVars), sep = "."), AnalyticVars)
    for (i in 1:length(AnalyticVars))  {
      weights[i, ] <- pca$rotation[, i]
    }
    write.csv(t(weights), file = paste(folder, ds.weights, sep = ""))
    #
    #  predicted values for first two components
    #
    predict.pc1 <- predict(pca)[, 1]
    predict.pc2 <- predict(pca)[, 2]
    #
    if (GroupVar[1] == " ") {
      # principal components plot of all points
      plot(predict.pc1,
           predict.pc2,
           xlab = "Component 1",
           ylab = "Component 2")
      Predicted <- predict(pca)
      browser()
    }
    if (GroupVar[1] != " ") {
      # principal component plots using grouping
      #
      #  add numeric code for group to data set
      #
      GroupIndex <- rep(NA, nrow(data.Used))
      for (i in 1:nrow(data.Used)) {
        for (j in 1:length(groups))
          if (data.Used[i, GroupVar] == groups[j])
            GroupIndex[i] <- j
      }
      Predicted <-
        data.frame(group = as.character(data.Used[, GroupVar]),
                   GroupIndex = GroupIndex,
                   predict(pca))
      #
      #  create plots
      #
      if (BoxPlots == T) {
        par(mfrow = c(1, 2)) # box plots of principal components
        plot(Predicted[, c("group", "PC1")], notch = T, main = "first PC")
        plot(Predicted[, c("group", "PC2")], notch = T, main = "second PC")
        browser()   # pause to save plot
      }
      #
      #  principal components plot
      #
      plot.new()
      #  set up dimensions of plot
      #
      if (PlotEllipses == F)
        plot(
          x = range(Predicted[, "PC1"]),
          y = range(Predicted[, "PC2"]),
          type = "n",
          xlab = "first PC",
          ylab = "second PC"
        )
      if (PlotEllipses == T)
        #  larger ranges to accomodate boundaries of ellipses
        plot(
          x = c(min(Predicted[, "PC1"], na.rm = T) - 1, max(Predicted[, "PC1"], na.rm =
                                                              T) + 1),
          y = c(min(Predicted[, "PC2"], na.rm = T) - 1, max(Predicted[, "PC2"], na.rm =
                                                              T) + 1),
          type = "n",
          xlab = "first PC",
          ylab = "second PC"
        )
      #
      if (PlotPoints == T)  {
        # plot points
        if (PlotColors == T)
          points(
            x = Predicted[, "PC1"],
            y = Predicted[, "PC2"],
            pch = (Predicted[, "GroupIndex"] - 1),
            col = Colors[Predicted[, "GroupIndex"]]
          )
        else
          points(x = Predicted[, "PC1"],
                 y = Predicted[, "PC2"],
                 pch = (Predicted[, "GroupIndex"] - 1))
      }
      if (PlotHull == T) {
        #  plot convex hull
        for (i in 1:length(groups))
          fn.convexhull(Code = groups[i])
      }
      if (PlotEllipses == T)
        fn.ellipses()
      if (PlotMedians == F)  {
        # add legend in upper left
        if (PlotColors == T)
          legend(
            x = "topleft",
            legend = groups,
            pch = 0:(length(groups) - 1),
            col = Colors,
            bty = "n"
          )
        else
          legend(
            x = "topleft",
            legend = groups,
            pch = 0:(length(groups) - 1),
            bty = "n"
          )  # use black symbols
      }
      if (PlotMedians == T) {
        #  plot codes at median of each group
        medians <-
          matrix(NA, nrow = length(groups), ncol = 2)  # matrix to store medians from each group
        for (i in 1:length(groups)) {
          temp <-
            Predicted[Predicted[, "group"] == groups[i], c("PC1", "PC2")]  # data from group i
          medians[i, 1] <- median(temp[, 1], na.rm = T)
          medians[i, 2] <- median(temp[, 2], na.rm = T)
        }
        medians <-
          cbind(1:length(groups), medians)  # add column with group number
        colnames(medians) <- c("group", "PC1", "PC2")
        text(
          x = medians[, "PC1"],
          y = medians[, "PC2"],
          labels = groups,
          cex = 0.75,
          adj = 0.5
        )
      }
    }
    if (GroupVar[1] == " ")
      DataPlusPredicted <- data.frame(data.Used, Predicted)
    else
      DataPlusPredicted <-
      data.frame(data.Used, Predicted[, -(1:2)])
    list(
      Documentation = doc,
      Summary = summary(pca),
      Weights = weights,
      Predicted = Predicted,
      DataPlusPredicted = DataPlusPredicted
    )
  }
