#'  fn.pca.evaluation  evaluate predicted artifact sources using principal component loadings
#'
#' @param doc: documentation in list returned by function
#' @param SourceData: data from known sources, including code for location and element analyses
#' @param ArtifactData: corresponding data from artifacts
#' @param SourceGroup: name of variable with code of location
#' @param ArtifactGroup: name of variable with code for predicted source
#' @param known.sources: vector of locations to be considered as sources
#'  predicted.soures: vector of predicted sources, names must match those in known.sources if both are the same (not all need to be in known.sources)
#'  predicted.sources: predicted sources to be compared
#' @param Els: elements used in principal component analyses
#' @param loc.legend: location of legend added to plots (alternates are "topleft",
#'    "bottomright","bottomleft")
#' @param folder: path to folder containing result files each file name should end with .csv
#' @param ds.importance: name of file with percent of variance explained for the known source analysis
#' @param ds.pts.outside: name of file with information on artifacts with principal component pointsoutside of hull for predicted source
#' @param ds.in.out: table with number of artifacts, by whether inside or outside hull for predicted  source, for each predicted source
#'
#' @import mgcv
#'
#' @export
#


fn.pca.evaluation <-
  function(doc = "fn.pca.evaluation",
           SourceData,
           ArtifactData,
           SourceGroup,
           ArtifactGroup,
           known.sources,
           predicted.sources,
           Els,
           loc.legend = "topright",
           folder,
           ds.importance,
           ds.pts.outside,
           ds.in.out) {
    #

    #
    #  create source data set with group code and elements, restricted to identified sources, for PCA
    #

    SourceRows <- SourceData[, SourceGroup] %in% known.sources
    pcaData <- SourceData[SourceRows, c(SourceGroup, Els)]
    #
    #  add numeric code for source to data set
    #
    SourceIndex <- rep(NA, nrow(pcaData))
    for (i in 1:nrow(pcaData)) {
      for (j in 1:length(known.sources))
        if (pcaData[i, SourceGroup] == known.sources[j])
          SourceIndex[i] <- j
    }
    temp <- data.frame(pcaData, SourceIndex)
    #
    #  compute principal components and save first two components with ID information
    #
    pca <- prcomp(temp[, Els], scale = TRUE)
    #
    #  matrix with information from summary
    #
    importance.pca <- summary(pca)$importance
    write.csv(importance.pca, file = paste(folder, ds.importance, sep = ""))
    #
    predict.pc1 <-
      cbind(temp[, SourceGroup], SourceIndex = SourceIndex, PC1 = predict(pca)[, 1])
    predict.pc2 <-
      cbind(temp[, SourceGroup], SourceIndex = SourceIndex, PC2 = predict(pca)[, 2])
    #
    #  principal component weights
    #
    PrinComp <-
      matrix(NA, length(Els), length(Els)) # principal component loadings
    dimnames(PrinComp) <- list(paste("pc", 1:length(Els), sep = ""), Els)
    #
    #  matrix with predicted values of PCs for each area
    #
    Predicted <- predict(pca)
    dimnames(Predicted) <- list(NULL, paste("pc", 1:length(Els), sep = ""))
    Predicted <-
      data.frame(SourceIndex = temp[, "SourceIndex"], Code = pcaData[, SourceGroup], Predicted)
    #
    for (i in 1:length(Els))  {
      PrinComp[i, ] <- pca$rotation[, i]
    }
    #
    #  principal components plot
    #
    #  weights applied to artifact data
    #
    temp <- ArtifactData[, c(ArtifactGroup, Els)]
    #  standardize data
    data.std <- matrix(NA, nrow(temp), length(Els))
    for (j in 1:length(Els)) {
      temp.mean <- mean(pcaData[, Els[j]])
      temp.sd <- sd(pcaData[, Els[j]])
      data.std[, j] <- (temp[, Els[j]] - temp.mean) / temp.sd
    }
    #  predicted principal components
    PC.1 <- rep(NA, nrow(data.std))
    PC.2 <- PC.1
    for (i in 1:nrow(data.std)) {
      PC.1[i] <- crossprod(data.std[i, ], PrinComp[1, ])
      PC.2[i] <- crossprod(data.std[i, ], PrinComp[2, ])
    }
    # compute index for each predicted artifact source
    index <- rep(NA, nrow(ArtifactData))
    for (i in 1:length(index))  {
      for (j in 1:length(predicted.sources))
        if (ArtifactData[i, ArtifactGroup] == predicted.sources[j])
          index[i] <- j
    }
    #
    data.pc <-
      data.frame(ArtifactData[, ArtifactGroup], index, cbind(PC.1, PC.2))
    colnames(data.pc) <- c("artifact.group", "index", "pc.1", "pc.2")
    #
    #  principal components plots
    #
    #  compute limits for axes to include all points
    #
    min.pc1 <- min(Predicted[, "pc1"], data.pc[, "pc.1"])
    max.pc1 <- max(Predicted[, "pc1"], data.pc[, "pc.1"])
    min.pc2 <- min(Predicted[, "pc2"], data.pc[, "pc.2"])
    max.pc2 <- max(Predicted[, "pc2"], data.pc[, "pc.2"])
    #
    #  convex hulls for source data
    #
    fn.convex.hull <- function(Code = "BS") {
      predicted <- Predicted[Predicted[, SourceGroup] == Code, c("pc1", "pc2")]
      chull <- chull(x = predicted[, "pc1"], y = predicted[, "pc2"])
      chull <- c(chull, chull[1])
      hull.pts <-
        predicted[chull, c("pc1", "pc2")]  # points in order defining hull
      lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
      hull.pts
    }
    #
    plot.data <-
      list(rep(NA, length(known.sources))) # save convex hull data for second plot
    #
    plot.new()  # plots of source and artifact data with source convex hulls
    par(mfrow = c(1, 2))  #  two plots on one page
    #  first plot is convex hulls for sources and all artifact points
    #  second plot is convex hulls and artifacts lying outside of predicted hull
    #
    #  first plot: convex hulls and all source data
    #
    #  set up size of plot
    plot(
      type = "n",
      x = c(min.pc1, max.pc1),
      y = c(min.pc2, max.pc2),
      xlab = "first PC",
      ylab = "second PC",
      main = "Principal component plot of source data"
    )
    #  plot points
    points(
      x = Predicted[, "pc1"],
      y = Predicted[, "pc2"],
      cex = .5,
      pch = (Predicted[, "SourceIndex"] - 1)
    )
    # plot convex hulls
    for (i in 1:length(known.sources))
      plot.data[[i]] <- fn.convex.hull(Code = known.sources[i])
    legend(
      x = loc.legend,
      legend = known.sources,
      pch = 0:(length(known.sources) - 1),
      bty = "n"
    )
    #
    #  second plot: source hulls and artifact points
    #
    plot(
      type = "n",
      x = c(min.pc1, max.pc1),
      y = c(min.pc2, max.pc2),
      xlab = "first PC",
      ylab = "second PC",
      main = "Artifacts and hulls of predicted sources"
    )
    #
    for (i in 1:length(known.sources))
      # convex hulls of source data
      lines(plot.data[[i]])
    #  plot artifact points
    #  get information for labels
    crosstab <- table(data.pc[, "artifact.group"], data.pc[, "index"])
    indices <- as.numeric(colnames(crosstab))
    source.names <- as.character(rownames(crosstab))
    points(x = data.pc[, "pc.1"],
           data.pc[, "pc.2"],
           cex = .5,
           pch = data.pc[, "index"])
    legend(
      x = loc.legend,
      legend = source.names,
      pch = indices,
      bty = "n"
    )
    browser()   #  pause to save plot
    #
    # plots to check whether artifact points are in the predicted convex hulls
    #
    artifact.sources <- as.character(unique(ArtifactData[, ArtifactGroup]))
    #
    plot.new()
    par(mfrow = c(1, 2))
    #  set up plot
    plot(
      type = "n",
      x = c(min.pc1, max.pc1),
      y = c(min.pc2, max.pc2),
      xlab = "first PC",
      ylab = "second PC",
      main = "Convex hulls for sources"
    )
    #
    #  plot convex hulls of sources
    #
    for (i in 1:length(known.sources))
      lines(plot.data[[i]])
    #  add ID for each group at the median of the observed values
    groups <- known.sources
    medians <-
      matrix(NA, nrow = length(groups), ncol = 2)  # matrix to store medians from each group
    for (i in 1:length(groups)) {
      temp <-
        Predicted[Predicted[, SourceGroup] == groups[i], c("pc1", "pc2")]  # data from group i
      medians[i, 1] <- median(temp[, 1], na.rm = T)
      medians[i, 2] <- median(temp[, 2], na.rm = T)
    }
    medians <-
      cbind(1:length(groups), medians)  # add column with group number
    colnames(medians) <- c("group", "pc1", "pc2")
    text(
      x = medians[, "pc1"],
      y = medians[, "pc2"],
      labels = groups,
      cex = 0.75,
      adj = 0.5
    )
    #
    #  set up plot of points outside corresponding convex hull
    #
    plot(
      type = "n",
      x = c(min.pc1, max.pc1),
      y = c(min.pc2, max.pc2),
      xlab = "first PC",
      ylab = "second PC",
      main = "Points outside source hulls"
    )
    #
    for (i in 1:length(known.sources))
      # plot convex hulls
      lines(plot.data[[i]])
    #
    #  compute indicator for predicted source within convex hull of that source
    #  plot points outside of predicted hull
    #  create crosstab of source/inside/outside hull
    #  create data set of points outside hulls
    #
    flag <- 0  #  reset to 1 with first source with point outside hull
    #cols.keep<-colnames(data.pc)[colnames(data.pc)!="index"]
    n.in.out <- matrix(0, nrow = length(known.sources) + 1, ncol = 3)
    colnames(n.in.out) <- c("in", "out", "total")
    rownames(n.in.out) <- c(known.sources, "total")
    for (i in 1:length(known.sources)) {
      index.i <-
        (data.pc[, "artifact.group"] == known.sources[i]) # rows with data prediced from this source
      if (sum(index.i) > 0) {
        # at least one artifact from source i
        temp.i <-
          cbind(data.pc[index.i, ], ArtifactData[index.i, ]) # add artifact data to principal components
        temp.pc <-
          as.matrix(temp.i[, c("pc.1", "pc.2")]) # force artifact PC data to matrix form
        plot.data.i <-
          as.matrix(plot.data[[i]])  # convex hull for this source
        indicator <- in.out(bnd = plot.data.i, x = temp.pc[, c("pc.1", "pc.2")])
        #  vector of indicators (TRUE, FALSE) whether artifacts lie within convex hull
        n.in.out[i, ] <-
          c(sum(indicator), sum(!indicator), length(indicator)) # create row of table
        if (sum(indicator) > 0) {
          # points outside hull for this source
          if (flag == 1)
            pts.outside <- rbind(pts.outside, temp.i[!indicator, ])
          if (flag == 0) {
            flag <- 1
            pts.outside <- temp.i[!indicator, ]
          }
          points(temp.pc[!indicator, c("pc.1", "pc.2")], pch = i, cex = .5)  # plots points outside hull
        }
      }
    }
    legend(
      x = loc.legend,
      legend = predicted.sources,
      pch = 1:length(predicted.sources),
      bty = "n"
    )  # legend for plot
    #
    #  keep desired columns from pts.outside
    #
    cols.keep <-
      colnames(pts.outside)[(colnames(pts.outside) != "index") &
                              (colnames(pts.outside) != "artifact.group")]
    pts.outside <- pts.outside[, cols.keep]
    write.csv(pts.outside, file = paste(folder, ds.pts.outside, sep = ""))
    #
    n.in.out[length(known.sources) + 1, ] <- apply(n.in.out, 2, sum)
    write.csv(n.in.out, file = paste(folder, ds.in.out, sep = ""))
    #
    list(
      Documentation = doc,
      table.in.out = n.in.out,
      points.outside = pts.outside
    )
  }
