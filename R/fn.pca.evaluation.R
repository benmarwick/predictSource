#'  fn.pca.evaluation
#'
#'  Create plots to evaluate the predicted artifact sources using the source data principal component loadings
#'
#' @param doc: documentation in list returned by function
#' @param sourceData: data from known sources, including code for location and element analyses, restricted to specified sources
#' @param artifactData: corresponding data from artifacts
#' @param ID: ID for sample, " " if none (default value)
#' @param SourceGroup: name of variable with code for location
#' @param ArtifactGroup: name of variable with code for predicted source
#' @param known.sources: vector of locations to be considered as sources
#' @param predicted.soures: vector of predicted sources to be considered, not all must be in known.sources
#' @param AnalyticVars: elements used in the principal component analyses
#' @param loc.legend: location of legend added to plots (alternates are "topleft",
#'    "bottomright","bottomleft")
#' @param Identify: if T, the user can identify artifacts of interest and obtain a data set with information on those artifacts
#'    (default is F)
#' @param plotAllPoints: if T (the default), show a plot with two panes: all source points and
#'    the convex hulls for the sources, and all unknown points with these source hulls
#' @param plotHullsOutsidePoints: if T (the default), show a plot with two panes: all source points and
#'    the convex hulls for the sources, and the unknown points lying outside of their predicted source
#'    convex hulls and these hulls
#' @param plotOutsidePoints: if T (the default), show a plot with one pane: athe unknown points lying
#'  outside of their predicted source convex hulls and these hulls (the second pane for
#'  plotHullsOutsidePoints)
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section Details
#' See the vignette for instructions for identifying points of interest using the paramter Identify = T.
#'
#'@return The function produces two plots: the convex hulls of the first two principal components of the source data,
#'  and a plot with those convex hulls and the artifact data (using the weights obtained from the source data).
#'  The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{params:}{  A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{table.in.out:} {  A data frame with counts of the numbers of artifacts inside and outside of each
#'    predicted source location}
#'   \item{pts.outside:}  {  A data frame with the data for artifact points located outside of the predicted source}
#'   \item{data.check:}{  If Identify=T, a data frame with the observations in dataUsed identified as of interest}
#'   \item{location:}{  The values of the parameter folder}
#'    }
#'
#' @section  Details
#'
#' If using Rstudio, the plot created when Identify = T must be expanded by increasing the size of the plot pane
#'  (the view tab is not available).
#'
#' @import mgcv
#'
#' @examples
#'
#' # Evaluate predicted sources of artifacts from using scatterplots
#'
#' # evaluate predictions from a tree model: plot only points outside the predicted source hull
#` data(ObsidianSources)
#` data(ObsidianArtifacts)
#` analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#` sources <- unique(ObsidianSources[,"Code"])
#` save.tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All",
#`   AnalyticVars=analyticVars, ID="ID", Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb",
#`   predictSources=T, predictData=ObsidianArtifacts, plotTree=T, plotCp=F)
#' pca.eval <- fn.pca.evaluation(SourceData=ObsidianSources,
#'   ArtifactData=save.tree$predictedSources, SourceGroup= "Code", ArtifactGroup="source",
#'   known.sources=sources, predicted.sources=sources, AnalyticVars=analyticVars, ID="ID",
#'   plotAllPoints=T, plotHullsOutsidePoints = T, plotOutsidePoints = T)
#'
#' # evaluate predictions from a random forest analysis: plot only points outside the predicted source hull
#' data(ObsidianSources)
#` data(ObsidianArtifacts)
#` analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#` sources <- unique(ObsidianSources[,"Code"])
#` save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
#`   AnalyticVars=analyticVars, ID="ID", NvarUsed=3, plotErrorRate=F, plotImportance=F,
#`   predictSources=T, predictData=ObsidianArtifacts, plotSourceProbs=F)
#' pca.eval <- fn.pca.evaluation(SourceData=ObsidianSources,
#'   ArtifactData=save.randomForest$predictedSources, SourceGroup= "Code", ArtifactGroup="source",
#'   known.sources=sources, predicted.sources=sources, AnalyticVars=analyticVars,
#'   plotAllPoints=F, plotHullsOutsidePoints = F, plotOutsidePoints = T)
#'
#' @export

fn.pca.evaluation <-
  function(doc = "fn.pca.evaluation",
           SourceData,
           ArtifactData,
           ID = " ",
           SourceGroup,
           ArtifactGroup,
           known.sources,
           predicted.sources,
           AnalyticVars,
           Identify = F,
           loc.legend = "topright",
           plotAllPoints = T,
           plotHullsOutsidePoints = T,
           plotOutsidePoints = T,
           folder = " ")
{
    #
    #  create source data set with group code and elements, restricted to identified sources
    #
    SourceRows <- SourceData[, SourceGroup] %in% known.sources
    sourceData <- SourceData[SourceRows, c(SourceGroup, AnalyticVars)]
    #
    #  add numeric code for source to data set
    #
    SourceIndex <- rep(NA, nrow(sourceData))
    for (i in 1:nrow(sourceData)) {
      for (j in 1:length(known.sources))
        if (sourceData[i, SourceGroup] == known.sources[j])
          SourceIndex[i] <- j
      }  # end of loop on i
    data.Source <- rep(T, nrow(sourceData))
    sourceData <- data.frame(sourceData[,c(SourceGroup,AnalyticVars)], SourceIndex, data.Source)
    colnames(sourceData) <- c("group",AnalyticVars,"index", "data.source")
    #
    #  create dummy lab ID for adding ArtifactData to sourceData
    #
    dummyID = rep(" ", nrow(sourceData))
    sourceData <- data.frame(sourceData[,c("group",AnalyticVars,"index", "data.source")], ID = dummyID)
    #
    #  create artifact data with group code and elements, restricted to potential sources of interest
    #
    artifactRows <- ArtifactData[, ArtifactGroup] %in% predicted.sources
    artifactData <- ArtifactData[artifactRows,]
    if (ID == " ")  {
      ID = rep(" ", nrow(artifactData))
      artifactData <- cbind(artifactData[, c(ArtifactGroup, AnalyticVars)], ID = ID)
    }
    else  artifactData <- cbind(artifactData[, c(ArtifactGroup, AnalyticVars)],
                                           ID = artifactData[artifactRows,ID])
    #
    #  sort on ArtifactGroup and ID
    #
    if (ArtifactGroup[1] != " ") {
      rowsSort <- order(artifactData[,ArtifactGroup])
      artifactData <- artifactData[rowsSort,]
    }
    if (ID[1] != " ") {
      rowsSort <- order(artifactData[,ID])
      artifactData <- artifactData[rowsSort,]
    }
    #
    #  add numeric code for predicted source to data set
    #
    ArtifactIndex <- rep(NA, nrow(artifactData))
    for (i in 1:nrow(artifactData)) {
      for (j in 1:length(known.sources))
        if (artifactData[i, ArtifactGroup] == as.character(predicted.sources[j],type="character"))
          ArtifactIndex[i] <- j
    }  # end of loop on i
    data.Source <- rep(F,nrow(artifactData))
    artifactData <- cbind(artifactData[,c(ArtifactGroup,AnalyticVars)], ArtifactIndex, data.Source,
                          artifactData[,"ID"])
    colnames(artifactData) <- c("group", AnalyticVars, "index", "data.source", "ID")
    #
    #  combine data sets for principal components analysis
    #
    analysisData <- rbind(sourceData, artifactData)
    #
    #  compute principal components and save first two components with ID information
    #
    pca <- prcomp(analysisData[, AnalyticVars], scale = TRUE)
    #
    #  matrix with information from summary
    #
    importance.pca <- summary(pca)$importance
    #
    #  compute locations of points on principal component plot
    #
    if (ID == " ")
      pcaLocations <- cbind(analysisData[, c("group", AnalyticVars, "index","data.source")], pc1 = predict(pca)[, 1],
        pc2 = predict(pca)[, 2])
      else
      pcaLocations <- cbind(analysisData[, c("group", AnalyticVars, "index","data.source", "ID")], pc1 = predict(pca)[, 1],
         pc2 = predict(pca)[, 2])
    #
    #  create separate principal component location data sets for sources and artifacts
    #
    pcaLocationsSources <- pcaLocations[pcaLocations[,"data.source"] == T,]
    pcaLocationsArtifacts <- pcaLocations[pcaLocations[,"data.source"] == F,]
    #
    #  principal component weights
    #
    pcaWeights <-
      matrix(NA, length(AnalyticVars), length(AnalyticVars)) # principal component loadings
    dimnames(pcaWeights) <- list(paste("pc", 1:length(AnalyticVars), sep = ""), AnalyticVars)
    for (i in 1:length(AnalyticVars))
      pcaWeights[i, ] <- pca$rotation[, i]
    #
    #  principal components plots
    #
    #  compute limits for axes to include all points
    #
    range.pc1 <- range(pcaLocations[, "pc1"])
    range.pc2 <- range(pcaLocations[, "pc2"])

    #
    plot.data <-
      list(rep(NA, length(known.sources))) # save convex hull data for second plot
    #
    # code for first two panel plot: source points and convex hulls, and all artifact poins with hulls
    #
    if (plotAllPoints == T) {
    # plots of source and artifact data with source convex hulls
    par(mfrow = c(1, 2))  #  two plots on one page
    #  first plot is convex hulls for sources and all artifact points
    #  second plot is convex hulls and artifacts lying outside of predicted hull
    #
    #  first plot: convex hulls and all source data
    #
    #  convex hulls for source data
    #
      fn.convex.hull <- function(hull.group) {
        locations <- pcaLocationsSources[pcaLocationsSources[, "group"] == hull.group, c("pc1", "pc2")]
        chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
        chull <- c(chull, chull[1])
        hull.pts <-
          locations[chull, c("pc1", "pc2")]  # points in order defining hull
        lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
        hull.pts
      }  # end of fn.convex.hull
      #
    #  set up size of plot
    plot(
      type = "n",
      x = range.pc1,
      y = range.pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Sources"
    )
    #  plot points
    points(
      x = pcaLocationsSources[, "pc1"],
      y = pcaLocationsSources[, "pc2"],
      cex = .5,
      pch = pcaLocationsSources[, "index"]
    )
    # plot convex hulls
    for (i in 1:length(known.sources))
      plot.data[[i]] <- fn.convex.hull(hull.group = known.sources[i])
    legend(
      x = loc.legend,
      legend = known.sources,
      pch = 1:(length(known.sources)),
      bty = "n"
    )
    #
    #  second plot: source hulls and artifact points
    #
    plot(
      type = "n",
      x = range.pc1,
      y = range.pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Hulls and unknowns"
    )
    #
    # convex hulls of source data
    for (i in 1:length(known.sources))
      lines(plot.data[[i]])
    #  plot artifact points
    points(x = pcaLocationsArtifacts[, "pc1"],
           y = pcaLocationsArtifacts[, "pc2"],
           cex = .5,
           pch = pcaLocationsArtifacts[, "index"])
    legend(
      x = loc.legend,
      legend = known.sources,
      pch = 1:(length(known.sources)),
      bty = "n"
    )
    browser()
    } # end of code for first two panel plot
    #
    #  create vector of indicators for whether principal components point for a artifact is in predicted source hull
    #
    artifact.in.hull <- rep(NA, nrow(pcaLocationsArtifacts))  # indicator, T if artifact in predicted source convex hull
    #
    #   convex hulls for source data
    #
    fn.convex.hull <- function(hull.group) {
      locations <- pcaLocationsSources[pcaLocationsSources[, "group"] == hull.group, c("pc1", "pc2")]
      chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
      chull <- c(chull, chull[1])
      hull.pts <-
        locations[chull, c("pc1", "pc2")]  # points in order defining hull
      lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
      hull.pts
    }  # end of fn.convex.hull
    #
        for (i in 1:length(known.sources)) {
      plot.data[[i]] <- fn.convex.hull(hull.group = known.sources[i])
      index.i <- (known.sources[pcaLocationsArtifacts[, "index"]] == known.sources[i]) # rows with data prediced from this source
      if (sum(index.i) > 0) {
        # at least one artifact from source i
        temp.i <- pcaLocationsArtifacts[index.i,]
        hull.i <- plot.data[[i]]  # convex hull for this source
        artifact.in.hull[index.i] <- in.out(bnd = as.matrix(hull.i, mode="numeric"), x = as.matrix(temp.i[, c("pc1", "pc2")],mode="numeric"))
      }  # end of loop for sum(index.i) > 0
    }  # end of loop on i
    pcaLocationsArtifacts <- data.frame(pcaLocationsArtifacts, in.hull = artifact.in.hull)
    #
    #  create data frame with data on points outside predicted hull
    #
    pts.outside <- pcaLocationsArtifacts[!pcaLocationsArtifacts[,"in.hull",],]
    pts.outside.pc <- round(as.matrix(pts.outside[, c("pc1","pc2")], mode = "numeric"), dig=2)
    cols.keep <- colnames(pts.outside)[(colnames(pts.outside) != "artifact.group") &
                                       (colnames(pts.outside) != "data.source") &
                                       (colnames(pts.outside) != "pc1") & (colnames(pts.outside) != "pc2")]
    pts.outside <- pts.outside[, cols.keep]
    pts.outside <- data.frame(pts.outside, pts.outside.pc)
    #
    #  table with counts of artifacts inside and outside of predicted source hull
    #
    n.in.out <- table(pcaLocationsArtifacts[,"group"],pcaLocationsArtifacts[,"in.hull"])
    dummy<-matrix(NA, nrow = nrow(n.in.out), ncol = ncol(n.in.out)+1)
    dummy[1:nrow(n.in.out),1:ncol(n.in.out)]<-n.in.out
    dummy[,ncol(n.in.out)+1] <- apply(n.in.out,1,sum)
    dummy <- rbind(dummy, apply(dummy, 2, sum))
    colnames(dummy) <- c("outside", "inside", "total")
    rownames(dummy) <- c(rownames(n.in.out), "total")
    #
    # end of code to create and use indicator for artifact points outside of predicted hulls
    #
    data.check <- c(NA, NA)  # value returned if no points identified
    #
    if (plotHullsOutsidePoints == T) {
      #
      #  two panel plot to check whether artifact points are in the predicted convex hulls
      #  first panel is source convex hulls, second panel is hulls with artifact points outside of
      #  predicted hull
    #
    #  if Identify = T, do not create the evaluation plot, but only the plot of points outside predicted hulls
    #
    #  left plot panel: convex hulls for sources
    #
    #  convex hulls for source data
    #
      fn.convex.hull <- function(hull.group) {
        locations <- pcaLocationsSources[pcaLocationsSources[, "group"] == hull.group, c("pc1", "pc2")]
        chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
        chull <- c(chull, chull[1])
        hull.pts <-
          locations[chull, c("pc1", "pc2")]  # points in order defining hull
        lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
        hull.pts
      }  # end of fn.convex.hull
      #
      if (plotAllPoints == F)  plot.new()
    par(mfrow = c(1, 2))
    #  set up plot
    plot(
      type = "n",
      x = range.pc1,
      y = range.pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Source hulls"
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
      temp.medians <-
        pcaLocationsSources[pcaLocationsSources[, "group"] == groups[i], c("pc1", "pc2")]  # data from group i
      medians[i, 1] <- median(temp.medians[, 1], na.rm = T)
      medians[i, 2] <- median(temp.medians[, 2], na.rm = T)
    }  # end of loop on i
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
    #  right panel: set up plot of points outside corresponding convex hull
    #
    plot(
      type = "n",
      x = range.pc1,
      y = range.pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Unknowns outside hulls"
    )
    #
    # plot source convex hulls
    #
    for (i in 1:length(known.sources))
      lines(plot.data[[i]])
    legend(
      x = loc.legend,
      legend = known.sources,
      pch = 1:length(known.sources),
      bty = "n"
    )  # legend for plot
    #
    #  plot points outside of predicted hull
    #
    points(x = pcaLocationsArtifacts[!pcaLocationsArtifacts["in.hull"], "pc1"],
           y = pcaLocationsArtifacts[!pcaLocationsArtifacts["in.hull"], "pc2"],
           cex = .5,
           pch = pcaLocationsArtifacts[!pcaLocationsArtifacts["in.hull"], "index"])
    browser()
    #
    }  #  end of code for two-panel evaluation plot
    #
    if ((plotOutsidePoints == T) | (Identify == T)) {
    #  code for single panel evaluation plot; source convex hulls and artifact points outside
    #  of predicted hull
    #
    #  convex hulls for source data
    #
      fn.convex.hull <- function(hull.group) {
        locations <- pcaLocationsSources[pcaLocationsSources[, "group"] == hull.group, c("pc1", "pc2")]
        chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
        chull <- c(chull, chull[1])
        hull.pts <-
          locations[chull, c("pc1", "pc2")]  # points in order defining hull
        lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
        hull.pts
      }  # end of fn.convex.hull
      #
    if ((plotAllPoints == F) & (plotHullsOutsidePoints == F))  plot.new()
    par(mfrow=c(1,1))
    plot(
      type = "n",
      x = range.pc1,
      y = range.pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Points outside source hulls"
    )
    #
    # plot source convex hulls
    #
    for (i in 1:length(known.sources))
      plot.data[[i]] <- fn.convex.hull(hull.group = known.sources[i])
      lines(plot.data[[i]])
    legend(
      x = loc.legend,
      legend = known.sources,
      pch = 1:length(known.sources),
      bty = "n"
    )  # legend for plot
    #
    #  plot points outside of predicted hull
    #
    points(x = pts.outside[, "pc1"], y = pts.outside[,"pc2"],
           cex = .5, pch = pts.outside[, "index"])
    #
    if (Identify == T) {
    # identify points of interest
    index<-identify(pts.outside[,c("pc1","pc2")])
    data.check<-pts.outside[index,]
    colnames.data.check<-colnames(data.check)[(colnames(data.check)!="index")&(colnames(data.check)!="data.source")]
    data.check<-data.check[,colnames.data.check]
    data.check[,c("pc1","pc2")] <- round(as.matrix(data.check[,c("pc1","pc2")],mode="numeric"),dig=2)
    }
    #
    browser()
    }   #  end of code for one-panel evaluation plot
    #
    #  return information to an R object
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    logicalParams <- c(plotAllPoints, plotHullsOutsidePoints, plotOutsidePoints)
    names(logicalParams) <- c("plotAllPoints", "plotHullsOutsidePoints", "plotOutsidePoints")
    params<-list(SourceGroup, ArtifactGroup,known.sources,predicted.sources,logicalParams)
    names(params)<-c("SourceGroup","ArtifactGroup","known.sources","predicted.sources",
                     "logicalParams")
    colnames(n.in.out) <- c("outside","inside")
    #
    if (ID == " ")  keepVars <- c("group", AnalyticVars, "pc1", "pc2")
      else  keepVars <- c("group", "ID", AnalyticVars, "pc1", "pc2")
    pts.outside <- pts.outside[,keepVars]
    if (Identify == T) data.check <- data.check[,keepVars]
    #
    if ((substr(folder,1,1) != " ") & (Identify == F)) {
      files=list(paste(folder,ds.importance,sep=""),paste(folder,ds.pts.outside,sep=""),
                 paste(folder,ds.in.out,sep=""))
      names(files) <- c("ds.importance", "ds.pts.outside", "ds.in.out")
    }
 #
  out<-list(usage=fcn.date.ver,
                sourceData = sourceData,
                artifactData = ArtifactData,
                analyticVars = AnalyticVars,
                params = params,
                table.in.out = n.in.out,
                points.outside = pts.outside,
                data.check = data.check,
                location=folder)
   out
   }
