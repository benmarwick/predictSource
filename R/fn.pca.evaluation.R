#'  fn.pca.evaluation
#'
#'  Create plots to evaluate the predicted artifact sources using the source data principal component loadings
#'
#' @param doc: documentation in list returned by function
#' @param sourceData: data from known sources, including code for location and element analyses, restricted to specified sources
#' @param artifactData: corresponding data from artifacts
#' @param labID: ID for sample, " " if none (default value)
#' @param SourceGroup: name of variable with code for location
#' @param ArtifactGroup: name of variable with code for predicted source
#' @param known.sources: vector of locations to be considered as sources
#' @param predicted.soures: vector of predicted sources to be considered, not all must be in known.sources
#' @param AnalyticVars: elements used in principal component analyses
#' @param loc.legend: location of legend added to plots (alternates are "topleft",
#'    "bottomright","bottomleft")
#' @param Identify: if T, the user can identify artifacts of interest and obtain a data set with information on those artifacts
#'    (default is F)
#' @param folder: path to folder containing result files each file name should end with .csv
#' @param ds.importance: name of file with percent of variance explained for the known source analysis
#' @param ds.pts.outside: name of file with information on artifacts with principal component pointsoutside of hull for predicted source
#' @param ds.in.out: table with number of artifacts, by whether inside or outside hull for predicted  source, for each predicted source
#'
#' @section Details
#' See the vignette for instructions for identifying points of interest using the paramter Identify = T.
#'
#'@return The function produces two plots: the convex hulls of the first two principal components of the source data,
#'  and a plot with those convex hulls and the artifact data (using the weights obtained from the source data).
#'  The function returns a list with the following components:
#' \itemize{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{params:}{  A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{table.in.out:} {  A data frame with counts of the numbers of artifacts inside and outside of the source locations}
#'   \item{pts.outside:}  {  A data frame with the data for artifact points located outside of a source}
#'   \item{data.check:}{  If Identify=T, a data frame with the observations in dataUsed identified as of interest}
#'   \item{files:}{  If folder != " ", a list with path and data set names to the excel files containing importance,
#'   ds.in.out, pts.outside, and, if Identify=T, data for the points of interest}
#'    }
#'
#' @import mgcv
#'
#' @examples
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' sources <- unique(ObsidianSources[,"Code"])
#' pca.eval <- fn.pca.evaluation(SourceData=ObsidianSources, ArtifactData=ObsidianArtifacts,SourceGroup= "Code",
#'    ArtifactGroup="Code",known.sources=sources, predicted.sources=sources, AnalyticVars=analyticVars)
#'
#' @export
#
fn.pca.evaluation <-
  function(doc = "fn.pca.evaluation",
           SourceData,
           ArtifactData,
           labID = " ",
           SourceGroup,
           ArtifactGroup,
           known.sources,
           predicted.sources,
           AnalyticVars,
           Identify = F,
           loc.legend = "topright",
           folder = " ",
           ds.importance,
           ds.pts.outside,
           ds.in.out,
           ds.identified) {
    #
    #  create source data set with group code and elements, restricted to identified sources, for PCA
    #
    SourceRows <- SourceData[, SourceGroup] %in% known.sources
    pcaData <- SourceData[SourceRows, c(SourceGroup, AnalyticVars)]
    #
    #  add numeric code for source to data set
    #
    SourceIndex <- rep(NA, nrow(pcaData))
    for (i in 1:nrow(pcaData)) {
      for (j in 1:length(known.sources))
        if (pcaData[i, SourceGroup] == known.sources[j])
          SourceIndex[i] <- j
    }  # end of loop on i
    temp <- data.frame(pcaData, SourceIndex)
    #
    #  compute principal components and save first two components with ID information
    #
    pca <- prcomp(temp[, AnalyticVars], scale = TRUE)
    #
    #  matrix with information from summary
    #
    importance.pca <- summary(pca)$importance
    if (folder != " ")  write.csv(importance.pca, file = paste(folder, ds.importance, sep = ""))
    #
    predict.pc1 <-
      cbind(temp[, SourceGroup], SourceIndex = SourceIndex, PC1 = predict(pca)[, 1])
    predict.pc2 <-
      cbind(temp[, SourceGroup], SourceIndex = SourceIndex, PC2 = predict(pca)[, 2])
    #
    #  principal component weights
    #
    PrinComp <-
      matrix(NA, length(AnalyticVars), length(AnalyticVars)) # principal component loadings
    dimnames(PrinComp) <- list(paste("pc", 1:length(AnalyticVars), sep = ""), AnalyticVars)
    #
    #  matrix with predicted values of PCs for each area
    #
    Predicted <- predict(pca)
    dimnames(Predicted) <- list(NULL, paste("pc", 1:length(AnalyticVars), sep = ""))
    Predicted <-
      data.frame(SourceIndex = temp[, "SourceIndex"], Code = pcaData[, SourceGroup], Predicted)
    #
    for (i in 1:length(AnalyticVars))
      PrinComp[i, ] <- pca$rotation[, i]
    #
    #  principal components plot
    #
    #  weights applied to artifact data
    #
    #  restrict artifact data to potential sources of interest
    artifactRows <- ArtifactData[, ArtifactGroup] %in% predicted.sources
    if (labID == " ")  artifactData <- ArtifactData[artifactRows, c(ArtifactGroup, AnalyticVars)]
      else  artifactData <- ArtifactData[artifactRows, c(ArtifactGroup, labID, AnalyticVars)]
    #  standardize data
    data.std <- matrix(NA, nrow(artifactData), length(AnalyticVars))
    for (j in 1:length(AnalyticVars)) {
      temp.mean <- mean(pcaData[, AnalyticVars[j]])
      temp.sd <- sd(pcaData[, AnalyticVars[j]])
      data.std[, j] <- (artifactData[, AnalyticVars[j]] - temp.mean) / temp.sd
    }  # end of loop on j
    #  predicted principal components
    PC.1 <- rep(NA, nrow(data.std))
    PC.2 <- PC.1
    for (i in 1:nrow(data.std)) {
      PC.1[i] <- crossprod(data.std[i, ], PrinComp[1, ])
      PC.2[i] <- crossprod(data.std[i, ], PrinComp[2, ])
    }  # end of loop on i
    #
    # compute index for each predicted artifact source
    index <- rep(NA, nrow(artifactData))
    for (i in 1:length(index))  {
      for (j in 1:length(predicted.sources))
        if (artifactData[i, ArtifactGroup] == predicted.sources[j])
          index[i] <- j
    }  #  end of loop on i
    #
    data.pc <-
      data.frame(artifactData[, ArtifactGroup], index, cbind(PC.1, PC.2))
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
    fn.convex.hull <- function(Code) {
      predicted <- Predicted[Predicted[, SourceGroup] == Code, c("pc1", "pc2")]
      chull <- chull(x = predicted[, "pc1"], y = predicted[, "pc2"])
      chull <- c(chull, chull[1])
      hull.pts <-
        predicted[chull, c("pc1", "pc2")]  # points in order defining hull
      lines(x = hull.pts[, "pc1"], y = hull.pts[, "pc2"])
      hull.pts
    }  # end of fn.convex.hull
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
    # convex hulls of source data
    for (i in 1:length(known.sources))
      lines(plot.data[[i]])
    #  plot artifact points
    #  get information for labAnalyticVars
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
    #
    # plots to check whether artifact points are in the predicted convex hulls
    #
    artifact.sources <- as.character(unique(artifactData[, ArtifactGroup]))
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
    # plot convex hulls
    for (i in 1:length(known.sources))
      lines(plot.data[[i]])
    legend(
      x = loc.legend,
      legend = predicted.sources,
      pch = 1:length(predicted.sources),
      bty = "n"
    )  # legend for plot
    #
    #  compute indicator for predicted source within convex hull of that source
    #  plot points outside of predicted hull
    #  create crosstab of source/inside/outside hull
    #  create data set of points outside hulls
    #
    flag <- 0  #  reset to 1 with first source with point outside hull
    #cols.keep<-colnames(data.pc)[colnames(data.pc)!="index"]
    n.in.out <- matrix(0, nrow = length(known.sources) + 1, ncol = 3)  # matrix with cross-tabulation of in/out points
       # dummy row to set up information on identified observations
    #
    create.data.check <- 0  # flag to create data.check in first iteration
    #
    for (i in 1:length(known.sources)) {
      index.i <-
        (data.pc[, "artifact.group"] == known.sources[i]) # rows with data prediced from this source
      if (sum(index.i) > 0) {
        # at least one artifact from source i
        temp.i <-
          cbind(data.pc[index.i, ], artifactData[index.i, ]) # add artifact data to principal components
        if ((Identify == T) & (create.data.check == 0)) {
          data.check<-temp.i[1,]
          create.data.check <- 1
          }
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
            }  # end of loop for flag=0
          points(temp.pc[!indicator, c("pc.1", "pc.2")], pch = i, cex = .5)  # plots points outside hull
          if (Identify==T)  {
            index<-identify(x = temp.pc[,"pc.1"], y = temp.pc[,"pc.2"])
            data.check<-rbind(data.check,temp.i[index,])
            }  # end of loop for Identify = T
         }  # end of loop for sum(indicator) > 0
      } # end of loop for sum(index.i) > 0
      browser()
    }  # end of loop on i
    #
    #  keep desired columns from pts.outside
    #
    cols.keep <-
      colnames(pts.outside)[(colnames(pts.outside) != "index") &
                              (colnames(pts.outside) != "artifact.group")]
    pts.outside <- pts.outside[, cols.keep]
    if (folder != " ")  write.csv(pts.outside, file = paste(folder, ds.pts.outside, sep = ""))
    #
    n.in.out[length(known.sources) + 1, ] <- apply(n.in.out, 2, sum)
    table.in.out <- data.frame(Source = c(as.vector(known.sources, mode = "character"),"tOtal"), n.in.out)
    colnames(table.in.out) <- c("Source","in", "out", "total")
    if (folder != " ") write.csv(table.in.out, file = paste(folder, ds.in.out, sep = ""))
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params<-list(SourceGroup, ArtifactGroup,known.sources,predicted.sources)
    names(params)<-c("SourceGroup","ArtifactGroup","known.sources","predicted.sources")
    if (Identify == T) {
      if (nrow(data.check) > 1) {
        data.check <- data.check[-1,c(-2,-5)]
        data.check[,c("pc.1","pc.2")] <- round(data.check[,c("pc.1","pc.2")],dig=2)
        }
      }
    #
    if ((substr(folder,1,1) != " ") & (Identify == F)) {
      files=list(paste(folder,ds.importance,sep=""),paste(folder,ds.pts.outside,sep=""),
                 paste(folder,ds.in.out,sep=""))
      names(files) <- c("ds.importance", "ds.pts.outside", "ds.in.out")
    }
    if ((substr(folder,1,1) != " ") & (Identify == T)) {
      files=list(paste(folder,ds.importance,sep=""),paste(folder,ds.pts.outside,sep=""),
                 paste(folder,ds.in.out,sep=""),paste(folder,ds.identified,sep=""))
      names(files) <- c("ds.importance", "ds.pts.outside", "ds.in.out","ds.identified")
    }
    #
    if ((substr(folder,1,1) == " ") & (!Identify))
      out<-list(usage=fcn.date.ver,
                sourceData = pcaData,
                artifactData = artifactData,
                analyticVars = AnalyticVars,
                params = params,
                table.in.out = table.in.out,
                points.outside = pts.outside)
    if ((substr(folder,1,1) != " ") & (!Identify))
      out<-list(usage = fcn.date.ver,
                sourceData = pcaData,
                artifactData = artifactData,
                analyticVars = AnalyticVars,
                params = params,
                table.in.out = table.in.out,
                points.outside = pts.outside,
                files=list(paste(folder,ds.weights,sep=""),paste(folder,ds.importance,sep="")))
    if ((substr(folder,1,1) == " ") & (Identify))
      out<-list(usage=fcn.date.ver,
                sourceData = pcaData,
                artifactData = artifactData,
                analyticVars=AnalyticVars,
                params=params,
                table.in.out = n.in.out,
                points.outside = pts.outside,
                data.check=data.check)
    if ((substr(folder,1,1) != " ") & (Identify == T))
      out<-list(usage=fcn.date.ver,
                sourceData = pcaData,
                artifactData = ArtifactData,
                analyticVars=AnalyticVars,
                params=params,
                table.in.out = table.in.out,
                points.outside = pts.outside,
                data.check=data.check,
                files=files)
    if ((substr(folder,1,1) != " ") & (Identify) == F)
      out<-list(usage=fcn.date.ver,
              sourceData = pcaceData,
              artifactData = artifactData,
              analyticVars=AnalyticVars,
              params=params,
              table.in.out = table.in.out,
              points.outside = pts.outside,
              files=files)
    out
   }
