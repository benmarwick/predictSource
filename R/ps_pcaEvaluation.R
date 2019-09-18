#'  ps_pcaEvaluation
#'
#'  Create principal component plots to evaluate the predicted sources of unknowns.
#'
#' @param doc A string with documentation, default is the function name
#' @param SourceData A data frame with the data from known sources,
#' including a code for location and element analyses
#' @param unknownData The corresponding data from unknowns
#' @param ID The name of the ID for samples (unknowns), " " if none (default value)
#' @param SourceGroup The name of the variable with the code for a source
#' @param unknownGroup The name of the variable with the code for predicted source
#' @param known_sources A vector of the source locations to be considered
#' @param predicted_sources A vector of predicted sources to be considered, not all
#'  need be in known_sources
#' @param AnalyticVars The elements used in the principal component analyses
#' @param legendLoc The location of legend added to plots (alternates are "topleft",
#'    "bottomright","bottomleft")
#' @param Identify Logical.  If TRUE, the user can identify unknowns of interest and
#' obtain a data set with information on those unknowns (default is FALSE)
#' @param plotAllPoints Logical.  If TRUE (the default), show a plot with two panes:
#' all source points and
#'    the convex hulls for the sources, and all unknown points with these source hulls
#' @param plotHullsOutsidePoints Logical.  If TRUE (the default), show a plot with two panes:
#'  all source points and
#'    the convex hulls for the sources, and the unknown points lying outside of their
#'     predicted source convex hulls and these hulls
#' @param plotOutsidePoints Logical.  If TRUE (the default), show a plot with one pane:
#' athe unknown points lying outside of their predicted source convex hulls and these
#'  hulls (the second pane for plotHullsOutsidePoints)
#' @param Seed If not NA, a positive integer used to initialize the random number generator
#'  when missing data are imputed_  Default value is 11111
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @details
#' See the vignette for instructions for identifying points of interest using the paramter
#' Identify = TRUE.
#'
#'.@return The function produces two plots: the convex hulls of the first two principal components
#' of the source data, and a plot with those convex hulls and the unknown data (this plot uses a
#' principal component analysis of both the source and unknown data).
#' The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run,
#'   the version of R used}
#'   \item{sourceData:}{  The contents of the argument SourceData restricted to knownSources}
#'   \item{sourcesNA:}{ A data frame with source observations with missing data for analytic
#'   variables; NA if no missing data}
#'   \item{unknownData:}{  The contents of the argument unknownData restricted to predictedSources}
#'   \item{unknownsNA:}{ A data frame with unknown observations with missing data for analytic
#'   variables; NA if no missing data}
#'   \item{impError}{  Normalized root mean square error estimate for imputed data;
#'   NA if no missing data}
#'   \item{params:}{  A list with the values of the grouping and source arguments and
#'   values of the logical arguments}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{tableInOut:} {  A data frame with counts of the numbers of unknowns inside and
#'   outside of each predicted source location}
#'   \item{ptsOutside:}  {  A data frame with the data for unknown points located outside of the
#'    predicted source}
#'   \item{dataCheck:}{  If Identify=TRUE, a data frame with the observations in dataUsed identified
#'    as of interest; value is c(NA,NA) if no points are identified}
#'   \item{location:}{  The value of the parameter folder}
#'    }
#'
#'
#' @import graphics stats  mgcv  assertthat
#'
#' @examples
#' # Evaluate sources of obsidian artifacts predicted from scatterplots
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' sources <- unique(ObsidianSources[,"Code"])
#' pca_eval <- ps_pcaEvaluation(SourceData=ObsidianSources,
#'   unknownData=ObsidianArtifacts, SourceGroup= "Code", unknownGroup="Code",
#'   known_sources=sources, predicted_sources=sources, AnalyticVars=analyticVars, ID="ID",
#'   plotAllPoints=TRUE, plotHullsOutsidePoints = TRUE, plotOutsidePoints = TRUE)
#'
#' # evaluate predictions from a tree model; this may need to be run twice
#' # (save_tree is created but may not be available for ps_pcaEvaluation)
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' sources <- unique(ObsidianSources[,"Code"])
#' save_tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All",
#'  AnalyticVars=analyticVars, ID="ID", Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb",
#'   ModelTitle="Rb + Sr + Y + Zr + Nb", predictSources=T, predictData=ObsidianArtifacts,
#'   plotTree=TRUE, plotCp=FALSE)
#' pca_eval <- ps_pcaEvaluation(SourceData=ObsidianSources,
#'   unknownData=save_tree$predictedSources, SourceGroup= "Code", unknownGroup="source",
#'   known_sources=sources, predicted_sources=sources, AnalyticVars=analyticVars, ID="ID",
#'   plotAllPoints=TRUE, plotHullsOutsidePoints = TRUE, plotOutsidePoints = TRUE)
#'
#' # evaluate predictions from a random forest analysis:
#' #     plot only points outside the predicted source hull
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' sources <- unique(ObsidianSources[,"Code"])
#' save_randomForest <- ps_randomForest(data=ObsidianSources,
#' GroupVar="Code",Groups="All",
#'   AnalyticVars=analyticVars, unknownID="ID", NvarUsed=3,
#'   plotErrorRate=FALSE, plotImportance=F,
#'   predictSources=TRUE, predictData=ObsidianArtifacts, plotSourceProbs=FALSE)
#' pca_eval <- ps_pcaEvaluation(SourceData=ObsidianSources,
#'   unknownData=save_randomForest$predictedSources, SourceGroup= "Code",
#'    unknownGroup="source",  known_sources=sources, predicted_sources=sources,
#'     AnalyticVars=analyticVars, ID="ID", plotAllPoints=FALSE,
#'      plotHullsOutsidePoints = FALSE, plotOutsidePoints = TRUE)
#'
#' @export
#'
ps_pcaEvaluation <-function(doc = "ps_pcaEvaluation",
           SourceData,
           unknownData,
           ID = " ",
           SourceGroup,
           unknownGroup,
           known_sources,
           predicted_sources,
           AnalyticVars,
           Identify = FALSE,
           legendLoc = "topright",
           plotAllPoints = TRUE,
           plotHullsOutsidePoints = TRUE,
           plotOutsidePoints = TRUE,
           Seed=11111,
           folder = " ")
{
  #
  #  check for valid parameters
  #  known_sources and predicted_sources not checked, as they may be names of objects
  #    from a call to a classification function
  #
  assert_that(is.data.frame(SourceData), msg="parameter SourceData not a data frame")
  assert_that(is.data.frame(unknownData), msg="parameter unknownData not a data frame")
  assert_that(is.character(SourceGroup), msg="parameter SourceGroup not character")
  assert_that(is.character(unknownGroup), msg="parameter unknownGroup not character")
  assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
              msg="parameter AnalyticVars not a character vector")
  assert_that(is.character(ID), msg="parameter ID not a character name")
  assert_that(is.logical(plotAllPoints), msg="type of parameter plotAllPoints not logical")
  assert_that(is.logical(plotHullsOutsidePoints), msg="type of parameter plotHullsOutsidePoints not logical")
  assert_that(is.logical(plotOutsidePoints), msg="type of parameter plotOutsidePoints not logical")
  assert_that(is.na(Seed) | ((round(Seed,0)==Seed)&(Seed>= 1)),
              msg="parameter Seed is not a positive integer or not NA")
  assert_that(is.character(legendLoc), msg="parameter legendLoc not character")
  assert_that(is.logical(Identify), msg="type of parameter Identify is not logical")
    #
    #  create source data set with group code and elements, restricted to identified sources
    #
    SourceRows <- SourceData[, SourceGroup] %in% known_sources
    sourceData <- SourceData[SourceRows, c(SourceGroup, AnalyticVars)]
    #
    # matrix to contain indices for observations with no missing values
    #
    sourceKeep <- rep(T, nrow(sourceData))
    for (i in 1:length(AnalyticVars))
      sourceKeep[is.na(sourceData[,AnalyticVars[i]])] <- F
    #
    #  redefine sourceData if some analysis variables are missing by imputing missing values
    #
    if (sum(sourceKeep) < nrow(sourceData)) {
      if (!is.na(Seed))  set.seed(Seed)
      temp<-rfImpute(sourceData[,SourceGroup] ~ ., sourceData[,AnalyticVars])
      colnames(temp) <- c(SourceGroup,AnalyticVars)
      sourceData <- temp[,c(SourceGroup,AnalyticVars)]
      sourcesNA <- sourceData[!sourceKeep,]
    }
    else sourcesNA <- NA
    #
    #  add numeric code for source to data set
    #
    SourceIndex <- rep(NA, nrow(sourceData))
    for (i in 1:nrow(sourceData)) {
      for (j in 1:length(known_sources))
        if (sourceData[i, SourceGroup] == known_sources[j])
          SourceIndex[i] <- j
      }  # end of loop on i
    data_Source <- rep(T, nrow(sourceData))
    sourceData <- data.frame(sourceData[,c(SourceGroup,AnalyticVars)], SourceIndex, data_Source)
    colnames(sourceData) <- c("group",AnalyticVars,"index", "data_source")
    #
    #  create dummy lab ID for combining unknownData and sourceData
    #
    dummyID = rep(" ", nrow(sourceData))
    sourceData <- data.frame(sourceData[,c("group",AnalyticVars,"index", "data_source")], ID = dummyID)
    #
    #  create unknown data with group code and elements, restricted to potential sources of interest
    #
    unknownRows <- unknownData[, unknownGroup] %in% predicted_sources
    unknownData <- unknownData[unknownRows,]
    #
    #  vector with F if row contains missing analytic variable
    #
    unknownKeep <- rep(T, nrow(unknownData))
    for (i in 1:length(AnalyticVars))
      unknownKeep[is.na(unknownData[,AnalyticVars[i]])] <- F
    #
    #  redefine predictData if some analysis variables are missing by imputing missing values
    #
    if (sum(unknownKeep) < nrow(unknownData)) {
      #  initialize random number generator if not initialized earlier
      #
      if ((sum(sourceKeep) == nrow(sourceData)) & (!is.na(Seed)))  set.seed(Seed)
      unknownsNA <- unknownData[!unknownKeep,]
      temp<-missForest(xmis=unknownData[,AnalyticVars],variablewise=F)
      impError <- temp$OOBerror
      if (ID == " ") unknownData <- data.frame(unknownData[,unknownGroup],temp$ximp)
      else  unknownData <- data.frame(unknownData[,c(unknownGroup, ID)],temp$ximp)
    }
    else {
      unknownsNA <- NA
      impError <- NA
    }
    #
    if (ID[1] == " ")  {
      ID = rep(" ", nrow(unknownData))
      unknownData <- cbind(unknownData[, c(unknownGroup, AnalyticVars)], ID = ID)
    }
    else  {unknownData <- unknownData[, c(unknownGroup, AnalyticVars, ID)]
           unknownData <- unknownData[order(unknownData[,"ID"]),]  # sort on ID
    }
    #
    #  sort on unknownGroup and ID
    #
    if (unknownGroup[1] != " ") {
      rowsSort <- order(unknownData[,unknownGroup])
      unknownData <- unknownData[rowsSort,]
    }
    if (ID[1] != " ") {
      rowsSort <- order(unknownData[,ID])
      unknownData <- unknownData[rowsSort,]
    }
    #
    #  add numeric code for predicted source to data set
    #
    ArtifactIndex <- rep(NA, nrow(unknownData))
    for (i in 1:nrow(unknownData)) {
      for (j in 1:length(known_sources))
        if (unknownData[i, unknownGroup] == as.character(predicted_sources[j],type="character"))
          ArtifactIndex[i] <- j
    }  # end of loop on i
    data_Source <- rep(F,nrow(unknownData))
    unknownData <- cbind(unknownData[,c(unknownGroup,AnalyticVars)], ArtifactIndex, data_Source,
                          unknownData[,"ID"])  # needed if no ID provided
    colnames(unknownData) <- c("group", AnalyticVars, "index", "data_source", "ID")
    if (ID[1] != " ")  unknownData <- unknownData[order(unknownData[,"ID"]),]
    #
    #  combine data sets for principal components analysis
    #
    analysisData <- rbind(sourceData, unknownData)
    #
    #  compute principal components and save first two components with ID information
    #
    pca <- prcomp(analysisData[, AnalyticVars], scale = TRUE)
    #
    #  matrix with information from summary
    #
    importance_pca <- summary(pca)$importance
    #
    #  compute locations of points on principal component plot
    #
    if (ID[1] == " ")
      pcaLocations <- cbind(analysisData[, c("group", AnalyticVars, "index","data_source")], pc1 = predict(pca)[, 1],
        pc2 = predict(pca)[, 2])
      else
      pcaLocations <- cbind(analysisData[, c("group", AnalyticVars, "index","data_source", "ID")], pc1 = predict(pca)[, 1],
         pc2 = predict(pca)[, 2])
    #
    #  create separate principal component location data sets for sources and unknowns
    #
    pcaLocationsSources <- pcaLocations[pcaLocations[,"data_source"] == T,]
    pcaLocationsArtifacts <- pcaLocations[pcaLocations[,"data_source"] == F,]
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
    range_pc1 <- range(pcaLocations[, "pc1"])
    range_pc2 <- range(pcaLocations[, "pc2"])

    #  store vertices of source convex hulls
    hullVertices <- vector("list", length(known_sources))
    names(hullVertices) <- as.character(1:length(known_sources))
    #
    for (i in 1:length(known_sources)) {
      locations <- pcaLocationsSources[pcaLocationsSources[, "group"] == known_sources[i], c("pc1", "pc2")]
      chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
      chull <- c(chull, chull[1])
      hullVertices[[i]] <- locations[chull, c("pc1", "pc2")]  # points in order defining hull
     }
    #
    # code for first two panel plot: source points and convex hulls, and all unknown poins with hulls
    #
    if (plotAllPoints == T) {
    # plots of source and unknown data with source convex hulls
    par(mfrow = c(1, 2))  #  two plots on one page
    #  first plot is convex hulls for sources and all unknown points
    #  second plot is convex hulls and unknowns lying outside of predicted hull
    #
    #  first plot: convex hulls and all source data
    #  set up size of plot
    plot(
      type = "n",
      x = range_pc1,
      y = range_pc2,
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
    for (i in 1:length(known_sources))
       lines(hullVertices[[i]])
    legend(
      x = legendLoc,
      legend = known_sources,
      pch = 1:(length(known_sources)),
      bty = "n"
    )
    #
    #  second plot: source hulls and unknown points
    #
    plot(
      type = "n",
      x = range_pc1,
      y = range_pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Hulls and unknowns"
    )
    #
    # convex hulls of source data
    for (i in 1:length(known_sources))
      lines(hullVertices[[i]])
    #  plot unknown points
    points(x = pcaLocationsArtifacts[, "pc1"],
           y = pcaLocationsArtifacts[, "pc2"],
           cex = .5,
           pch = pcaLocationsArtifacts[, "index"])
    legend(
      x = legendLoc,
      legend = known_sources,
      pch = 1:(length(known_sources)),
      bty = "n"
    )
#    browser()
    } # end of code for first two panel plot
    #
    #  create vector of indicators for whether principal components point for a unknown is in predicted source hull
    #
    unknown_in_hull <- rep(NA, nrow(pcaLocationsArtifacts))  # indicator, T if unknown in predicted source convex hull
    #
    #   convex hulls for source data
    #
    for (i in 1:length(known_sources)) {
      lines(hullVertices[[i]])
      #
      index_i <- (known_sources[pcaLocationsArtifacts[, "index"]] == known_sources[i]) # rows with data prediced from this source
      if (sum(index_i) > 0) {
        # at least one unknown from source i
        temp_i <- pcaLocationsArtifacts[index_i,]
        unknown_in_hull[index_i] <- in.out(bnd = as.matrix(hullVertices[[i]], mode="numeric"),
                                            x = as.matrix(temp_i[, c("pc1", "pc2")],mode="numeric"))
      }  # end of loop for sum(index_i) > 0
    }  # end of loop on i
    pcaLocationsArtifacts <- data.frame(pcaLocationsArtifacts, in_hull = unknown_in_hull)
    #
    #  create data frame with data on points outside predicted hull
    #
    pts_outside <- pcaLocationsArtifacts[!pcaLocationsArtifacts[,"in_hull",],]
    pts_outside_pc <- round(as.matrix(pts_outside[, c("pc1","pc2")], mode = "numeric"), digits =2)
    cols_keep <- colnames(pts_outside)[(colnames(pts_outside) != "unknown_group") &
                                       (colnames(pts_outside) != "data_source") &
                                       (colnames(pts_outside) != "pc1") & (colnames(pts_outside) != "pc2")]
    pts_outside <- pts_outside[, cols_keep]
    pts_outside <- data.frame(pts_outside, pts_outside_pc)
    #
    #  table with counts of unknowns inside and outside of predicted source hull
    #
    n_in_out <- table(pcaLocationsArtifacts[,"group"],pcaLocationsArtifacts[,"in_hull"])
    dummy<-matrix(NA, nrow = nrow(n_in_out), ncol = ncol(n_in_out)+1)
    dummy[1:nrow(n_in_out),1:ncol(n_in_out)]<-n_in_out
    dummy[,ncol(n_in_out)+1] <- apply(n_in_out,1,sum)
    dummy <- rbind(dummy, apply(dummy, 2, sum))
    colnames(dummy) <- c("outside", "inside", "total")
    rownames(dummy) <- c(rownames(n_in_out), "total")
    #
    # end of code to create and use indicator for unknown points outside of predicted hulls
    #
    dataCheck <- c(NA, NA)  # value returned if no points identified
    #
    if (plotHullsOutsidePoints == T) {
      #
      #  two panel plot to check whether unknown points are in the predicted convex hulls
      #  first panel is source convex hulls, second panel is hulls with unknown points outside of
      #  predicted hull
    #
    #  if Identify = T, do not create the evaluation plot, but only the plot of points outside predicted hulls
    #
    #  left plot panel: convex hulls for sources
    #
    #  convex hulls for source data
    #
    if (plotAllPoints == F)  plot.new()
    par(mfrow = c(1, 2))
    #  set up plot
    plot(
      type = "n",
      x = range_pc1,
      y = range_pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Source hulls"
    )
    #
    #  plot convex hulls of sources
    #
    for (i in 1:length(known_sources))
       lines(hullVertices[[i]])
    #  add ID for each group at the median of the observed values
    groups <- known_sources
    medians <-
      matrix(NA, nrow = length(groups), ncol = 2)  # matrix to store medians from each group
    for (i in 1:length(groups)) {
      temp_medians <-
        pcaLocationsSources[pcaLocationsSources[, "group"] == groups[i], c("pc1", "pc2")]  # data from group i
      medians[i, 1] <- median(temp_medians[, 1], na_rm = T)
      medians[i, 2] <- median(temp_medians[, 2], na_rm = T)
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
      x = range_pc1,
      y = range_pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Unknowns outside hulls"
    )
    #
    # plot source convex hulls
    #
    for (i in 1:length(known_sources))
     lines(hullVertices[[i]])
     legend(
      x = legendLoc,
      legend = known_sources,
      pch = 1:length(known_sources),
      bty = "n"
    )  # legend for plot
    #
    #  plot points outside of predicted hull
    #
    points(x = pcaLocationsArtifacts[!pcaLocationsArtifacts["in_hull"], "pc1"],
           y = pcaLocationsArtifacts[!pcaLocationsArtifacts["in_hull"], "pc2"],
           cex = .5,
           pch = pcaLocationsArtifacts[!pcaLocationsArtifacts["in_hull"], "index"])
#    browser()
    #
    }  #  end of code for two-panel evaluation plot
    #
    if ((plotOutsidePoints == T) | (Identify == T)) {
    #  code for single panel evaluation plot; source convex hulls and unknown points outside
    #  of predicted hull
    #
    #  convex hulls for source data
    #
    if ((plotAllPoints == F) & (plotHullsOutsidePoints == F))  plot.new()
    par(mfrow=c(1,1))
    plot(
      type = "n",
      x = range_pc1,
      y = range_pc2,
      xlab = "first PC",
      ylab = "second PC",
      main = "Points outside source hulls"
    )
    #
    # plot source convex hulls
    #
    for (i in 1:length(known_sources))
       lines(hullVertices[[i]])
    legend(
      x = legendLoc,
      legend = known_sources,
      pch = 1:length(known_sources),
      bty = "n"
    )  # legend for plot
    #
    #  plot points outside of predicted hull
    #
    points(x = pts_outside[, "pc1"], y = pts_outside[,"pc2"],
           cex = .5, pch = pts_outside[, "index"])
    #
    if (Identify == T) {
    # identify points of interest
    index<-identify(pts_outside[,c("pc1","pc2")])
    dataCheck<-pts_outside[index,]
    colnames_dataCheck<-colnames(dataCheck)[(colnames(dataCheck)!="index")&(colnames(dataCheck)!="data_source")]
    dataCheck<-dataCheck[,colnames_dataCheck]
    dataCheck[,c("pc1","pc2")] <- round(as.matrix(dataCheck[,c("pc1","pc2")],mode="numeric"),digits =2)
    }
    #
    }   #  end of code for one-panel evaluation plot
    #
    #  return information to an R object
    #
    colnames(n_in_out) <- c("outside","inside")
    #
    if (ID[1] == " ")  keepVars <- c("group", AnalyticVars)
      else          keepVars <- c("group", "ID", AnalyticVars)
    unknownData <- unknownData[,keepVars]
    pts_outside <- pts_outside[,c(keepVars, "pc1", "pc2")]
    if (Identify == T) dataCheck <- dataCheck[,c(keepVars, "pc1", "pc2")]
    #
    if (ID[1] != " ") {
       unknownData <- unknownData[order(unknownData[, "ID"]),]
       pts_outside <- pts_outside[order(pts_outside[, "ID"]),]
       if (Identify == T)  dataCheck <- dataCheck[order(dataCheck[, "ID"]),]
    }
    fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
    #
    params_logical <- c(plotAllPoints, plotHullsOutsidePoints, plotOutsidePoints)
    names(params_logical) <- c("plotAllPoints", "plotHullsOutsidePoints", "plotOutsidePoints")
    params_grouping<-list(SourceGroup, unknownGroup,known_sources,predicted_sources)
    names(params_grouping)<-c("SourceGroup","unknownGroup","known_sources","predicted_sources")
    params<-list(grouping=params_grouping,logical=params_logical,Seed=Seed)
  #
  out<-list(usage=fcnDateVersion,
                sourceData = sourceData,
                sourcesNA = sourcesNA,
                unknownData = unknownData,
                unknownsNA = unknownsNA,
                analyticVars = AnalyticVars,
                impError = impError,
                params = params,
                tableInOut = n_in_out,
                pointsOutside = pts_outside,
                dataCheck = dataCheck,
                location=folder)
   out
   }
