#' ps_3dPlot
#'
#' Create 3-dimensional data plot. The function stops after producing each plot.
#' Enter c ("continue") at the prompt to get the next plot.
#' If this function is run using Rstudio, each plot appears in a separate window, not in the Rstudio plot pane.
#'
#' @param doc A string documenting use written to the output list;
#' default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param GroupVar The name for variable defining grouping
#' @param Groups A vector of values of group variable for which plots
#'  are to be done; if "All", use all groups
#' @param AnalyticVars A vector of names (character values) of the analytic variables
#' @param Selections A vector of length 3, or matrix or data frame with 3 columns,
#'  with the combinations of the analytic variables to be plotted
#' @param ByGroup Logical.  If TRUE, show scatterplot for each group
#'  for each selection of 3 variables; if FALSE (the default),
#'   all specified groups are on one plot
#' @param PlotMedians  Logical.  If TRUE, plot only the medians in each group
#'  (the points are not plotted).  If FALSE (the default), the median locations are not plotted
#' @param Colors A vector with the colors of plotted points, used sequentially for the groups
#' @param SymbolSize A value at most 1, a smaller value gives smaller diameter points
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage:}{  A string with the contents of the argument doc,
#' the date run, the version of R used}
#' \item{dataUsed:}{  The contents of the argument data restricted
#' to the groups used}
#' \item{dataNA:}{  A data frame with observations containing at
#' least one missing value for an analysis variable, NA if no missing values}
#' \item{params:}{  A list with the values of the grouping, logical,
#'  numeric and Color arguments}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#' }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot,
#'  specification of the argument Selections as a matrix or data frame,
#'   and use of colors.  If the plot or plots are not by group,
#'   all points have the color of the first element in Colors.
#'   If PlotMedians = TRUE, the value of ByGroup is not used.
#'
#' @import MASS stats assertthat
#'
#' @examples
#' #  show points from several groups on one plot
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot3d<-ps_3dPlot(data=ObsidianSources, GroupVar="Code",
#' Groups=c("A","B"), AnalyticVars=analyticVars,
#' Selections=rbind(analyticVars[1:3],analyticVars[2:4]))
#'
#' #  plots with one group per plot
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot3d<-ps_3dPlot(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),
#'  ByGroup=TRUE, AnalyticVars=analyticVars, Selections=analyticVars[1:3])
#'
#' @export

ps_3dPlot <-
  function(doc = "ps_3dPlot",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup = FALSE,
           PlotMedians = FALSE,
           Colors = c("red","black","blue","green","purple"),
           SymbolSize = 0.7) {

    #
    #  check for valid parameters
    #
    assert_that(is.data.frame(data), msg="parameter data not a data frame")
    assert_that(is.character(GroupVar), msg="parameter GroupVar not character")
    assert_that(is.character(Groups), msg="parameter Groups not character")
    assert_that(is.logical(ByGroup), msg="type of parameter ByGroup not logical")
    assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
                msg="parameter AnalyticVars not a character vector")
    assert_that(is.character(Selections), msg="type of parameter Selections not character")
    assert_that(is.vector(Selections) | is.matrix(Selections),
                msg="parameter VariablePairs must be a vector or matrix")
    if (is.vector(Selections))  assert_that(length(Selections)==3, msg="vector Selections not of length 3")
    if (is.matrix(Selections))  assert_that(ncol(Selections)==3,
                                               msg="number of columns of matrix Selections not 3")
    assert_that(is.logical(PlotMedians), msg="type of parameter PlotMedians not logical")
    assert_that(is.character(Colors), msg="parameter Colors not character")
    assert_that(is.numeric(SymbolSize)&(SymbolSize > 0), msg="parameter SymbolSize not positive and numeric")
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows, c(GroupVar, AnalyticVars)]
    }
    else if (GroupVar[1] == " ")
      dataUsed <- data[, AnalyticVars]
    else dataUsed <- data[, c(GroupVar, AnalyticVars)]
    #
    #  vector to contain indices for observations with no missing values
    #
    dataKeep <- rep(T, nrow(dataUsed))
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- FALSE
    #
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(dataUsed[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    #  check for number of colors specified
    #
    if (!ByGroup)  #  FALSE
      if (length(Colors) < length(groups))  stop("too few colors specified")
    #
    #  sort dataUsed on grouping variable to assign colors to points
    #
    if (GroupVar[1] != " ") dataUsed<-dataUsed[order(dataUsed[,GroupVar]),]
    #
    #  add index to dataUsed to specific color for plotting points in groups
    #
    if (!ByGroup) {
      n_group<-rep(0,length(groups))
      for (i in 1:length(groups))  {
        n_group<-nrow(dataUsed[dataUsed[,GroupVar]==groups[i],])
        if (i == 1) group_index<-rep(1,n_group)
        else  group_index<-c(group_index,rep(i,n_group))
      }
      dataUsed<-cbind(dataUsed,group_index=group_index)
    }
    #
    if (!PlotMedians) {  #  FALSE
      #  plot points
      if ((GroupVar[1] == " ") | (!ByGroup)) {
        if (is.vector(Selections)) {
          index <- is.na(dataUsed[, Selections[1]]) | is.na(dataUsed[,Selections[2]]) |
                      is.na(dataUsed[, Selections[3]])
          scatterplot3d::scatterplot3d(
                        dataUsed[!index, Selections[1]], dataUsed[!index, Selections[2]],
                        dataUsed[!index, Selections[3]], xlab = Selections[1],
                        ylab = Selections[2], zlab = Selections[3],
                        pch = 16, cex.symbols = SymbolSize,
                        main = paste(Selections[1], ",", Selections[2], ",", Selections[3]))
        }
        if (is.matrix(Selections)) {
          for (i in 1:nrow(Selections)) {
            groups<-unique(dataUsed[,GroupVar])
            subtitle<-paste("Group:  ",groups[1],": ",Colors[1],"  ",sep="")
            for (j in 2:length(groups))
              subtitle<-paste(subtitle,groups[j],": ",Colors[j],"  ",sep="")
              index_na <- is.na(dataUsed[, Selections[i,1]]) | is.na(dataUsed[,Selections[i,2]]) |
              is.na(dataUsed[, Selections[i,3]])
              scatterplot3d::scatterplot3d(
                          dataUsed[!index_na, Selections[i, 1]],dataUsed[!index_na, Selections[i, 2]],
                          dataUsed[!index_na, Selections[i, 3]], xlab = Selections[i, 1],
                          ylab = Selections[i, 2], zlab = Selections[i, 3],
                          color = Colors[group_index], pch = 16, cex.symbols = SymbolSize,
                          main = paste(Selections[i, 1], ",", Selections[i,2], ",", Selections[i, 3]),
                          sub=subtitle)
 #         browser()
          }
        }
      } # end of plot points for groups combined
      #
      if ((GroupVar[1] != " ") & (ByGroup)) { # plot points by group
        if (is.vector(Selections)) {
          for (i in 1:length(groups)) {
            dev.new()
            data_i<-dataUsed[dataUsed[,GroupVar]==groups[i],Selections]
            index_na <- is.na(data_i[, Selections[1]]) | is.na(data_i[,Selections[2]]) |
              is.na(data_i[, Selections[3]])
            scatterplot3d::scatterplot3d(
                          data_i[!index_na,], xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
                          color = Colors[1], pch = 16, cex.symbols = SymbolSize,
                          main = paste(groups[i],": ",Selections[1]," ,", Selections[2], ",", Selections[3],sep=""))
 #          browser()
          }
        }
        if (is.matrix(Selections)) {
          for (i in 1:nrow(Selections)) {
            for (j in 1:length(groups)) {
              dev.new()
              data_j<-dataUsed[dataUsed[,GroupVar]==groups[j],Selections[i,], index]
              index_na <- is.na(data_j[, Selections[i,1]]) | is.na(data_j[,Selections[i,2]]) |
                is.na(data_j[, Selections[i,3]])
              scatterplot3d::scatterplot3d(
                            data_j[!index_na,], xlab = Selections[i, 1], ylab = Selections[i, 2],
                            zlab = Selections[i,3], pch = 16, cex.symbols = SymbolSize,
                            main = paste(groups[i],": ",Selections[i, 1], ",", Selections[i,2], ",",
                                         Selections[i, 3]), color = Colors[index])
 #            browser()
            }
          }
        }
      } # end of plot points by group
    }
    if (PlotMedians & ByGroup) {
      # plot medians
      if (is.vector(Selections)) {
        dev.new()
        medians<-matrix(NA,nrow=length(groups),ncol=3)
        for (i in 1:length(groups)) {
          data_i<-dataUsed[dataUsed[,GroupVar]==groups[i],Selections]
          index_na <- is.na(data_i[, Selections[1]]) | is.na(data_i[,Selections[2]]) |
            is.na(data_i[, Selections[3]])
          medians[i,]<-apply(data_i[!index_na,],2,median)
        }
        scatterplot3d::scatterplot3d(
                      medians, xlab = Selections[1],
                      ylab = Selections[2], zlab = Selections[3],
                      color = "black", pch = groups, cex.symbols = SymbolSize,
                      main = paste("group medians:",Selections[1], ",", Selections[2], ",", Selections[3]))
      }
      if (is.matrix(Selections) | is.data.frame(Selections)) {
        for (i in 1:nrow(Selections)) {
          medians<-matrix(NA,nrow=length(groups),ncol=3)
          for (j in 1:length(groups)) {
            data_j <- dataUsed[dataUsed[, GroupVar] == groups[j],Selections[i,] ]
            index_na <- is.na(data_j[, Selections[i,1]]) | is.na(data_j[,Selections[i,2]]) |
              is.na(data_j[, Selections[i,3]])
            medians[j,]<-apply(data_j[!index_na,],2,median)
          }
          dev.new()
          scatterplot3d::scatterplot3d(
                        medians, xlab = Selections[i, 1], ylab = Selections[i, 2],
                        zlab = Selections[i, 3], color = "black",
                        pch = groups, cex.symbols = SymbolSize, main = paste("group medians:",
                        Selections[i,1], ",", Selections[i,2], ",", Selections[i,3]))
 #         browser()
        }
      }
    }
    fcnDateVersion<-c(doc,date(),R.Version()$version.string)
    #
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-c(ByGroup,PlotMedians)
    names(params_logical)<-c("ByGroup","PlotMedians")
    params_numeric<-SymbolSize
    names(params_numeric)<-"SymbolSize"
    params<-list(grouping=params_grouping,logical=params_logical,numeric=params_numeric,colors=Colors)
    #
    if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep]
    else dataNA <- NA
    #
    out<-list(usage=fcnDateVersion,
              dataUsed=dataUsed,
              dataNA=dataNA,
              params=params,
              analyticVars=AnalyticVars)
    out
  }
