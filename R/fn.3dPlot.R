#' fn.3dPlot
#'
#' Create 3-dimensional data plot. The function stops after producing each plot.  Enter c ("continue") at the prompt to get the next plot.  If this function is run using Rstudio, each plot appears in a separate window, not in the Rstudio plot pane.
#'
#' @param doc A string documenting use written to the output list; default is the function name
#' @param data R matrix or data frame containing the data to be analyzed
#' @param GroupVar name for variable defining grouping, " " if no grouping
#' @param Groups vector of values of group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars vector of names (character values) of analytic results
#' @param Selections vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param ByGroup if T, show scatterplot for each group for each selection of 3 variables
#' @param PlotMedians  If T, plot the medians
#' @param Colors colors of plotted points, default is a vector for showing several groups on one plot
#' @param SymbolSize value at most 1, smaller value gives smaller diameter points
#'
#'#' @return   A list with the following components:
#'  \itemize{
#' \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params:}{  A vector with the values of the arguments ByGroup and SymbolSize}
#' \item{groups:}{  A vector (may be of length 1) with the value of the argument Groups}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#' \item{colors:}{  A vector with the value of the argument Color}
#' }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot, specification of the argument Selections
#' as a matrix or data frame, and use of colors.
#'
#' @import MASS scatterplot3d
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot3d<-fn.3dPlot(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"), AnalyticVars=analyticVars,
#'                   Selections=rbind(analyticVars[1:3],analyticVars[2:4]))
#'
#' @export

fn.3dPlot <-
  function(doc = "fn.3dPlot version 0.1",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup = FALSE,
           PlotMedians = FALSE,
           Colors = c("red","black","blue","green","purple"),
           SymbolSize = 0.7) {

    library(scatterplot3d)
#    library(rgl)
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    }
    else if (GroupVar[1] == " ")
      data.Used <- data[, AnalyticVars]
    else data.Used <- data[, c(GroupVar, AnalyticVars)]
    #
    dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(data.Used[,AnalyticVars[i]])] <- F
    data.Used <- data.Used[dataKeep,]  # remove observations with any analysis variable with missing data
    #
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    #  check for number of colors specified
    #
    if (!ByGroup)
      if (length(Colors) < length(groups))  stop("too few colors specified")
    #
    #  sort data.Used on grouping variable to assign colors to points
    #
    if (GroupVar[1] != " ") {
      index<-order(data.Used[,GroupVar])
      data.Used<-data.Used[index,]
    }
    #
    #  add index to data.Used to specific color for plotting points in groups
    #
    if (!ByGroup)  {
      n.group<-rep(0,length(groups))
      for (i in 1:length(groups))  {
        n.group<-nrow(data.Used[data.Used[,GroupVar]==groups[i],])
        if (i == 1) group.index<-rep(1,n.group)
        else  group.index<-c(group.index,rep(i,n.group))
      }
      data.Used<-cbind(data.Used,group.index=group.index)
    }
    if (!PlotMedians) {
      #  plot points
      if ((GroupVar[1] == " ") | (ByGroup != T)) {
        if (is.vector(Selections)) {
          index <- is.na(data.Used[, Selections[1]]) | is.na(data.Used[,Selections[2]]) |
                      is.na(data.Used[, Selections[3]])
          scatterplot3d(data.Used[!index, Selections[1]], data.Used[!index, Selections[2]],
                        data.Used[!index, Selections[3]], xlab = Selections[1],
                        ylab = Selections[2], zlab = Selections[3], color = Colors[data.Used[,"group.index"]],
                        pch = 16, cex.symbols = SymbolSize, main = paste(Selections[1],
                                                                         ",", Selections[2], ",", Selections[3]))
          legend("topright",legend=groups,pch=16,col=Colors[1:length(groups)])
          browser()
        }
        if (is.matrix(Selections)) {
          for (i in 1:nrow(Selections)) {
            win.graph()
            index <- is.na(data.Used[, Selections[i,1]]) | is.na(data.Used[,Selections[i,2]]) |
              is.na(data.Used[, Selections[i,3]])
            scatterplot3d(data.Used[!index, Selections[i, 1]],data.Used[!index, Selections[i, 2]],
                           data.Used[!index, Selections[i, 3]], xlab = Selections[i, 1],
                          ylab = Selections[i, 2], zlab = Selections[i, 3],
                          color = Colors[data.Used[,"group.index"]], pch = 16, cex.symbols = SymbolSize,
                          main = paste(Selections[i, 1], ",", Selections[i,2], ",", Selections[i, 3]))
            legend("topright",legend=groups,pch=16,col=Colors[1:length(groups)])
            browser()
          }
        }
      } # end of plot points for groups combined
      #
      if ((GroupVar[1] != " ") & (ByGroup)) { # plot points by group
        if (is.vector(Selections)) {
          for (i in 1:length(groups)) {
            win.graph()
            data.i<-data.Used[data.Used[,GroupVar]==groups[i],Selections]
            index <- is.na(data.i[, Selections[i,1]]) | is.na(data.i[,Selections[i,2]]) |
              is.na(data.i[, Selections[i,3]])
            scatterplot3d(data.i[!index,], xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
                          color = Colors[1], pch = 16, cex.symbols = SymbolSize,
                          main = paste(groups[i],": ",Selections[1]," ,", Selections[2], ",", Selections[3],sep=""))
            browser()
          }
        }
        if (is.matrix(Selections)) {
          for (i in 1:nrow(Selections)) {
            for (j in 1:length(groups)) {
              win.graph()
              data.j<-data.Used[data.Used[,GroupVar]==groups[j],Selections[i,]]
              index <- is.na(data.j[, Selections[i,1]]) | is.na(data.j[,Selections[i,2]]) |
                is.na(data.j[, Selections[i,3]])
              scatterplot3d(data.j[!index,], xlab = Selections[i, 1], ylab = Selections[i, 2],
                            zlab = Selections[i,3], color = Colors[1], pch = 16, cex.symbols = SymbolSize,
                            main = paste(groups[i],": ",Selections[i, 1], ",", Selections[i,2], ",",
                                         Selections[i, 3]))
              browser()
            }
          }
        }
      } # end of plot points by group
    }
    if ((PlotMedians) & (GroupVar != " ")) {
      # plot medians
      if (is.vector(Selections)) {
        win.graph()
        medians<-matrix(NA,nrow=length(groups),ncol=3)
        for (i in 1:length(groups)) {
          data.i<-data.Used[data.Used[,GroupVar]==groups[i],Selections]
          index <- is.na(data.i[, Selections[1]]) | is.na(data.i[,Selections[2]]) |
            is.na(data.i[, Selections[3]])
          medians[i,]<-apply(data.i[!index,],2,median)
        }
        scatterplot3d(medians, xlab = Selections[1],
                      ylab = Selections[2], zlab = Selections[3],
                      color = "black", pch = groups, cex.symbols = SymbolSize,
                      main = paste("group medians:",Selections[1], ",", Selections[2], ",", Selections[3]))
      }
      if (is.matrix(Selections) | is.data.frame(Selections)) {
        for (i in 1:nrow(Selections)) {
          medians<-matrix(NA,nrow=length(groups),ncol=3)
          for (j in 1:length(groups)) {
            data.j <- data.Used[data.Used[, GroupVar] == groups[j],Selections[i,] ]
            index <- is.na(data.j[, Selections[i,1]]) | is.na(data.j[,Selections[i,2]]) |
              is.na(data.j[, Selections[i,3]])
            medians[j,]<-apply(data.j[!index,],2,median)
          }
          win.graph()
          scatterplot3d(medians, xlab = Selections[i, 1], ylab = Selections[i, 2],
                        zlab = Selections[i, 3], color = "black",
                        pch = groups, cex.symbols = SymbolSize, main = paste("group medians:",
                              Selections[i,1], ",", Selections[i,2], ",", Selections[i,3]))
          browser()
        }
      }
    }
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    params<-list(ByGroup, PlotMedians, SymbolSize)
    names(params)<-c("ByGroup","PlotMedians", "SymbolSize")
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep]
    else dataNA <- NA
    #
    out<-list(usage=fcn.date.ver,
              dataUsed=data.Used,
              dataNA=dataNA,
              params=params,
              groups=Groups,
              analyticVars=AnalyticVars,
              colors=Colors)
    out

  }
