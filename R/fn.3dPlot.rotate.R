#'
#'  fn.3dPlot.rotate
#'
#'  create 3-dimensional data plot(s) that can be rotated
#'
#' @param doc A string documenting usage written to the list return, default is the function name
#' @param data: R matrix or data frame containing the data to be analyzed
#' @param GroupVar: name for variable defining grouping, " " if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done;
#'    "All": use all groups;" ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Selections: vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param ByGroup: if T, show scatterplot for each group for each selection of 3 variables
#' @param Color: color of plotted points
#' @param folder: default is " ", when no rotated 3-d plot is saved to a file
#'      if a rotated 3-d plot is to be saved, the path (ending in \\) to the folder in which the file is saved
#' @param ds.3dPlot: if folder != " ",the file name (with no extension) in which the plot is saved
#' @param extension: if folder != " ", the format in which the file should be saved;
#'        options are pdf ("pdf", the default) and postscript (value is "ps")
#'
#' @import MASS rgl
#'
#' @section: Details
#' \itemize{
#' \item{"x"}{If the plots are not to be saved to a file, the function stops after producing each plot.
#'   Close the plot window, then enter c ("continue") at the prompt to get the next plot.
#'   If this function is run using Rstudio, each plot appears in a separate window,
#'   not in the Rstudio plot pane; this window may appear behind the Rstudio window.}
#'
#' \item{"x"}{If the plots are to be saved to a file, the function also stops after producing each plot.
#'   Rotate the plot as desired, then enter c at the prompt, so that the function will save
#'   the file in the desired format.}
#'
#' \item{"x"}{If more than one plot is to be produced (the argument Selections is a matrix),
#'   close the rgl plot window before entering c to go to the next plot.  In this case, if
#'   plots are saved to files, the successive file names are ds.3dPlot1, ds.3dPlot2,....}
#'
#' \item{"x"}{If only one group is shown, the points have the color of the first color specified.  For multiple
#'   groups, the colors are used in the order specified.}
#'   }
#'
#' @return A list with the following components:
#' \itemize{
#' \item{"usage"}{a vector with the contents of the argument doc, the date run, the version of R used}
#' \item{"dataUsed"}{the contents of the argument data restricted to the groups used}
#' \item{"params"}{a vector with the values of the arguments ByGroup and SymbolSize}
#' \item{"groups"}{a vector (may be of length 1) with the value of the argument Groups}
#' \item{"analyticVars"}{a vector with the value of the argument AnalyticVars}
#' \item{"selections"}{a vector or matrix with the value of the argument Selections}
#' \item{"colors"}{a vector with the value of the argument Color}
#' \item{"file"}{if folder != " ": the path and file name ds.3dPlot}
#' }
#'
#' @export

fn.3dPlot.rotate <-
  function(doc = "fn.3dPlot.rotate version 0.1",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup,
           Color = c("red","black","blue","green","purple"),
           folder = " ",
           ds.3dPlot,
           extension="pdf") {
#
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    }
    else if (GroupVar[1] == " ")
      data.Used <- data[, AnalyticVars]
    else data.Used <- data[, c(GroupVar, AnalyticVars)]
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    #  check for number of cols specified
    #
    if (!ByGroup)
      if (length(Colors) < length(groups))  stop("too few cols specified")
    #
    #  sort data.Used on grouping variable to assign cols to points
    #
    if (GroupVar[1] != " ") {
      index<-order(data.Used[,GroupVar])
      data.Used<-data.Used[index,]
    }
    #
    #  add index to data.Used to specific col for plotting points in groups
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
    #  plot points
    if (!ByGroup) {
      # create title with groups and colors
      header<-paste(Groups[1],": ",Colors[1],sep="")
      for (i in 2:length(Groups))
        header<-paste(header,"  ",Groups[i],": ",Colors[i],sep="")
      if (is.vector(Selections)) {
        plot3d(data.Used[, Selections[1]], data.Used[,Selections[2]], data.Used[, Selections[3]],
               xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
               col = Colors[data.Used[,"group.index"]],
               pch = 16, cex.symbols = SymbolSize, main = header)
        browser()
        if (folder!=" ") {
          rgl.postscript(filename=paste(folder,ds.3dPlot,".",extension,sep=""),
                         fmt=extension,drawText=T)
          browser()
        }
      }
      if (is.matrix(Selections)) {
        for (i in 1:nrow(Selections)) {
          plot.new()
          par(oma=rep(2,4))
          plot3d(data.Used[, Selections[i, 1]], data.Used[, Selections[i, 2]],
                 data.Used[, Selections[i, 3]], xlab = Selections[i, 1],
                 ylab = Selections[i, 2], zlab = Selections[i, 3],
                 col = Colors[data.Used[,"group.index"]], pch = 16, cex.symbols = SymbolSize,
                 main=header)
          browser()
          if (folder!=" ") {
            rgl.postscript(filename=paste(folder,ds.3dPlot,i,".",extension,sep=""),
                           fmt=extension,drawText=T)
            browser()
          }
        }
      }
    }
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    params<-list(ByGroup,SymbolSize)
    names(params)<-c("ByGroup","SymbolSize")
    #
    if (folder == " ")
      out<-list(usage=fcn.date.ver,
              dataUsed=data.Used,
              params=params,
              groups=Groups,
              analyticVars=AnalyticVars,
              selections=Selections,
              colors=Colors)
    if (folder != " ")
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                params=params,
                groups=Groups,
                analyticVars=AnalyticVars,
                selections=Selections,
                colors=Colors,
                file=paste(folder,ds.3dPlot,".",extension,sep=""))
    out
  }
