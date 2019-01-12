#'
#'  fn.3dPlot.rotate
#'
#'  Create 3-dimensional data plot(s) that can be rotated
#'
#' @param doc A string documenting usage written to the list return, default is the function name
#' @param data: R matrix or data frame containing the data to be analyzed
#' @param GroupVar: name for variable defining grouping; a group variable must be specified
#' @param Groups: vector of values of group variable for which plots are to be done;
#'    "All": use all groups;" ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Selections: vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param ByGroup: if T, show scatterplot for each group for each selection of 3 variables
#'                       default is F
#' @param ptSize: size of plotted points, default is 5 (a larger value gives larger points)
#' @param Color: color(s) of plotted points; default is a vector
#'            red, black, blue, green, purple
#' @param folder: the folder to which one or more files with images will be saved;
#' default is " " (no files saved)
#' @param dsFile: the complete path to a file in folder to which each image will be saved;
#' if folder is not " ", this must be a valid path and file name (ends in .pdf for current function)
#`
#' @import MASS rgl scatterplot3d
#'
#' @section: Details
#' See the vignette for details on the use of colors.  The rotated 3d plot can be saved to a file
#' located at dsFile.  The code saves a file as a pdf; see the documentation for the function
#' rgl.postscript() for changing the format to postscript, eps, tex, or others.  Point sizes may
#' appear much larger in a saved file than on the monitor.
#'
#' @return A list with the following components:
#' \itemize{
#' \item{usage}{  A vector with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed}{  The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params}{  A vector with the values of the arguments ByGroup and ptRadius}
#' \item{groups}{  A vector (may be of length 1) with the value of the argument Groups}
#' \item{analyticVars}{  A vector with the value of the argument AnalyticVars}
#' \item{selections}{  A vector or matrix with the value of the argument Selections}
#' \item{colors}{  A vector with the value of the argument Color}
#' \item{location}{  The value of the parameter folder}
#' }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot.3d.rotate<-fn.3dPlot.rotate(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),
#'                                 AnalyticVars = analyticVars, Selections=analyticVars[1:3],ByGroup=T)
#' # two plots
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot.3d.rotate<-fn.3dPlot.rotate(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),
#'                                 AnalyticVars = analyticVars,
#'                                 Selections=rbind(analyticVars[1:3],analyticVars[2:4]))
#' @export

fn.3dPlot.rotate <-
  function(doc = "fn.3dPlot.rotate",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup = F,
           ptSize = 5,
           Colors = c("red","black","blue","green","purple"),
           folder = " ",
           dsFile
  )
  {
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
    #
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    #  check for number of colors specified
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
    #  add index to data.Used to specific color for plotting points in groups
    #
    if ((!ByGroup) & (Groups[1] != " "))  {
      n.group<-rep(0,length(groups))
      for (i in 1:length(groups))  {
        n.group<-nrow(data.Used[data.Used[,GroupVar]==groups[i],])
        if (i == 1) group.index<-rep(1,n.group)
        else  group.index<-c(group.index,rep(i,n.group))
      }
      data.Used<-cbind(data.Used,group.index=group.index)
    }
    #
    #  use first color if no grouping
    #
    if ((!ByGroup) & (Groups[1] == " "))
      data.Used<-cbind(data.Used,group.index=rep(1,nrow(data.Used)))
    #
    #  plot points
    if (!ByGroup) { # groups combined
      # create title with groups and colors
      if (Groups[1] != " ") {
        header<-paste(Groups[1],": ",Colors[1],sep="")
        for (i in 2:length(Groups))
          header<-paste(header,"  ",Groups[i],": ",Colors[i],sep="")
      }
      else  header <- " "
      #
      if (is.vector(Selections)) {
        plot.new()
        index <- is.na(data.Used[, Selections[1]]) | is.na(data.Used[,Selections[2]]) |
          is.na(data.Used[, Selections[3]])
         plot3d(data.Used[!index, Selections[1]], data.Used[!index,Selections[2]],
               data.Used[!index, Selections[3]], type="p", size=ptSize,
               xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
               col = Colors[data.Used[!index,"group.index"]],
               pch = 16, main=header)
        if (folder != " ") rgl.postscript(filename=dsFile, fmt="pdf")
       }  # end of code for Selections as vector
      #
      if (is.matrix(Selections)) {
        for (i in 1:nrow(Selections)) {
          plot.new()
          par(oma=rep(2,4))
          index <- is.na(data.Used[, Selections[i,1]]) | is.na(data.Used[,Selections[i,2]]) |
            is.na(data.Used[, Selections[i,3]])
          plot3d(data.Used[!index, Selections[i, 1]], data.Used[!index, Selections[i, 2]],
                 data.Used[!index, Selections[i, 3]], type="s", xlab = Selections[i, 1],
                 ylab = Selections[i, 2], zlab = Selections[i, 3],
                 col = Colors[data.Used[,"group.index"]], pch = 16, type="p", size=ptSize,
                 main=header)
          browser()
         }
      } # end of code for Selections as a matrix
    } # end of code for plot points with groups combined
    #
    if ((GroupVar[1] != " ") & (ByGroup)) { # plot points by group
      if (is.vector(Selections)) {
        for (i in 1:length(groups)) {
          win.graph()
          data.i<-data.Used[data.Used[,GroupVar]==groups[i],Selections]
          index <- is.na(data.i[, Selections[1]]) | is.na(data.i[,Selections[2]]) |
            is.na(data.i[, Selections[3]])
          plot3d(data.i[!index,], xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
                        col = Colors[1], pch = 16, type="p", size=ptSize,
                        main = paste(groups[i],": ",Selections[1]," ,", Selections[2], ",",
                                     Selections[3],sep=""))
          if (i < length(groups))  browser()
        }
      } # end of code for Selections as vector
      if (is.matrix(Selections)) {
        for (i in 1:nrow(Selections)) {
          for (j in 1:length(groups)) {
            win.graph()
            data.j<-data.Used[data.Used[,GroupVar]==groups[j],Selections[i,]]
            index <- is.na(data.j[, Selections[i,1]]) | is.na(data.j[,Selections[i,2]]) |
              is.na(data.j[, Selections[i,3]])
            plot3d(data.j[!index,], xlab = Selections[i, 1], ylab = Selections[i, 2],
                   zlab = Selections[i,3], col = Colors[1], pch = 16,
                   type="p", size=ptSize,
                   main = paste(groups[i],": ",Selections[i, 1], ",", Selections[i,2], ",",
                                       Selections[i, 3]))
            browser()
          }
        }
      } # end of code for Selections as a matrix
    } # end of plot points by group
    #
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    params<-list(ByGroup,ptSize)
    names(params)<-c("ByGroup","ptSize")
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep,]
    else dataNA <- NA
    #
    out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA=dataNA,
                params=params,
                groups=Groups,
                analyticVars=AnalyticVars,
                selections=Selections,
                colors=Colors,
                location=folder)
    out
  }
