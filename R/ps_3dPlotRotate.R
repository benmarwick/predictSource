#'
#'  ps_3dPlotRotate
#'
#'  Create 3-dimensional data plot(s) that can be rotated
#'
#' @param doc A string documenting usage written to the list return, default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param GroupVar The name for variable defining grouping; a group variable must be specified
#' @param Groups A vector of values of group variable for which plots are to be done;
#'    "All": use all groups;" ": no grouping
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param Selections A vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param ByGroup  Logical. If TRUE, show scatterplot for each group for each selection of 3 variables;
#'                       default is FALSE
#' @param ptSize  The size of plotted points, default is 5 (a larger value gives larger points)
#' @param Colors A vector with the color(s) of plotted points; default is a vector
#'            red, black, blue, green, purple
#' @param folder The folder to which one or more files with images will be saved;
#' default is " " (no files saved)
#' @param dsFile The complete path to a file in folder to which each image will be saved;
#' if folder is not " ", this must be a valid path and file name (ends in _pdf for current function)
#`
#' @import MASS rgl scatterplot3d
#'
#' @section: Details:
#' See the vignette for details on the use of colors.  The rotated 3d plot can be saved to a file
#' located at dsFile.  The code saves a file as a pdf; see the documentation for the function
#' rgl.postscript() for changing the format to postscript, eps, tex, or others.  Point sizes may
#' appear much larger in a saved file than on the monitor.
#'
#' @return A list with the following components:
#' \itemize{
#' \item{usage}{  A string with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed}{  The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params}{  A list with the values of the grouping, logical, numerical, and Color arguments}
#' \item{analyticVars}{  A vector with the value of the argument AnalyticVars}
#' \item{selections}{  A vector or matrix with the value of the argument Selections}
#' \item{location}{  The value of the argument folder}
#' }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot_3d_rotate<-ps_3dPlotRotate(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),
#'               AnalyticVars = analyticVars, Selections=analyticVars[1:3],ByGroup=TRUE)
#' # two plots
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot_3d_rotate<-ps_3dPlotRotate(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),
#'                                 AnalyticVars = analyticVars,
#'                                 Selections=rbind(analyticVars[1:3],analyticVars[2:4]))
#' @export

ps_3dPlotRotate <-
  function(doc = "ps_3dPlotRotate",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup = FALSE,
           ptSize = 5,
           Colors = c("red","black","blue","green","purple"),
           folder = " ",
           dsFile
  )
  {
  #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows, c(GroupVar, AnalyticVars)]
    }
    else if (GroupVar[1] == " ")
      dataUsed <- data[, AnalyticVars]
    else dataUsed <- data[, c(GroupVar, AnalyticVars)]
    #
    dataKeep <- rep(T, nrow(dataUsed)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- F
    #
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <- as.character(unique(dataUsed[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    #  check for number of colors specified
    #
    if (!ByGroup)
      if (length(Colors) < length(groups))  stop("too few cols specified")
    #
    #  sort dataUsed on grouping variable to assign cols to points
    #
    if (GroupVar[1] != " ") {
      index<-order(dataUsed[,GroupVar])
      dataUsed<-dataUsed[index,]
    }
    #
    #  add index to dataUsed to specific color for plotting points in groups
    #
    if ((!ByGroup) & (Groups[1] != " "))  {
      n_group<-rep(0,length(groups))
      for (i in 1:length(groups))  {
        n_group<-nrow(dataUsed[dataUsed[,GroupVar]==groups[i],])
        if (i == 1) group_index<-rep(1,n_group)
        else  group_index<-c(group_index,rep(i,n_group))
      }
      dataUsed<-cbind(dataUsed,group_index=group_index)
    }
    #
    #  use first color if no grouping
    #
    if ((!ByGroup) & (Groups[1] == " "))
      dataUsed<-cbind(dataUsed,group_index=rep(1,nrow(dataUsed)))
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
        index_na <- is.na(dataUsed[, Selections[1]]) | is.na(dataUsed[,Selections[2]]) |
          is.na(dataUsed[, Selections[3]])
         plot3d(dataUsed[!index_na, Selections[1:3]], type="p", size=ptSize,
               xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
               col = Colors[dataUsed[!index,"group_index"]],
               pch = 16, main=header)
        if (folder != " ") rgl_postscript(filename=dsFile, fmt="pdf")
       }  # end of code for Selections as vector
      #
      if (is.matrix(Selections)) {
        for (i in 1:nrow(Selections)) {
          plot.new()
          par(oma=rep(2,4))
          index_na <- is.na(dataUsed[, Selections[i,1]]) | is.na(dataUsed[,Selections[i,2]]) |
            is.na(dataUsed[, Selections[i,3]])
          plot3d(dataUsed[!index_na, Selections[i, 1:3]], xlab = Selections[i, 1],
                 ylab = Selections[i, 2], zlab = Selections[i, 3],
                 col = Colors[dataUsed[,"group_index"]], pch = 16, type="p", size=ptSize,
                 main=header)
          browser()
         }
      } # end of code for Selections as a matrix
    } # end of code for plot points with groups combined
    #
    if ((GroupVar[1] != " ") & (ByGroup)) { # plot points by group
      if (is.vector(Selections)) {
        for (i in 1:length(groups)) {
          dev.new()
          data_i<-dataUsed[dataUsed[,GroupVar]==groups[i],Selections]
          index_na <- is.na(data_i[, Selections[1]]) | is.na(data_i[,Selections[2]]) |
            is.na(data_i[, Selections[3]])
          plot3d(data_i[!index_na,], xlab = Selections[1], ylab = Selections[2], zlab = Selections[3],
                        col = Colors[1], pch = 16, type="p", size=ptSize,
                        main = paste(groups[i],": ",Selections[1]," ,", Selections[2], ",",
                                     Selections[3],sep=""))
          if (i < length(groups))  browser()
        }
      } # end of code for Selections as vector
      if (is.matrix(Selections)) {
        for (i in 1:nrow(Selections)) {
          for (j in 1:length(groups)) {
            dev.new()
            data_j<-dataUsed[dataUsed[,GroupVar]==groups[j],Selections[i,]]
            index_na <- is.na(data_j[, Selections[i,1]]) | is.na(data_j[,Selections[i,2]]) |
              is.na(data_j[, Selections[i,3]])
            plot3d(data_j[!index_na,], xlab = Selections[i, 1], ylab = Selections[i, 2],
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
    fcnDateVersion<-c(doc,date(),R.Version()$version.string)
    #
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-ByGroup
    names(params_logical)<-"ByGroup"
    params_numeric<-ptSize
    names(params_numeric)<-"ptSize"
    params<-list(grouping=params_grouping,logical=params_logical,numeric=params_numeric,colors=Colors)
    #
    if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
    else dataNA <- NA
    #
    out<-list(usage=fcnDateVersion,
                dataUsed=dataUsed,
                dataNA=dataNA,
                params=params,
                groups=Groups,
                analyticVars=AnalyticVars,
                selections=Selections,
                colors=Colors,
                location=folder)
    out
  }
