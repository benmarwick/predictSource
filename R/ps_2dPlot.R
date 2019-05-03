#' ps_2dPlot
#'
#' Create 2-dimensional data plot or plots.  By default, a page has panes in two rows and
#' two columns (but there is a new page for successive groups). The function stops after producing
#' each row of each plot.  Enter c ("continue") at the prompt to get the next plot.
#'
#' @param doc A string documenting use written to the output list; default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param GroupVar The name for the variable defining grouping, " " if no grouping
#' @param ID  The name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  A vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; grouping is required (cannot be " ").
#' @param AnalyticVars The names of two analytic variables to be shown in the plots, vector of length 2
#'  or matrix with 2 columns; if a matrix, the set of plots is produced for each row_
#' @param PlotByGroup Logical.  The default is TRUE; if FALSE, all groups are on each plot for a pair of variables
#' @param PlotPoints Logical.  If TRUE (the default), all points are plotted; if FALSE, no points are plotted
#' @param LowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param Lowess_f parameter for lowess() less than or equal to 1, defining the range of x-values used;
#'     if NA (the default), uses the default value of 0.67
#' @param KernelSmooth Logical.  If TRUE, a kernel smooth is plotted for each group;
#' if FALSE (the default), no kernel smooth is plotted
#' @param KernelWidth the proportion of the range of x-values used in the kernel smooth;
#' default is 0.3
#' @param PlotEllipses Logical.  If TRUE, Gaussian confidence ellipses are plotted for each group;
#' if F (the default), no ellipses are plotted
#' @param Ellipses single value or vector of values with confidence values for the ellipses; default is c(0_95,0_99)
#' @param PlotHulls if TRUE, the convex hull is drawn for each set of points; if FALSE (the default),
#' no hulls are drawn
#' @param PlotMedians if TRUE, the code for each group is plotted at the median of the values
#'  for that group; default is FALSE
#' @param Identify if TRUE, user can identify points of interest in the plots; default is FALSE
#' @param PlotColors  Logical.  If TRUE, colors are assigned to the groups
#' @param Colors single value or vector of color names; if PlotByGroup = FALSE,
#' the vector must have the same number of colors as the number of groups
#' @param legendLoc Character, location of legend for a plot with points;
#' default is "topright", alternatives are combinations of "top", "bottom", "right", "left"
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage: }{  A string with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed: }{ The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params: }{ A list with the values of the grouping, logical and numeric arguments}
#' \item{analyticVars: }{ The value of the argument AnalyticVars}
#' \item{colors:}{  A vector with the value of the argument Color}
#' \item{dataCheck: }{ If Identify = TRUE, a data frame with the information on user-identified points
#'  of interest; value is c(NA,NA) if no points are identified}
#' \item{location:}{ The path to a folder in which results will be saved}
#'  }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot, use of colors, and
#'   identifying points of interest.  If Identify=TRUE, execution stops after creating each
#'   bivariate plot for the user to identify any points of interest.
#'
#' @examples

#' #
#' #  plot two pair of variables by source (default has four plots on a page)
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plotVars<-rbind(analyticVars[1:2], analyticVars[c(1,5)])
#' plot_2d <- ps_2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = c("A","B"),
#'           AnalyticVars=plotVars, PlotByGroup=TRUE, PlotColors=TRUE, PlotEllipses=TRUE,
#'           LowessLine=TRUE)
#' #
#' #  plot one pair of variables with all sources on one plot
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' plot_2d <- ps_2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = "All",
#'           AnalyticVars =analyticVars[1:2], PlotByGroup=FALSE, PlotColors=TRUE, PlotEllipses=TRUE,
#'           LowessLine=TRUE)
#'
#' @import MASS  ellipse
#'
#' @export
#'
ps_2dPlot <- function (doc = "ps_2dPlot",
                       data,
                       GroupVar,
                       ID,
                       Groups,
                       AnalyticVars,
                       PlotByGroup=TRUE,
                       PlotPoints = TRUE,
                       LowessLine=FALSE,
                       Lowess_f=NA,
                       KernelSmooth=FALSE,
                       KernelWidth=0.3,
                       PlotEllipses = FALSE,
                       PlotHulls = FALSE,
                       PlotMedians = FALSE,
                       Ellipses = c(0.95, 0.99),
                       Identify=FALSE,
                       PlotColors = FALSE,
                       Colors=c("black","red","blue","green","purple"),
                       legendLoc = "topright",
                       folder=" ")
{
  if (Groups[1] != "All") {
    Use_rows <- (data[, GroupVar] %in% Groups)
    dataUsed <- data[Use_rows,]
  }
  else dataUsed <- data
  #
  # matrix to contain indices for observations with no missing values
  dataKeep <- rep(T, nrow(dataUsed))
  for (i in 1:length(AnalyticVars))
    dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- F
  #
  if (Groups[1] == "All")
    groups <- as.character(unique(dataUsed[, GroupVar]))
  else groups <- as.character(Groups)
  #
  #  vector to contain indices of groups, used in plotting
  GroupIndex <- rep(NA, nrow(dataUsed))
  for (i in 1:nrow(dataUsed)) {
    for (j in 1:length(groups)) if (dataUsed[i, GroupVar] == groups[j])
      GroupIndex[i] <- j
  }
  #
  #   define number of pairs of variables to analyze
  #
  if (is.vector(AnalyticVars)) {  # create AnalyticVars as a matrix or data frame to use default code
    AnalyticVars<-rbind(AnalyticVars,AnalyticVars)
    n_pairs<-1
  }
  else n_pairs<-nrow(AnalyticVars)
  #
  #  set up matrix to store information on points identified as of interest
  #
  if (Identify)  dataCheck<-dataUsed[1,]  # dummy row to set up information on
     # identified observations
     else  dataCheck <- c(NA, NA)
  #
  #   plots by Group
  #
  if (PlotByGroup) {
#    n_pages <- round(0_01+length(groups)/2, dig = 0)  # number of pages (rounds up with an odd number of groups)
    #
    fnPlot <- function(group,group_j, groupName) {
      temp <- dataUsed[(dataUsed[, GroupVar] == groups[group]),]
      # restrict to observtions with both variables not NA
      notNA <- !is.na(temp[,AnalyticVars[group_j,1]]) & !is.na(temp[,AnalyticVars[group_j,2]])
      temp <- temp[notNA,]
      # set up plot
      rangeX<-range(temp[,AnalyticVars[group_j,1]])
      rangeY<-range(temp[,AnalyticVars[group_j,2]])
      #
      #  modify ranges if necessary to account for plotting of confidence ellipses
      #
      if (PlotEllipses) {
        for (j in 1:length(Ellipses)) {
          Covar <- var(temp[,AnalyticVars[group_j,]])
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,AnalyticVars[group_j,]],
                             2, mean, na.rm = T), level = Ellipses[j], npoints = 200)
          rangeX<-range(rangeX,Ellipse[,1])
          rangeY<-range(rangeY,Ellipse[,2])
        }
      } # end of code for PlotEllipses=TRUE
      #
      #  set up plot for specified pair of variables and group
      #
      plot(type="n", x=rangeX, y=rangeY,xlab=AnalyticVars[group_j,1],
           ylab=AnalyticVars[group_j,2], main=paste("group",groupName))
      #
      #  if specified, plot confidence ellipses
      #
      if (PlotEllipses) {
        Covar <- var(temp[,AnalyticVars[group_j,]])
        for (j in 1:length(Ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,AnalyticVars[group_j,]],
                                                       2, mean, na.rm = T), level = Ellipses[j],
                             npoints = 200)
          lines(Ellipse, lty=1)
        }
      }  # end of code for PlotEllipses=TRUE
      #
      if (PlotPoints) {
        points(temp[,AnalyticVars[group_j,]])
        if ( Identify)  {
          index<-identify(x=temp[,AnalyticVars[group_j,1]],y=temp[,AnalyticVars[group_j,2]] ) # row numbers identified
          dataCheck<-rbind(dataCheck,temp[index,])  # add these rows to dataCheck
        }
      }  # end of code for PlotPoints = TRUE
      if (LowessLine) {
        if (is.na(Lowess_f)) lowess_fit<-lowess(temp[,AnalyticVars[group_j,]])
        else  lowess_fit<-lowess(temp[,AnalyticVars[group_j,]],f=Lowess_f)
        lines(lowess_fit)
      } # end of code for LowessLine = TRUE
      if (KernelSmooth) {
        kernel_fit<-ksmooth(x=temp[,AnalyticVars[group_j,1]],y=temp[,AnalyticVars[group_j,2]],"normal",
                            bandwidth=sum(range(temp[,AnalyticVars[group_j,]][,1])*c(-1,1))*KernelWidth)
        lines(kernel_fit)
      } # end of code for KernelSmooth = TRUE
      if (PlotHulls)  {
        hull_pts <- chull(temp[,AnalyticVars[group_j],])
        hull_pts <- c(hull_pts, hull_pts[1])
        lines(temp[hull_pts,])
      }  # end of code for KernelSmooth = TRUE
      if (Identify)  dataCheck  # return data frame dataCheck with identified points
    }  # end of code for fnPlot
    par(mfrow = c(2, 2))
    for (i_group in 1:length(groups)) {
      i_plot<-0  # initialize counter for number of plot panes
       for (k in 1:n_pairs) {
        if (!Identify)  fnPlot(group=i_group,group_j=k,groupName=groups[i_group])
        if ( Identify)  dataCheck<-fnPlot(group=i_group,group_j=k,groupName=groups[i_group])
        i_plot <- i_plot + 1
       if ((i_plot == 4) | (i_plot == nrow(AnalyticVars))) browser()  # allow user to look at page
       } # end of loop on k
      if (nrow(AnalyticVars) != 2) plot.new() # new page for next group
    }  # end of loop on i_group
  }  # end of code for plotting by group
  #
  #  all groups on one plot
  #
  if (!PlotByGroup) {
    if (length(Colors)==1)  Colors<-rep(Colors[1],length(groups))
    else {
      if (length(Colors) < length(groups))  stop("fewer colors specified than the number of groups")
    }
    #
    for (j in 1:n_pairs) {
      # set up plot for this pair of variables
      par(mfrow=c(1,1))
      temp<-dataUsed[,AnalyticVars[j,]]
      rangeX<-range(temp[,1])
      rangeY<-range(temp[,2])
      if (PlotEllipses) {
        for (k in 1:length(Ellipses)) {
          for (i_group in 1:length(groups)) {
            temp_i <- dataUsed[dataUsed[, GroupVar] == groups[i_group],AnalyticVars[j,]]
            Covar <- var(temp_i)
            Ellipse <- ellipse(x = Covar, centre = apply(temp_i,
                                                         2, mean, na.rm = T), level = Ellipses[k], npoints = 200)
            rangeX<-range(rangeX,Ellipse[,1])
            rangeY<-range(rangeY,Ellipse[,2])
          }
        }
      }  # end of loop on k to compute plot bounds
      if (!PlotMedians)  rangeX<-range(rangeX,rangeX[2]+0.1*sum(rangeX*c(-1,1)))
      # move right boundary of box to always allow plotting the legend
      plot(type="n", x=rangeX, y=rangeY,xlab=AnalyticVars[j,1],ylab=AnalyticVars[j,2])
      #
      #  add legend to plot in specified corner
      #
      if (!PlotMedians) {
        if (PlotColors)
          legend(x = legendLoc, legend = groups, pch = 0:(length(groups) - 1), col = Colors[1:length(groups)], bty = "n")
        else legend(x = legendLoc, legend = groups, pch = 0:(length(groups) - 1), bty = "n")
      }
      for (i_group in 1:length(groups)) {
        temp_i <- dataUsed[dataUsed[, GroupVar] == groups[i_group],]
        if (PlotMedians) {
          medians<- apply(temp_i[,AnalyticVars[j,]],2,median, na.rm = T)
          text(x = medians[1], y = medians[2], labels = groups[i_group], cex = 0.75, adj = 0.5)
        }  # end of code for PlotMediams
        if (PlotPoints) {points(x=temp_i[,AnalyticVars[j,1]],y=temp_i[,AnalyticVars[j,2]],pch=(i_group-1), col = Colors[i_group])
          if ( Identify)  {
            index<-identify(x=temp_i[,AnalyticVars[j,1]],y=temp_i[,AnalyticVars[j,2]]) # row numbers identified
            dataCheck<-rbind(dataCheck,temp_i[index,])  # add these rows to dataCheck
          }
        }
        #
        if (LowessLine) {
          if (is.na(Lowess_f)) lowess_fit<-lowess(x=temp_i[,AnalyticVars[j,1]],y=temp_i[,AnalyticVars[j,2]])
          else  lowess_fit<-lowess(x=temp_i[,AnalyticVars[j,1]],y=temp_i[,AnalyticVars[j,2]],f=Lowess_f)
          lines(lowess_fit,col=Colors[i_group])
        }
        if (KernelSmooth) {
          kernel_fit<-ksmooth(x=temp_i[,AnalyticVars[j,1]],y=temp_i[,AnalyticVars[j,2]],"normal",
                              bandwidth=sum(range(temp_i[,AnalyticVars[j,1]])*c(-1,1))*KernelWidth)
          lines(kernel_fit,col=Colors[i_group])
        }
        if (PlotHulls)  {
          hull_pts <- chull(temp_i[,AnalyticVars[j,]])
          hull_pts <- c(hull_pts, hull_pts[1])
          lines(temp_i[hull_pts,AnalyticVars[j,]],col=Colors[i_group])
        }
        if (PlotEllipses) {
          for (k in 1:length(Ellipses)) {
            Covar <- var(temp_i[,AnalyticVars[j,]])
            Ellipse <- ellipse(x = Covar, centre = apply(temp_i[,AnalyticVars[j,]],2, mean, na.rm = T),
                               level = Ellipses[k], npoints = 200)
            lines(Ellipse, lty=1,col=Colors[i_group])
          }
        }  # end of code to plot ellipses
      } # end of loop on i_group
      browser()  # pause to examine and save plot for jth set of variables
    }  # end of code for jth set of variables
  }  #  end of code for plots with all groups on one plot for each pair of variables
  #
  #  return documentation and results if identified points
  #
  if (Identify)  {
    if (nrow(dataCheck) == 1)  dataCheck <- NA
    if (nrow(dataCheck) > 1) {
      dataCheck<-dataCheck[-1,]  #  remove dummy first row
    #
    #  remove duplicated observations from dataCheck
    #
      if (ID != " ") index<-duplicated(dataCheck[,ID])
      else  index<-duplicated(dataCheck[,c(GroupVar,AnalyticVars)])
      if (length(index) > 0)  dataCheck<-dataCheck[!index,]
      if (ID != " ") {
        index_ID<-order(dataCheck[,ID])
        dataCheck<-dataCheck[index_ID,]
        }
      } # end of code for nrow(dataCheck) > 1
    } # end of code for Identify = TRUE
  #
  fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
  #
  smoothing<-c(Lowess_f,KernelWidth)
  names(smoothing)<-c("Lowess_f","Kernelwidth")
  params_numeric<-list(smoothing=smoothing,ellipse_pct=Ellipses)
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(PlotByGroup,PlotPoints,PlotEllipses,PlotHulls,PlotMedians,LowessLine,
                    KernelSmooth,Identify,PlotColors)
  names(params_logical)<-c("PlotByGroup","PlotPoints","PlotEllipses","PlotHulls","PlotMedians",
                           "LowessLine","KernelSmooth","Identify","PlotColors")
  params<-list(grouping=params_grouping,logical=params_logical,numeric=params_numeric)
  #
  if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
  else dataNA <- NA
  #
  list(       usage=fcnDateVersion,
              dataUsed=dataUsed,
              dataNA=dataNA,
              params=params,
              analyticVars=AnalyticVars,
              dataCheck=dataCheck,
              location=folder)
}
