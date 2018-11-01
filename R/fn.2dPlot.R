#' fn.2dPlot
#'
#' Create 2-dimensional data plot or plots. The function stops after producing each plot.  Enter c ("continue") at the prompt to get the next plot.  If this function is run using Rstudio, each plot appears in a separate window, not in the Rstudio plot pane.
#'
#' @param doc A string documenting use written to the output list; default is the function name
#' @param data R matrix or data frame containing the data to be analyzed
#' @param GroupVar name for the variable defining grouping, " " if no grouping
#' @param ID name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars names of two analytic variables to be shown in the plots, vector of length 2 or matrix with 2 columns;
#'     if a matrix, the set of plots is produced for each row
#' @param PlotByGroup default is T; if F, all groups are on each plot for a pair of variables
#' @param PlotPoints if T (the default), all points are plotted; if F, no points are plotted
#' @param LowessLine if T, a lowess line is plotted for each group; if F, no line is plotted
#' @param Lowess.f parameter for lowess()less than or equal to 1, defining the range of x-values used;
#'     if NA (the default), uses the default value of 0.67
#' @param KernelSmooth if T, a kernel smooth is plotted for each group; if F (the default), no kernel smooth is plotted
#' @param KernelWidth the proportion of the range of x-values used in the kernel smooth; default is 0.3
#' @param PlotEllipses if T, Gaussian confidence ellipses are plotted for each group; if F, no ellipses are plotted
#' @param Ellipses single value or vector of values with confidence values for the ellipses; default is c(0.95,0.99)
#' @param PlotHulls if T, the convex hull is drawn for each set of points; if F (the default), no hulls are drawn
#' @param PlotMedians if T, the code for each group is plotted at the median of the values for that group; default is F
#' @param Identify if T, user can identify points of interest in the plots; default is F
#' @param PlotColors: if T, colors are assigned to the groups
#' @param Colors: single value or vector of color names; if PlotByGroup = F, the vector must have the same number of colors as the number of groups
#' @param legendLoc: character, location of legend for a plot with points default is "topright", alternatives are combinations of "top", "bottom", "right", "left"
#' @param folder:  The path to the folder in which data frames will be saved; default is " "
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage: }{  A vector with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed: }{ The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params.numeric: }{ A vector with the values of the arguments Lowess.f and KernelWidth}
#' \item{params.grouping: }{ A character vector with the values of the arguments GroupVar and Groups}
#' \item{ellipse.pct: }{ The value of the argument Ellipses}
#' \item{analyticVars: }{ The value of the argument AnalyticVars}
#' \item{colors:}{  A vector with the value of the argument Color}
#' \item{data.check: }{ If Identify = T, a data frame with the information on user-identified points of interest}
#' \item{location:}{ The contents of the parameter folder}
#'  }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot, use of colors, and identifying points of interest.
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' #
#' #  plot four pairs of variables by source (default has four plots on a page)
#' plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = c("A","B"),
#'           AnalyticVars = rbind(analyticVars[1:2],analyticVars[c(1,3)], analyticVars[c(1,4)],
#'           analyticVars[2:3]), PlotEllipses=T, LowessLine=T)
#' #
#' #  plot one pair of variables with all sources on one plot
#' plot.2d <- fn.2dPlot(data = ObsidianSources, GroupVar = "Code", ID = "ID", Groups = "All",
#'           AnalyticVars =analyticVars[1:2], PlotByGroup=F, PlotColors=T, namesPlotEllipses=T, LowessLine=T)
#'
#' @import MASS
#'
#' @export
#'
fn.2dPlot <- function (doc = "fn.2dPlot",
                       data,
                       GroupVar,
                       ID,
                       Groups,
                       AnalyticVars,
                       PlotByGroup=T,
                       PlotPoints = T,
                       LowessLine=F,
                       Lowess.f=NA,
                       KernelSmooth=F,
                       KernelWidth=0.3,
                       PlotEllipses = F,
                       PlotHulls = F,
                       PlotMedians = F,
                        Ellipses = c(0.95, 0.99),
                       Identify=F,
                       PlotColors = F,
                       Colors=c("black","red","blue","green","purple"),
                       legendLoc = "topright",
                       folder=" ")
{
  if (Groups[1] != "All") {
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows,]
  }
  else data.Used <- data
  #
  dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with
  # no missing values
  for (i in 1:length(AnalyticVars))
    dataKeep[is.na(data.Used[,AnalyticVars[i]])] <- F
  #
  if (Groups[1] == "All")
    groups <- as.character(unique(data.Used[, GroupVar]))
  else groups <- as.character(Groups)
  #
  GroupIndex <- rep(NA, nrow(data.Used))
  for (i in 1:nrow(data.Used)) {
    for (j in 1:length(groups)) if (data.Used[i, GroupVar] == groups[j])
      GroupIndex[i] <- j
  }
  #
  #   if only one specified pair of variables, create matrix or data frame to use later code
  #   define number of pairs of variables to analyze
  #
  if (is.vector(AnalyticVars)) {  # create AnalyticVars as a matrix or data frame to use default code
    AnalyticVars<-rbind(AnalyticVars,AnalyticVars)
    n.pairs<-1
  }
  else n.pairs<-nrow(AnalyticVars)
  if (Identify)  data.check<-data.Used[1,]  # dummy row to set up information on
     # identified observations
     else  data.check <- NA
  #
  #   plots by Group
  #
  if (PlotByGroup) {
#    n.pages <- round(0.01+length(groups)/2, dig = 0)  # number of pages (rounds up with an odd number of groups)
    #
    fn.plot <- function(group,group.j, groupName) {
      temp <- data.Used[(data.Used[, GroupVar] == groups[group]),]
      # restrict to observtions with both variables not NA
      notNA <- !is.na(temp[,AnalyticVars[group.j,1]]) & !is.na(temp[,AnalyticVars[group.j,2]])
      temp <- temp[notNA,]
      # set up plot
      rangeX<-range(temp[,AnalyticVars[group.j,1]])
      rangeY<-range(temp[,AnalyticVars[group.j,2]])
      if (PlotEllipses) {
        for (j in 1:length(Ellipses)) {
          Covar <- var(temp[,AnalyticVars[group.j,]])
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,AnalyticVars[group.j,]],
                                                       2, mean, na.rm = T), level = Ellipses[j], npoints = 200)
          rangeX<-range(rangeX,Ellipse[,1])
          rangeY<-range(rangeY,Ellipse[,2])
        }
      }
      plot(type="n", x=rangeX, y=rangeY,xlab=AnalyticVars[group.j,1],ylab=AnalyticVars[group.j,2], main=paste("group",groupName))
      if (PlotEllipses) {
        Covar <- var(temp[,AnalyticVars[group.j,]])
        for (j in 1:length(Ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,AnalyticVars[group.j,]],
                                                       2, mean, na.rm = T), level = Ellipses[j], npoints = 200)
          lines(Ellipse, lty=1)
        }
      }
      #
      if (PlotPoints) {
        points(temp[,AnalyticVars[group.j,]])
        if ( Identify)  {
          index<-identify(x=temp[,AnalyticVars[group.j,1]],y=temp[,AnalyticVars[group.j,2]] ) # row numbers identified
          data.check<-rbind(data.check,temp[index,])  # add these rows to data.check
        }
      }
      if (LowessLine) {
        if (is.na(Lowess.f)) lowess.fit<-lowess(temp[,AnalyticVars[group.j,]])
        else  lowess.fit<-lowess(temp[,AnalyticVars[group.j,]],f=Lowess.f)
        lines(lowess.fit)
      }
      if (KernelSmooth) {
        kernel.fit<-ksmooth(x=temp[,AnalyticVars[group.j,1]],y=temp[,AnalyticVars[group.j,2]],"normal",
                            bandwidth=sum(range(temp[,AnalyticVars[group.j,]][,1])*c(-1,1))*KernelWidth)
        lines(kernel.fit)
      }
      if (PlotHulls)  {
        hull.pts <- chull(temp[,AnalyticVars[group.j],])
        hull.pts <- c(hull.pts, hull.pts[1])
        lines(temp[hull.pts,])
      }
      if (Identify)  data.check
    }
    par(mfrow = c(2, 2))
    i.plot<-0
    for (i.group in 1:length(groups)) {
       for (k in 1:n.pairs) {
        if (!Identify)  fn.plot(group=i.group,group.j=k,groupName=groups[i.group])
        if ( Identify)  data.check<-fn.plot(group=i.group,group.j=k,groupName=groups[i.group])
        i.plot <- i.plot + 1
        if ((i.plot == 4) | (i.plot == nrow(AnalyticVars))) {
        i.plot <- 0
        browser()
          }
       } # end of loop on k
    }  # end of loop on i.group
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
    for (j in 1:n.pairs) {
      # set up plot for this pair of variables
      par(mfrow=c(1,1))
      temp<-data.Used[,AnalyticVars[j,]]
      rangeX<-range(temp[,1])
      rangeY<-range(temp[,2])
      if (PlotEllipses) {
        for (k in 1:length(Ellipses)) {
          for (i.group in 1:length(groups)) {
            temp.i <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[j,]]
            Covar <- var(temp.i)
            Ellipse <- ellipse(x = Covar, centre = apply(temp.i,
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
      for (i.group in 1:length(groups)) {
        temp.i <- data.Used[data.Used[, GroupVar] == groups[i.group],]
        if (PlotMedians) {
          medians<- apply(temp.i[,AnalyticVars[j,]],2,median, na.rm = T)
          text(x = medians[1], y = medians[2], labels = groups[i.group], cex = 0.75, adj = 0.5)
        }  # end of code for PlotMediams
        if (PlotPoints) {points(x=temp.i[,AnalyticVars[j,1]],y=temp.i[,AnalyticVars[j,2]],pch=(i.group-1), col = Colors[i.group])
          if ( Identify)  {
            index<-identify(x=temp.i[,AnalyticVars[j,1]],y=temp.i[,AnalyticVars[j,2]]) # row numbers identified
            data.check<-rbind(data.check,temp.i[index,])  # add these rows to data.check
          }
        }
        #
        if (LowessLine) {
          if (is.na(Lowess.f)) lowess.fit<-lowess(x=temp.i[,AnalyticVars[j,1]],y=temp.i[,AnalyticVars[j,2]])
          else  lowess.fit<-lowess(x=temp.i[,AnalyticVars[j,1]],y=temp.i[,AnalyticVars[j,2]],f=Lowess.f)
          lines(lowess.fit,col=Colors[i.group])
        }
        if (KernelSmooth) {
          kernel.fit<-ksmooth(x=temp.i[,AnalyticVars[j,1]],y=temp.i[,AnalyticVars[j,2]],"normal",
                              bandwidth=sum(range(temp.i[,AnalyticVars[j,1]])*c(-1,1))*KernelWidth)
          lines(kernel.fit,col=Colors[i.group])
        }
        if (PlotHulls)  {
          hull.pts <- chull(temp.i[,AnalyticVars[j,]])
          hull.pts <- c(hull.pts, hull.pts[1])
          lines(temp.i[hull.pts,AnalyticVars[j,]],col=Colors[i.group])
        }
        if (PlotEllipses) {
          for (k in 1:length(Ellipses)) {
            Covar <- var(temp.i[,AnalyticVars[j,]])
            Ellipse <- ellipse(x = Covar, centre = apply(temp.i[,AnalyticVars[j,]],2, mean, na.rm = T),
                               level = Ellipses[k], npoints = 200)
            lines(Ellipse, lty=1,col=Colors[i.group])
          }
        }  # end of code to plot ellipses
      } # end of loop on i.group
      browser()  # pause to examine and save plot for jth set of variables
    }  # end of code for jth set of variables
  }  #  end of code for plots with all groups on one plot for each pair of variables
  #
  #  return documentation and results if identified points
  #
  if (Identify)  {
    data.check<-data.check[-1,]  #  remove dummy first row
    if (substr(folder,1,1) != " ")  write.csv(data.check[-1,], paste(folder,ds.identified,sep=""))
    #
    #  remove duplicated observations from data.check
    #
    if (Identify) {
      if (ID != " ") index<-duplicated(data.check[,ID])
      else  index<-duplicated(data.check[,c(GroupVar,AnalyticVars)])
      if (length(index) > 0)  data.check<-data.check[!index,]
      if (ID != " ") {
        index.ID<-order(data.check[,ID])
        data.check<-data.check[index.ID,]
      }
    }
  }
  fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
  params.numeric<-c(Lowess.f,KernelWidth)
  names(params.numeric)<-c("Lowess.f","KernelWidth")
  params.grouping<-list(GroupVar,Groups)
  names(params.grouping)<-c("GroupVar","Groups")
  if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep]
  else dataNA <- NA
  #
  list(       usage=fcn.date.ver,
              dataUsed=data.Used,
              dataNA=dataNA,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              ellipse.pct=Ellipses,
              data.check=data.check,
              location=folder)
}
