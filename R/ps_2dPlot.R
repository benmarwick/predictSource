#' ps_plot
#'
#' Create 2-dimensional scatter plot for one group: used by ps_2dPlot().
#'
#' @param data A matrix or data frame containing the data to be analyzed
#' @param groupVar The name for the variable defining grouping
#' @param analyticVars The names of two analytic variables to be shown in the plots, vector of length 2
#'  or matrix with 2 columns; if a matrix, the set of plots is produced for each row_
#' @param plotByGroup Logical.  The default is TRUE; if FALSE, all groups are on each plot for a pair of variables
#' @param plotPoints Logical.  If TRUE (the default), all points are plotted; if FALSE, no points are plotted
#' @param lowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param lowess_f parameter for lowess() less than or equal to 1, defining the range of x-values used;
#'     if NA (the default), uses the default value of 0.67
#' @param kernelSmooth Logical.  If TRUE, a kernel smooth is plotted for each group;
#' if FALSE (the default), no kernel smooth is plotted
#' @param kernelWidth the proportion of the range of x-values used in the kernel smooth;
#' default is 0.3
#' @param plotEllipses Logical.  If TRUE, Gaussian confidence ellipses are plotted for each group;
#' if F (the default), no ellipses are plotted
#' @param ellipses single value or vector of values with confidence values for the ellipses; default is c(0_95,0_99)
#' @param plotHulls if TRUE, the convex hull is drawn for each set of points; if FALSE (the default),
#' no hulls are drawn
#' @param PlotMedians if TRUE, the code for each group is plotted at the median of the values
#'  for that group; default is FALSE
#' @param identify if TRUE, user can identify points of interest in the plots; default is FALSE
#'
#' @return   If the user identifies points of interest:
#'  \itemize{
#' \item{dataCheck: }{ If ps_identify = TRUE, a data frame with the information on user-identified points
#'  of interest; value is c(NA,NA) if no points are identified}
#'  }
#'
#' @section Details:
#'  This function is used internally by the functions ps_2dPlot and
#'
#' @examples

#'
#'
#' @import MASS  ellipse
#'
#' @export
#'
ps_plot <- function(   data = dataUsed,
                       groupVar = GroupVar,
                       analyticVars = AnalyticVars,
                       ps_groups = groups,
                       ps_group,
                       ps_group_j,
                       groupName,
                       plotEllipses = PlotEllipses,
                       ps_ellipses = Ellipses,
                       plotPoints = PlotPoints,
                       lowessLine = LowessLine,
                       lowess_f = Lowess_f,
                       kernelSmooth = KernelSmooth,
                       kernelWidth = Kernelwidth,
                       plotHulls = PlotHulls,
                       ps_identify = Identify) {
      temp <- data[(data[, groupVar] == ps_groups[ps_group]),]
      # restrict to observtions with both variables not NA
      notNA <- !is.na(temp[,analyticVars[ps_group_j,1]]) & !is.na(temp[,analyticVars[ps_group_j,2]])
      temp <- temp[notNA,]
      # set up plot
      rangeX<-range(temp[,analyticVars[ps_group_j,1]])
      rangeY<-range(temp[,analyticVars[ps_group_j,2]])
      #
      #  modify ranges if necessary to account for plotting of confidence ellipses
      #
      if (plotEllipses) {
        for (j in 1:length(ps_ellipses)) {
          Covar <- var(temp[,analyticVars[ps_group_j,]])
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,analyticVars[ps_group_j,]],
                             2, mean, na.rm = T), level = ps_ellipses[j], npoints = 200)
          rangeX<-range(rangeX,Ellipse[,1])
          rangeY<-range(rangeY,Ellipse[,2])
        }
      } # end of code for PlotEllipses=TRUE
      #
      #  set up plot for specified pair of variables and group
      #
      plot(type="n", x=rangeX, y=rangeY,xlab=analyticVars[ps_group_j,1],
           ylab=analyticVars[ps_group_j,2], main=paste("group",groupName))
      #
      #  if specified, plot confidence ellipses
      #
      if (plotEllipses) {
        Covar <- var(temp[,analyticVars[ps_group_j,]])
        for (j in 1:length(ps_ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(temp[,analyticVars[ps_group_j,]],
                                                       2, mean, na.rm = T), level = ps_ellipses[j],
                             npoints = 200)
          lines(Ellipse, lty=1)
        }
      }  # end of code for plotEllipses=TRUE
      #
      if (plotPoints) {
        points(temp[,analyticVars[ps_group_j,]])
        if (ps_identify)  {
          index<-identify(x=temp[,analyticVars[ps_group_j,1]],y=temp[,analyticVars[ps_group_j,2]] )
            # row numbers identified
          dataCheck<-rbind(dataCheck,temp[index,])  # add these rows to dataCheck
        }
      }  # end of code for plotPoints = TRUE
      if (lowessLine) {
        if (is.na(lowess_f)) lowess_fit<-lowess(temp[,analyticVars[ps_group_j,]])
        else  lowess_fit<-lowess(temp[,analyticVars[ps_group_j,]],f=lowess_f)
        lines(lowess_fit)
      }  # end of code for lowessLine = TRUE
      if (kernelSmooth) {
        kernel_fit<-ksmooth(x=temp[,analyticVars[ps_group_j,1]],
                            y=temp[,analyticVars[ps_group_j,2]],"normal",
                            bandwidth=sum(range(temp[,analyticVars[ps_group_j,]][,1])*c(-1,1))*kernelWidth)
        lines(kernel_fit)
      }
      if (plotHulls)  {
        hull_pts <- chull(temp[,analyticVars[ps_group_j],])
        hull_pts <- c(hull_pts, hull_pts[1])
        lines(temp[hull_pts,])
      } #  end of code for plotHulls = TRUE
      if (ps_identify)  dataCheck  # return data frame dataCheck with identified points
    }
