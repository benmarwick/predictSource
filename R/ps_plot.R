#' ps_plot
#'
#' Create 2-dimensional scatter plot for one group: used by ps_2dPlot().
#'
#' @param data A data frame containing the data to be analyzed
#' @param useVars A vector of length 2: the names of two analytic variables to
#'  be shown in the plots
#' @param plotPoints Logical.  If TRUE, all points are plotted; if FALSE, no points are plotted
#' @param lowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param lowess_f A parameter for lowess() less than or equal to 1, defining the range of x-values used
#' @param kernelSmooth Logical.  If TRUE, a kernel smooth is plotted for each group
#' if FALSE, no kernel smooth is plotted
#' @param kernelWidth the proportion of the range of x-values used in the kernel smooth
#' @param plotEllipses Logical.  If TRUE, Gaussian confidence ellipses are plotted for each group;
#' if FALSE, no ellipses are plotted
#' @param ps_ellipses single value or vector of values with confidence values for the ellipses
#' @param plotHulls if TRUE, the convex hull is drawn for each set of points; if FALSE,
#' no hulls are drawn
#' @param ps_identify if TRUE, user can identify points of interest in the plots
#'
#' @return   If the user identifies points of interest:
#'  \itemize{
#' \item{dataCheck: }{ If ps_identify = TRUE, a data frame with the information on user-identified points
#'  of interest; value is c(NA,NA) if no points are identified}
#'  }
#'
#' @section Details:
#'  This function is used internally by the function ps_2dPlot.  The default values are those
#'  from ps_2dPlot.  data will contain a group code, a lab ID (if used in the data set), and all
#'  analytic variables, for display if the user identifies points of interest.
#'
#' @import MASS  ellipse
#'
#' @export
#'
ps_plot <- function(   data,
                       useVars,
                       plotPoints = PlotPoints,
                       lowessLine = LowessLine,
                       lowess_f = Lowess_f,
                       kernelSmooth = KernelSmooth,
                       kernelWidth = Kernelwidth,
                       plotHulls = PlotHulls,
                       plotEllipses = PlotEllipses,
                       ps_ellipses = Ellipses,
                       ps_identify = Identify) {
      #
      # set up plot
      rangeX<-range(data[,useVars[,1]])
      rangeY<-range(data[,useVars[,2]])
      #
      #  modify ranges if necessary to account for plotting of confidence ellipses
      #
      if (plotEllipses) {
        for (j in 1:length(ps_ellipses)) {
          Covar <- var(data[,useVars])
          Ellipse <- ellipse(x = Covar, centre = apply(data[,useVars], 2, mean, na.rm = T),
                             level = ps_ellipses[j], npoints = 200)
          rangeX<-range(rangeX,Ellipse[,1])
          rangeY<-range(rangeY,Ellipse[,2])
        }
      } # end of code for PlotEllipses=TRUE
      #
      #  set up plot for specified pair of variables and group
      #
      plot(type="n", x=rangeX, y=rangeY,xlab=useVars[1],
           ylab=useVars[2], main=paste("group",groupName))
      #
      #  if specified, plot confidence ellipses
      #
      if (plotEllipses) {
        Covar <- var(data[,useVars])
        for (j in 1:length(ps_ellipses)) {
          Ellipse <- ellipse(x = Covar, centre = apply(data[,useVars],2, mean, na.rm = T),
                             level = ps_ellipses[j], npoints = 200)
          lines(Ellipse, lty=1)
        }
      }  # end of code for plotEllipses=TRUE
      #
      if (plotPoints) {
        points(data[,useVars])
        if (ps_identify)  {
          index<-identify(x=data[,useVars],y=data[,useVars] )
            # row numbers identified
          dataCheck<-rbind(dataCheck,data[index,])  # add these rows to dataCheck
        }
      }  # end of code for plotPoints = TRUE
      if (lowessLine) {
        if (is.na(lowess_f)) lowess_fit<-lowess(data[,useVars])
        else  lowess_fit<-lowess(data[,useVars],f=lowess_f)
        lines(lowess_fit)
      }  # end of code for lowessLine = TRUE
      if (kernelSmooth) {
        kernel_fit<-ksmooth(x=data[,useVars[1]],
                            y=data[,useVars[2]],"normal",
                            bandwidth=sum(range(data[,useVars[1]])*c(-1,1))*kernelWidth)
        lines(kernel_fit)
      }
      if (plotHulls)  {
        hull_pts <- chull(data[,useVars])
        hull_pts <- c(hull_pts, hull_pts[1])
        lines(data[hull_pts,useVars])
      } #  end of code for plotHulls = TRUE
      if (ps_identify)  dataCheck  # return data frame dataCheck with identified points
         else  invisible()
    }
