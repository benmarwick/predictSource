#' ps_plot
#'
#' Create 2-dimensional scatter plot for one group: used by ps_2dPlot().  This is an internal function.
#'
#' @keywords internal
#'
#' @param data A data frame containing the data to be analyzed
#' @param useVars A vector of length 2: the names of two analytic variables to
#'  be shown in the plots
#' @param ps_groupVar Character.  Optional name of the grouping variable.
#' @param ps_byGroup Logical.  If TRUE, plots are being created by group.
#' @param ps_plotAllGroups  Logical.  If TRUE, all groups are one one plot, with identifying information
#' @param plotPoints Logical.  If TRUE, all points are plotted; if FALSE, no points are plotted
#' @param lowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param lowess_f A parameter for lowess() less than or equal to 1, defining the range of x-values used
#' @param kernelSmooth Logical.  If TRUE, a kernel smooth is plotted for each group;
#' if FALSE, no kernel smooth is plotted
#' @param kernelWidth the proportion of the range of x-values used in the kernel smooth
#' @param locPoly Logical.  If TRUE, fit a local linear polynomial with a bandwidth selected
#' using the data.  Currently not used.
#' @param plotEllipses Logical.  If TRUE, Gaussian confidence ellipses are plotted for each group;
#' if FALSE, no ellipses are plotted
#' @param ps_ellipses single value or vector of values with confidence values for the ellipses
#' @param plotHulls  Logical. If TRUE, the convex hull is drawn for each set of points; if FALSE,
#' no hulls are drawn
#' @param plotMedians  Logical. If TRUE, the median value for each group is plotted with the
#' code for the group
#' @param groupIndex  Integer.  Used to specify the plotting character and color for multiple
#' groups shown on one plot.
#' @param ps_colors  Character vector of plotting colors for multiple groups shown on one plot
#' @param ps_legend  Character, specifying location of legend with multiple groups shown on one plot
#' @param ps_identify Logical. If TRUE, user can identify points of interest in the plots
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
#'  analytic variables, for display if the user identifies points of interest.  If plotMedians is TRUE,
#'  the convex hull and median of the points are plotted for each group.  The package fANACOVA (required
#'  to use the function lowess.as()) was not available for the current version of R.
#'
#' @import MASS graphics stats  grDevices
#'
#' @export
#'
ps_plot <- function(   data,
                       ps_groupVar,
                       ps_byGroup,
                       ps_plotAllGroups,
                       useVars,
                       plotPoints,
                       lowessLine,
                       lowess_f,
                       plotMedians,
                       kernelSmooth,
                       kernelWidth,
                       locPoly,
                       plotHulls,
                       plotEllipses,
                       ps_ellipses,
                       groupIndex,
                       ps_colors,
                       ps_legend,
                       ps_identify) {
      #
      # set up plot
      plot.new()
      rangeX<-range(data[,useVars[1]])
      rangeY<-range(data[,useVars[2]])
      #
      if (!ps_plotAllGroups) { # plot does not identify groups on a single plot
      #
      #  modify ranges if necessary to account for plotting of confidence ellipses
      #
      if (plotEllipses) {
        for (j in 1:length(ps_ellipses)) {
          Covar <- var(data[,useVars])
          Ellipse <- ellipse::ellipse(x = Covar, centre = apply(data[,useVars], 2, mean, na.rm = T),
                             level = ps_ellipses[j], npoints = 200)
          rangeX<-range(rangeX,Ellipse[,1])
          rangeY<-range(rangeY,Ellipse[,2])
        }
      } # end of code for PlotEllipses=TRUE
      #
      #  set up plot for specified pair of variables and group
      #
      if (!ps_byGroup) plot(type="n", x=rangeX, y=rangeY, xlab=useVars[1], ylab=useVars[2])
      if ( ps_byGroup) plot(type="n", x=rangeX, y=rangeY, xlab=useVars[1], ylab=useVars[2],
                            main=paste("Group",data[1,ps_groupVar]))
      #
      #  if specified, plot confidence ellipses
      #
      if (plotEllipses) {
        Covar <- var(data[,useVars])
        for (j in 1:length(ps_ellipses)) {
          Ellipse <- ellipse::ellipse(x = Covar, centre = apply(data[,useVars],2, mean, na.rm = T),
                             level = ps_ellipses[j], npoints = 200)
          lines(Ellipse, lty=1)
        }
      }  # end of code for plotEllipses=TRUE
      #
      if (plotPoints) {
        points(data[,useVars], pch=data[,"group_index"], col=ps_colors[data[,"group_index"]])
        if (ps_identify)  {  # show lowess line to help identify points of interest
          if (lowessLine) {
             if (is.na(lowess_f)) lowess_fit<-lowess(data[,useVars])
             else  lowess_fit<-lowess(data[,useVars],f=lowess_f)
             lines(lowess_fit)
             }  # end of code for lowessLine = TRUE
          index<-identify(x=data[,useVars[1]],y=data[,useVars[2]] )
            # row numbers identified
          ps_dataCheck<-data[index,]  # identified points
        }
      }  # end of code for plotPoints = TRUE
      #
      #  plot smoothed line
      #
      if (lowessLine) {
        if (is.na(lowess_f)) lowess_fit<-lowess(data[,useVars])
        else  lowess_fit<-lowess(data[,useVars],f=lowess_f)
#        lowess_fit <- lowess.as(x=data[,useVars[1]],y=data[,useVars[2]],criterion="aicc",
#                                family="gaussian")
        lines(lowess_fit)
      }  # end of code for lowessLine = TRUE
      if (kernelSmooth) {
        kernel_fit<-ksmooth(x=data[,useVars[1]],
                            y=data[,useVars[2]],"normal",
                            bandwidth=sum(range(data[,useVars[1]])*c(-1,1))*kernelWidth)
        lines(kernel_fit)
      }
#      if (locPoly) {
#         bw <- dpill(x=data[,useVars[1]],y=data[,useVars[2]])  # compute bandwidth
#         browser()
#         fit <- locpoly(x=data[,useVars[1]],y=data[,useVars[2]],degree=1,bandwidth=bw,gridsize=50L)
#         lines(fit)
#      }
      #
      if (plotHulls & (!plotMedians))  {  # plot hulls but not medians
        hull_pts <- chull(data[,useVars])
        hull_pts <- c(hull_pts, hull_pts[1])
        lines(data[hull_pts,useVars])
      } #  end of code for plotHulls = TRUE
      if (plotMedians)  {  # draw hulls for each group and plot median in group
         groups <- unique(data[,ps_groupVar])  # unique group codes
         for (i in 1:length(groups)) {
           data_i <- data[data[,ps_groupVar]==groups[i], ]  # data for group i
           median_i <- apply(data_i[,useVars],2,median)
           #
           hull_pts_i <- chull(data_i[,useVars])
           hull_pts_i <- c(hull_pts_i, hull_pts_i[1])  # indices of rows defining the convex hull
           lines(data_i[hull_pts_i,useVars])
           if (i == 1)  medians <- median_i
             else  medians <- rbind(medians, median_i)
         }  # end of loop on i
         text(x=medians[,useVars[1]], y=medians[,useVars[2]],labels=groups)
      } #  end of code for plotMedians = TRUE
      }  # end of code for ps_plotAllGroups = FALSE
      #
      if (ps_plotAllGroups) { # plot identifies groups on a single plot
        #
        groups <- unique(data[,ps_groupVar])  # unique group codes
        #
        #  modify ranges if necessary to account for plotting of confidence ellipses
        #
        if (plotEllipses) {
          for (i in 1:length(groups)) {
            data_i <- data[data[,ps_groupVar]== groups[i],]
            for (j in 1:length(ps_ellipses)) {
              Covar <- var(data_i[,useVars])
              Ellipse <- ellipse::ellipse(x = Covar, centre = apply(data_i[,useVars], 2, mean, na.rm = T),
                               level = ps_ellipses[j], npoints = 200)
              rangeX<-range(rangeX,Ellipse[,1])
              rangeY<-range(rangeY,Ellipse[,2])
          }
        }
        } # end of code for PlotEllipses=TRUE
        #
        #  set up plot for specified pair of variables
        #
        plot(type="n", x=rangeX, y=rangeY, xlab=useVars[1], ylab=useVars[2])
        #
        #  if specified, plot confidence ellipses
        #
        if (plotEllipses) {
          for (i in 1:length(groups)) {
            data_i<-data[data[,ps_groupVar]==groups[i],]
            Covar <- var(data_i[,useVars])
            for (j in 1:length(ps_ellipses)) {
              Ellipse <- ellipse::ellipse(x = Covar, centre = apply(data_i[,useVars],2, mean, na.rm = T),
                               level = ps_ellipses[j], npoints = 200)
            lines(Ellipse, lty=1)
            }
          }
        }  # end of code for plotEllipses=TRUE
        #
        if (plotPoints) {
          for (i in 1:length(groups)) {
            data_i<-data[data[,ps_groupVar]==groups[i],]
            points(data_i[,useVars], pch=i, col=ps_colors[i])
            }
            if (ps_identify)  {  # show lowess lines to help identify points of interest
            if (lowessLine) {
              for (i in 1:length(groups)) {
                data_i<-data[data[,ps_groupVar]==groups[i],]
                if (is.na(lowess_f)) lowess_fit<-lowess(data_i[,useVars])
                else  lowess_fit<-lowess(data_i[,useVars],f=lowess_f)
#                lowess_fit <- lowess.as(x=data[,useVars[1]],y=data[,useVars[2]],criterion="aicc",
#                                        family="gaussian")
                lines(lowess_fit)
                }
              }  # end of code for lowessLine = TRUE
            index<-identify(x=data[,useVars[1]],y=data[,useVars[2]] )
            # row numbers identified
            ps_dataCheck<-data[index,]  # identified points
          }  # end of code for ps_identify = TRUE
        }  # end of code for plotPoints = TRUE
        #
        #  plot smoothed lines
        #
        if (lowessLine) {
          for (i in 1:length(groups)) {
            data_i<-data[data[,ps_groupVar]==groups[i],]
            if (is.na(lowess_f)) lowess_fit<-lowess(data_i[,useVars])
            else  lowess_fit<-lowess(data_i[,useVars],f=lowess_f)
#            lowess_fit <- lowess.as(x=data[,useVars[1]],y=data[,useVars[2]],criterion="aicc",
#                                    family="gaussian")
            lines(lowess_fit)
            }
          }  # end of code for lowessLine = TRUE
        if (kernelSmooth) {
          for (i in 1:length(groups)) {
            data_i<-data[data[,ps_groupVar]==groups[i],]
            kernel_fit<-ksmooth(x=data_i[,useVars[1]],
                              y=data_i[,useVars[2]],"normal",
                              bandwidth=sum(range(data_i[,useVars[1]])*c(-1,1))*kernelWidth)
            lines(kernel_fit)
            }
        }
#        if (locPoly) {
#          bw <- dpill(x=data[,useVars[1]],y=data[,useVars[2]])  # compute bandwidth
#          browser()
#          fit <- locpoly(x=data[,useVars[1]],y=data[,useVars[2]],degree=1,bandwidth=bw,gridsize=50L)
#          lines(fit)
#        }
        if (plotHulls)  {  # plot hulls
          for (i in length(groups)) {
            data_i<-data[data[,ps_groupVar]==groups[i],]
            hull_pts <- chull(data_i[,useVars])
            hull_pts <- c(hull_pts, hull_pts[1])
            lines(data_i[hull_pts,useVars])
            }
          } #  end of code for plotHulls = TRUE
        legend(ps_legend,bty="n",legend=groups, pch=1:length(groups), col=ps_colors)
       }  # end of code for ps_plotAllGroups = TRUE
      if (ps_identify)  ps_dataCheck  # return data frame dataCheck with identified points
         else  invisible()
    }
