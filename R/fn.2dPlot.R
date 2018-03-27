

fn.2dPlot <- function (doc = "fn.bivariate", data=ObsidianSources, GroupVar = "Code", labID=" ", Groups = c("A","B"),
          AnalyticVars=ElsBivariate[1:2,], PlotByGroup=T, PlotPoints = T, LowessLine=F, Lowess.f=NA,
          KernelSmooth=F,KernelWidth=0.3, PlotEllipses = F,
          PlotHulls = F, PlotMedians = F, Ellipses = c(0.95, 0.99), Identify=F,
          PlotColors = F, Colors="black",folder=" ",ds.identified)
{
  #
  #   doc: documentation
  #   data: data frame containing character group variable and analytic variables
  #   GroupVar: name of grouping variable
  #   Groups: define groups to be used, can be a vector of character values
  #   AnalyticVars: names of two analytic variables to be considered
  #     vector of length 2 or matrix with 2 columns
  #     if a matrix, the set of plots is produced for each row
  #   PlotByGroup: if F, all groups are on each plot for a pair of variables
  #   PlotPoints: if T, all points are plotted
  #   LowessLine: if T, a lowess line is plotted for each group
  #   Lowess.f: parameter for lowess()less than or equal to 1, defining range of x-values used
  #     if NA, uses default value of 0.67
  #   KernelSmooth: if T, a kernel smooth is plotted for each group
  #   KernelWidth: the proportion of the range of x-values used in the kernel smooth
  #   PlotEllipses: if T, Gaussian confidence ellipses are plotted for each group
  #   Ellipses: single value or vector of values with confidence values for the ellipses
  #   PlotHulls: if T, the convex hull is drawn for each set of points
  #   PlotMedians: if T, the code for each group is plotted at the median of the values for that group
  #   Identify: if T, identify points of interest in plots
  #   PlotColors: if T, colors are assigned to the groups
  #   Colors: single value or vector of color names
  #   folder: in Windows, path to the folder containing an excel file with the identified points, ending in //
  #         if " ", no data set is written
  #   ds.identified: excel file name with extension .csv containing information on the identified points
  #
  if (Groups[1] != "All") {
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows,]
  }
  else data.Used <- data
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
  if (Identify)  data.check<-data.Used[1,]  # dummy row to set up information on identified observations
  #
  #   plots by Group
  #
  if (PlotByGroup) {
    n.pages <- round(0.01+length(groups)/2, dig = 0)  # number of pages (rounds up with an odd number of groups)
    #
    fn.plot <- function(group,group.j, groupName) {
      temp <- data.Used[(data.Used[, GroupVar] == groups[group]),]
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
    for (i.group in 1:length(groups)) {
      plot.new()
      par(mfrow = c(2, 2))
      n.page<-0
      for (k in 1:n.pairs) {
        n.page<-n.page+1
        if (n.page > 4) {
          browser()
          plot.new()
          par(mfrow=c(2,2))
          n.page<-0
        }
        if (!Identify)  fn.plot(group=i.group,group.j=k,groupName=groups[i.group])
        if ( Identify)  data.check<-fn.plot(group=i.group,group.j=k,groupName=groups[i.group])
      }
      browser()
    }
  }  # end of code for plotting by group
  #
  #  all groups on one plot
  #
  if (!PlotByGroup) {
    if ((length(Colors)==1)&(Colors[1]=="black"))  Colors<-rep("black",length(groups))
    else {
      if (length(Colors) != length(groups))  stop("length of specified colors must equal number of groups")
    }
    #
    for (j in 1:n.pairs) {
      # set up plot for this pair of variables
      plot.new()
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
      #  add legend to plot in top right corner
      #
      if (!PlotMedians) {
        if (PlotColors)
          legend(x = "topright", legend = groups, pch = 0:(length(groups) - 1), col = Colors[1:length(groups)], bty = "n")
        else legend(x = "topright", legend = groups, pch = 0:(length(groups) - 1), bty = "n")
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
      if (labID != " ") index<-duplicated(data.check[,labID])
      else  index<-duplicated(data.check[,c(GroupVar,AnalyticVars)])
      if (length(index) > 0)  data.check<-data.check[!index,]
      if (labID != " ") {
        index.ID<-order(data.check[,labID])
        data.check<-data.check[index.ID,]
      }
    }
  }
  fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
  params.numeric<-c(Lowess.f,KernelWidth)
  names(params.numeric)<-c("Lowess.f","KernelWidth")
  params.grouping<-list(GroupVar,Groups)
  names(params.grouping)<-c("GroupVar","Groups")
  if ((substr(folder,1,1) == " ") & (!Identify))
    out<-list(fcn.date.ver=fcn.date.ver,dataUsed=data.Used,params.numeric=params.numeric,params.grouping=params.grouping,ellipse.pct=Ellipses)
  if ((substr(folder,1,1) == " ") & (Identify))
    out<-list(fcn.date.ver=fcn.date.ver,dataUsed=data.Used,params.numeric=params.numeric,params.grouping=params.grouping,ellipse.pct=Ellipses,data.check=data.check)
  if ((substr(folder,1,1) != " ") * (Identify))
    out<-list(fcn.date.ver=fcn.date.ver,dataUsed=data.Used,params.numeric=params.numeric,params.grouping=params.grouping,ellipse.pct=Ellipses,
              data.check=data.check,folder.data.check=paste(folder,ds.identified,sep=""))
  out
}
