#' ps_2dPlot
#'
#' Create 2-dimensional scatter plots, using ps_Plot().
#'
#' @param doc A string documenting use written to the output list;
#' default is the function name
#' @param data A data frame containing the data to be analyzed
#' @param GroupVar The name for the variable defining grouping; can be " " if no grouping
#' @param AnalyticVars The names of analytic variables to be used
#' @param ID The character value of the name of a lab ID, "none" (the default) if none
#' @param Groups A vector of values of GroupVar for which plots
#'  are to be done; if "All", use all groups; if " ", no grouping
#' @param ByGroup Logical.  If TRUE, show scatterplot for each group
#'  for each selection of 2 variables; if FALSE (the default),
#'   all specified groups are on one plot
#' @param PlotAllGroups  Logical.  If TRUE (default is FALSE), all groups are one one plot, with
#' group membership shown.  If PlotEllipses is TRUE, confidence ellipses are also shown.
#' @param VariablePairs The names of two analytic variables to be shown in the plots, vector of length 2
#'  or matrix with 2 columns; if a matrix, the set of plots is produced for each row.
#' @param PlotPoints Logical.  If TRUE (the default), all points are plotted; if FALSE, no points are plotted
#' @param LowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param Lowess_f A parameter for lowess() less than or equal to 1, defining the range of x-values used;
#'     if NA (the default), uses the default value of 0.67
#' @param KernelSmooth Logical.  If TRUE, a kernel smooth is plotted for each group;
#' if FALSE (the default), no kernel smooth is plotted
#' @param Kernelwidth the proportion of the range of x-values used in the kernel smooth;
#' default is 0.3
#' @param localPoly Logical.  If TRUE (default is FALSE), plot local polynomial lines with
#' a bandwidth selected from the data.  Currently not used.
#' @param PlotEllipses Logical.  If TRUE, Gaussian confidence ellipses are plotted for each group;
#' if F (the default), no ellipses are plotted
#' @param Ellipses single value or vector of values with confidence values for the ellipses; default is c(0.95,0.99)
#' @param PlotHulls if TRUE, the convex hull is drawn for each set of points; if FALSE (the default),
#' no hulls are drawn
#' @param PlotMedians if TRUE, the code for each group is plotted at the median of the values
#'  for that group; default is FALSE
#' @param Colors A vector with the colors of plotted points,
#' used sequentially for the groups
#' @param legendLoc  Character.  Identifies the location of the legend for a plot showing all groups
#' on one plot.  Default is "topleft"; alternatives are "bottomleft", "topright", "bottomright"
#' @param parRowsCols A vector of length 2, with the numbers of rows and columns for a plot
#' when plots are shown by group; default is c(2,2)
#' @param Identify if TRUE, user can identify points of interest in the plots; default is FALSE
#'
#' @return
#'  \itemize{
#' \item{usage:}{  A string with the contents of the argument doc,
#' the date run, the version of R used}
#' \item{dataUsed:}{  The contents of the argument data restricted
#' to the groups used}
#' \item{dataNA:}{  A data frame with observations containing at
#' least one missing value for an analysis variable, NA if no missing values}
#' \item{params:}{  A list with the values of the grouping, logical,
#'  numeric and Color arguments}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#' \item{dataCheck: }{ If ps_identify = TRUE, a data frame with the information on user-identified points
#'  of interest}
#'  }
#'
#' @section Details:
#' With multiple plots, execution halts after each page is complete; enter c (continue) at
#' the prompt to continue execution.  For a plot of the labelled convex hulls of the groups,
#' see the example code for the required combination of logical arguments.
#'
#' @examples
#'
#' # All Jemez obsidian sources on one plot
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Nb","Zr","Y","Sr")
#' temp<-ps_2dPlot(data=ObsidianSources,GroupVar="Code",Groups="All",ByGroup=FALSE,
#' AnalyticVars=analyticVars,VariablePairs=c("Rb","Nb"),PlotEllipses=TRUE,PlotAllGroups=TRUE)
#'
#' # Plots of obsidian source data for each source with confidence ellipses and lowess lines
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Nb","Zr","Y","Sr")
#' temp<-ps_2dPlot(data=ObsidianSources,GroupVar="Code",Groups="All",ByGroup=TRUE,
#' AnalyticVars=analyticVars,VariablePairs=c("Rb","Nb"),PlotEllipses=TRUE,PlotAllGroups=FALSE)
#'
#' # Plot of the labelled convex hulls of the obsidian source data for each source
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Nb","Zr","Y","Sr")
#' temp<-ps_2dPlot(data=ObsidianSources,GroupVar="Code",Groups="All",ByGroup=FALSE,
#' AnalyticVars=analyticVars,VariablePairs=c("Rb","Nb"),PlotEllipses=FALSE,LowessLine=FALSE,
#' PlotHulls=TRUE,PlotMedians=TRUE,PlotPoints=FALSE)
#'
#' @import MASS  ellipse  assertthat
#'
#' @export
#'
ps_2dPlot <- function(doc = "ps_2dPlot",
                      data,
                      GroupVar,
                      AnalyticVars,
                      ID = "none",
                      Groups,
                      ByGroup = FALSE,
                      PlotAllGroups = FALSE,
                      VariablePairs,
                      PlotPoints = TRUE,
                      LowessLine = TRUE,
                      Lowess_f = NA,
                      PlotMedians = FALSE,
                      PlotEllipses = FALSE,
                      Ellipses = c(0.95, 0.99),
                      KernelSmooth = FALSE,
                      Kernelwidth = 0.3,
                      localPoly = FALSE,
                      PlotHulls = FALSE,
                      parRowsCols = c(2,2),
                      Colors = c("red","black","blue","green","purple"),
                      legendLoc="topleft",
                      Identify = FALSE) {
  #
  #  check for valid parameters
  #
  assert_that(is.data.frame(data), msg="parameter data not a data.frame")
  assert_that(is.character(GroupVar), msg="paramter GroupVar not character")
  assert_that(is.character(Groups), msg="parameter Groups not character")
  assert_that(is.logical(ByGroup), msg="parameter ByGroup not logical")
  assert_that(is.logical(PlotAllGroups), msg="parameter PlotAllGroups not logical")
  assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
              msg="parameter AnalyticVars not a character vector")
  assert_that(is.character(ID), msg="parameter ID not a character name")
  assert_that(is.character(VariablePairs), msg="type of parameter VariablePairs not character")
  assert_that(is.vector(VariablePairs) | is.matrix(VariablePairs),
              msg="parameter VariablePairs must be a vector or matrix")
  if (is.vector(VariablePairs))  assert_that(length(VariablePairs)==2, msg="vector VariablePairs not of length 2")
  if (is.matrix(VariablePairs))  assert_that(ncol(VariablePairs)==2,
              msg="number of columns of matrix VariablePairs not 2")
  assert_that(is.logical(PlotPoints), msg="parameter PlotPoints not logical")
  assert_that(is.logical(LowessLine), msg="parameter LowessLine not logical")
  assert_that(is.na(Lowess_f) |(is.numeric(Lowess_f) & (Lowess_f > 0) & (Lowess_f <= 1)),
          msg="invalid value for parameter Lowess_f")
  assert_that(is.logical(PlotMedians), msg="type of parameter PlotMedians not logical")
  assert_that(is.logical(PlotEllipses), msg="type of parameter PlotEllipses not logical")
  assert_that(is.numeric(Ellipses), msg="parameter Ellipses not numeric")
  assert_that((min(Ellipses) > 0) & (max(Ellipses) < 1), msg="values of parameter Ellipses not between 0 and 1")
  assert_that(is.logical(KernelSmooth), msg="type of parameter KernelSmooth not logical")
  assert_that(is.numeric(Kernelwidth) & (Kernelwidth > 0), msg="parameter KernelWidth must be numeric and positive")
  assert_that(is.logical(localPoly), msg="type of parameter localPoly not logical")
  assert_that(is.logical(PlotHulls), msg="type of parameter PlotHulls not logical")
  assert_that(is.vector(parRowsCols) & (length(parRowsCols) == 2),
              msg="parameter parRowsCols not a vector of length 2")
  assert_that((round(parRowsCols[1],0)==parRowsCols[1])&
                (round(parRowsCols[1],0)==parRowsCols[1]) &
                (min(parRowsCols) >= 1), msg="values in parRowsCols not positive integers")
  assert_that(is.character(Colors), msg="parameter Colors not character")
  assert_that(is.character(legendLoc), msg="parameter legendLoc not character")
  assert_that(is.logical(Identify), msg="type of parameter Identify is not logical")
  #
  #  use a subset of the groups
  #
  if ((Groups[1] != " ") & (Groups[1] != "All")) {
    Use_rows <- (data[, GroupVar] %in% Groups)
    if (ID == "none") dataUsed <- data[Use_rows, c(GroupVar, AnalyticVars)]
       else  dataUsed <- data[Use_rows, c(GroupVar, ID, AnalyticVars)]
  }
  #
  #  no grouping
  #
  if (ID == "none") {
     if (GroupVar[1] == " ") dataUsed <- data[, AnalyticVars]  # groups not shown
     if (Groups[1] == "All") dataUsed <- data[, c(GroupVar, AnalyticVars)] # groups shown
  }
  if (ID != "none") {
    if (GroupVar[1] == " ") dataUsed <- data[,c(ID, AnalyticVars)]  # groups not shown
    if (Groups[1] == "All") dataUsed <- data[, c(GroupVar, ID, AnalyticVars)] # groups shown
  }
  #
  #  vector to contain indices for observations with no missing values
  #
  dataKeep <- rep(T, nrow(dataUsed))
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
  if (!ByGroup)  # FALSE
    if (length(Colors) < length(groups))  stop("too few colors specified")
  #
  #  sort dataUsed on grouping variable to assign colors to points
  #
  if (GroupVar[1] != " ") dataUsed<-dataUsed[order(dataUsed[,GroupVar]),]
  #
  #  add index to dataUsed to specific color for plotting points in groups
  #
  if ((!ByGroup) & (Groups[1] != " ")) {
    n_group<-rep(0,length(groups))
    for (i in 1:length(groups))  {
      n_group<-nrow(dataUsed[dataUsed[,GroupVar]==groups[i],])
      if (i == 1) group_index<-rep(1,n_group)
      else  group_index<-c(group_index,rep(i,n_group))
    }
    dataUsed<-cbind(dataUsed,group_index=group_index)
  }
  if (ByGroup | (Groups[1] == " ")) dataUsed<-cbind(dataUsed,group_index=rep(1,nrow(dataUsed)))
  #
  # set up data frame to store identified points
  if (Identify) dataCheck<-dataUsed[1,]
  #
  # create plots
  #
  if (is.vector(VariablePairs)) {
    if (!ByGroup) {
      plotData <- dataUsed
      par(mfrow=c(1,1))  # require that the single plot be a full page
      if (!Identify)
      ps_plot(data = plotData,
              ps_groupVar = GroupVar,
              ps_byGroup = ByGroup,
              ps_plotAllGroups = PlotAllGroups,
              useVars = VariablePairs,
              plotEllipses = PlotEllipses,
              ps_ellipses = Ellipses,
              plotPoints = PlotPoints,
              lowessLine = LowessLine,
              lowess_f = Lowess_f,
              plotMedians= PlotMedians,
              kernelSmooth = KernelSmooth,
              kernelWidth = Kernelwidth,
              locPoly = localPoly,
              plotHulls = PlotHulls,
              groupIndex = group_index,
              ps_colors = Colors,
              ps_legend = legendLoc,
              ps_identify = Identify)
      if (Identify)
        dataCheck<-ps_plot(data = plotData,
                ps_groupVar = GroupVar,
                ps_byGroup = ByGroup,
                ps_plotAllGroups = PlotAllGroups,
                useVars = VariablePairs,
                plotEllipses = PlotEllipses,
                ps_ellipses = Ellipses,
                plotPoints = PlotPoints,
                lowessLine = LowessLine,
                lowess_f = Lowess_f,
                plotMedians= PlotMedians,
                kernelSmooth = KernelSmooth,
                kernelWidth = Kernelwidth,
                locPoly = localPoly,
                plotHulls = PlotHulls,
                groupIndex = group_index,
                ps_colors = Colors,
                ps_legend = legendLoc,
                ps_identify = Identify)
    }  # end of loop for ByGroup = FALSE
    if (ByGroup) {
      groupCodes <- unique(dataUsed[,GroupVar])
      if (Identify)  dataCheck<-dataUsed[1,]
      par(mfrow=parRowsCols)  # specify number of plots per page
      for (i in 1:length(groupCodes)) {
         plotData<-dataUsed[(dataUsed[,GroupVar]==groupCodes[i]),]
         if (!Identify)
         ps_plot(data = plotData,
                 ps_groupVar = GroupVar,
                 ps_byGroup = ByGroup,
                 ps_plotAllGroups = PlotAllGroups,
                 useVars = VariablePairs,
                 plotEllipses = PlotEllipses,
                 ps_ellipses = Ellipses,
                 plotPoints = PlotPoints,
                 lowessLine = LowessLine,
                 lowess_f = Lowess_f,
                 plotMedians = PlotMedians,
                 kernelSmooth = KernelSmooth,
                 kernelWidth = Kernelwidth,
                 locPoly = localPoly,
                 plotHulls = PlotHulls,
                 groupIndex = group_index,
                 ps_colors = Colors,
                 ps_legend = legendLoc,
                 ps_identify = Identify)
         if (Identify) {
           identified<-ps_plot(data = plotData,
                   ps_groupVar = GroupVar,
                   ps_byGroup = ByGroup,
                   ps_plotAllGroups = PlotAllGroups,
                   useVars = VariablePairs,
                   plotEllipses = PlotEllipses,
                   ps_ellipses = Ellipses,
                   plotPoints = PlotPoints,
                   lowessLine = LowessLine,
                   lowess_f = Lowess_f,
                   plotMedians = PlotMedians,
                   kernelSmooth = KernelSmooth,
                   kernelWidth = Kernelwidth,
                   locPoly = localPoly,
                   plotHulls = PlotHulls,
                   groupIndex = group_index,
                   ps_colors = Colors,
                   ps_legend = legendLoc,
                   ps_identify = Identify)
           dataCheck <- rbind(dataCheck,identified)
         }  # end of code for Identify = TRUE
         #  pause to save plot after a page is full
#         if (i %% (parRowsCols[1]*parRowsCols[2]) == 0)  browser()
      }  # end of loop on i
      if (Identify)  dataCheck<-dataCheck[-1,]  # remove dummy first row
    }  # end of code for ByGroup = TRUE
  }  # end of code for variablePairs as a vector
  #
  #  code for a matrix of variable pairs
  #
  if (is.matrix(VariablePairs)) {
    for (i_row in 1:nrow(VariablePairs)) {
       if (!ByGroup) {
         plotData <- dataUsed
         if (!Identify)
            ps_plot(data = plotData,
                ps_groupVar = GroupVar,
                ps_byGroup = ByGroup,
                ps_plotAllGroups = PlotAllGroups,
                useVars = VariablePairs[i_row,],
                plotEllipses = PlotEllipses,
                ps_ellipses = Ellipses,
                plotPoints = PlotPoints,
                lowessLine = LowessLine,
                lowess_f = Lowess_f,
                plotMedians= PlotMedians,
                kernelSmooth = KernelSmooth,
                kernelWidth = Kernelwidth,
                locPoly = localPoly,
                plotHulls = PlotHulls,
                groupIndex = group_index,
                ps_colors = Colors,
                ps_legend = legendLoc,
                ps_identify = Identify)
         if (Identify)
           dataCheck<-ps_plot(data = plotData,
                           ps_groupVar = GroupVar,
                           ps_byGroup = ByGroup,
                           ps_plotAllGroups = PlotAllGroups,
                           useVars = VariablePairs[i_row],
                           plotEllipses = PlotEllipses,
                           ps_ellipses = Ellipses,
                           plotPoints = PlotPoints,
                           lowessLine = LowessLine,
                           lowess_f = Lowess_f,
                           plotMedians= PlotMedians,
                           kernelSmooth = KernelSmooth,
                           kernelWidth = Kernelwidth,
                           locPoly = localPoly,
                           plotHulls = PlotHulls,
                           groupIndex = group_index,
                           ps_colors = Colors,
                           ps_legend = legendLoc,
                           ps_identify = Identify)
         if (i_row < nrow(VariablePairs))  {
 #          browser()  #  pause to save plot
           plot.new()  #  next plot on a new page
         }
       }  # end of code for ByGroup = FALSE
       if (ByGroup) {
         groupCodes <- unique(dataUsed[,GroupVar])
         if (Identify)  dataCheck<-dataUsed[1,]
         par(mfrow=parRowsCols)
         for (i in 1:length(groupCodes)) {
            plotData<-dataUsed[(dataUsed[,GroupVar]==groupCodes[i]),]
            if (!Identify)
            ps_plot(data = plotData,
                  ps_groupVar = GroupVar,
                  ps_byGroup = ByGroup,
                  ps_plotAllGroups = PlotAllGroups,
                  useVars = VariablePairs[i_row,],
                  plotEllipses = PlotEllipses,
                  ps_ellipses = Ellipses,
                  plotPoints = PlotPoints,
                  lowessLine = LowessLine,
                  lowess_f = Lowess_f,
                  plotMedians = PlotMedians,
                  kernelSmooth = KernelSmooth,
                  kernelWidth = Kernelwidth,
                  locPoly = localPoly,
                  plotHulls = PlotHulls,
                  groupIndex = group_index,
                  ps_colors = Colors,
                  ps_legend = legendLoc,
                  ps_identify = Identify)
           if (Identify) {
             identified<-ps_plot(data = plotData,
                              ps_groupVar = GroupVar,
                              ps_byGroup = ByGroup,
                              ps_plotAllGroups = PlotAllGroups,
                              useVars = VariablePairs[i_row,],
                              plotEllipses = PlotEllipses,
                              ps_ellipses = Ellipses,
                              plotPoints = PlotPoints,
                              lowessLine = LowessLine,
                              lowess_f = Lowess_f,
                              plotMedians = PlotMedians,
                              kernelSmooth = KernelSmooth,
                              kernelWidth = Kernelwidth,
                              locPoly = localPoly,
                              plotHulls = PlotHulls,
                              groupIndex = group_index,
                              ps_colors = Colors,
                              ps_legend = legendLoc,
                              ps_identify = Identify)
             dataCheck <- rbind(dataCheck,identified)
             }  # end of code for Identify = TRUE
 #           if (i %% (parRowsCols[1]*parRowsCols[2]) == 0)  browser()
         }  # end of code for ByGroup = TRUE
      }  #  end of loop on i
    if (i_row < nrow(VariablePairs))  plot.new()  #  start a new page for the next set of variable pairs
    }  # end of loop on i_row
    if (Identify)  dataCheck<-dataCheck[-1,]  # remove dummy first row
  }  # end of code for variablePairs as a matrix
  #
  fcnDateVersion<-c(doc,date(),R.Version()$version.string)
  #
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(ByGroup,PlotAllGroups,PlotPoints,LowessLine,PlotEllipses,KernelSmooth,
                    localPoly,PlotHulls,PlotMedians)
  names(params_logical)<-c("ByGroup","PlotAllGroups","PlotPoints","LowessLine","PlotEllipses","KernelSmooth",
                           "localPoly","PlotHulls","PlotMedians")
  params_continuous<-c(Lowess_f,Kernelwidth)
  names(params_continuous)<-c("Lowess_f","Kernelwidth")
  params<-list(grouping=params_grouping,logical=params_logical,continuous=params_continuous,colors=Colors)
  #
  if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep]
  else dataNA <- NA
  #
  if (!Identify)
    out<-list(usage=fcnDateVersion,
            dataUsed=dataUsed,
            dataNA=dataNA,
            params=params,
            analyticVars=AnalyticVars)
  if (Identify) {
    if (ID == "none")  dataCheck<-dataCheck[,c(GroupVar,AnalyticVars)]
        else  dataCheck<-dataCheck[,c(GroupVar, ID, AnalyticVars)]
    out<-list(usage=fcnDateVersion,
              dataUsed=dataUsed,
              dataNA=dataNA,
              params=params,
              analyticVars=AnalyticVars,
              dataCheck=dataCheck)
  }
  out
    }
