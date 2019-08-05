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
#' @param VariablePairs The names of two analytic variables to be shown in the plots, vector of length 2
#'  or matrix with 2 columns; if a matrix, the set of plots is produced for each row.
#' @param PlotPoints Logical.  If TRUE (the default), all points are plotted; if FALSE, no points are plotted
#' @param LowessLine Logical. If TRUE, a lowess line is plotted for each group; if FALSE, no line is plotted
#' @param Lowess_f A parameter for lowess() less than or equal to 1, defining the range of x-values used;
#'     if NA (the default), uses the default value of 0.67
#' @param PlotMedians Logical.  If TRUE, plot the median of each group; default if FALSE
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
#' @param Colors A vector with the colors of plotted points,
#' used sequentially for the groups
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
#'     If the user identifies points of interest:
#' \item{dataCheck: }{ If ps_identify = TRUE, a data frame with the information on user-identified points
#'  of interest; value is c(NA,NA) if no points are identified}
#'  }
#'
#' @section Details:
#'
#'
#' @examples
#'
#'
#'
#' @import MASS  ellipse
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
                      VariablePairs,
                      PlotPoints = TRUE,
                      LowessLine = TRUE,
                      Lowess_f = NA,
                      PlotMedians = FALSE,
                      PlotEllipses = FALSE,
                      Ellipses = c(0.95, 0.99),
                      KernelSmooth = FALSE,
                      Kernelwidth = 0.3,
                      PlotHulls = FALSE,
                      parRowsCols = c(2,2),
                      Colors = c("red","black","blue","green","purple"),
                      Identify = FALSE) {
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
      if (!Identify)
      ps_plot(data = plotData,
              ps_groupVar = GroupVar,
              ps_byGroup = ByGroup,
              useVars = VariablePairs,
              plotEllipses = PlotEllipses,
              ps_ellipses = Ellipses,
              plotPoints = PlotPoints,
              lowessLine = LowessLine,
              lowess_f = Lowess_f,
              plotMedians= PlotMedians,
              kernelSmooth = KernelSmooth,
              kernelWidth = Kernelwidth,
              plotHulls = PlotHulls,
              ps_identify = Identify)
      if (Identify)
        dataCheck<-ps_plot(data = plotData,
                ps_groupVar = GroupVar,
                ps_byGroup = ByGroup,
                useVars = VariablePairs,
                plotEllipses = PlotEllipses,
                ps_ellipses = Ellipses,
                plotPoints = PlotPoints,
                lowessLine = LowessLine,
                lowess_f = Lowess_f,
                plotMedians= PlotMedians,
                kernelSmooth = KernelSmooth,
                kernelWidth = Kernelwidth,
                plotHulls = PlotHulls,
                ps_identify = Identify)
    }  # end of loop for ByGroup = FALSE
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
                 useVars = VariablePairs,
                 plotEllipses = PlotEllipses,
                 ps_ellipses = Ellipses,
                 plotPoints = PlotPoints,
                 lowessLine = LowessLine,
                 lowess_f = Lowess_f,
                 plotMedians = PlotMedians,
                 kernelSmooth = KernelSmooth,
                 kernelWidth = Kernelwidth,
                 plotHulls = PlotHulls,
                 ps_identify = Identify)
         if (Identify) {
           identified<-ps_plot(data = plotData,
                   ps_groupVar = GroupVar,
                   ps_byGroup = ByGroup,
                   useVars = VariablePairs,
                   plotEllipses = PlotEllipses,
                   ps_ellipses = Ellipses,
                   plotPoints = PlotPoints,
                   lowessLine = LowessLine,
                   lowess_f = Lowess_f,
                   plotMedians = PlotMedians,
                   kernelSmooth = KernelSmooth,
                   kernelWidth = Kernelwidth,
                   plotHulls = PlotHulls,
                   ps_identify = Identify)
           dataCheck <- rbind(dataCheck,identified)
         }  # end of code for Identify = TRUE
      }  # end of loop on i
      if (Identify)  dataCheck<-dataCheck[-1,]  # remove dummy first row
    }  # end of code for ByGroup = TRUE
  }  # end of code for variablePairs as a matrix

  #
  fcnDateVersion<-c(doc,date(),R.Version()$version.string)
  #
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(ByGroup,PlotMedians)
  names(params_logical)<-c("ByGroup","PlotMedians")
  params<-list(grouping=params_grouping,logical=params_logical,colors=Colors)
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
  if (Identify)
    if (ID == "none")  dataCheck<-dataCheck[,c(GroupVar,AnalyticVars)]
        else  dataCheck<-dataCheck[,c(GroupVar, ID, AnalyticVars)]
    out<-list(usage=fcnDateVersion,
              dataUsed=dataUsed,
              dataNA=dataNA,
              params=params,
              analyticVars=AnalyticVars,
              dataCheck=dataCheck)
  out
    }
