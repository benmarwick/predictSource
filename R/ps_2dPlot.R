#' ps_2dPlot
#'
#' Create 2-dimensional scatter plots, using ps_Plot().
#'
#' @param doc A string documenting use written to the output list;
#' default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param GroupVar The name for the variable defining grouping; can be " " if no grouping
#' @param Groups A vector of values of group variable for which plots
#'  are to be done; if "All", use all groups; if " ", no grouping
#' @param ByGroup Logical.  If TRUE, show scatterplot for each group
#'  for each selection of 3 variables; if FALSE (the default),
#'   all specified groups are on one plot
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
#' @param Colors A vector with the colors of plotted points,
#' used sequentially for the groups
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
ps_2dplot <- function(doc = "ps_2dPlot",
                      data,
                      GroupVar,
                      AnalyticVars,
                      Groups,
                      ByGroup = "FALSE",
                      PlotEllipses = FALSE,
                      Ellipses = c(0.95, 0.99),
                      PlotPoints = TRUE,
                      LowessLine = TRUE,
                      Lowess_f = NA,
                      KernelSmooth = FALSE,
                      Kernelwidth = 0.3,
                      PlotHulls = FALSE,
                      Colors = c("red","black","blue","green","purple"),
                      Identify = FALSE) {
  #
  if ((Groups[1] != " ") & (Groups[1] != "All")) {
    Use_rows <- (data[, GroupVar] %in% Groups)
    dataUsed <- data[Use_rows, c(GroupVar, AnalyticVars)]
  }
  else if (GroupVar[1] == " ")
    dataUsed <- data[, AnalyticVars]
  else dataUsed <- data[, c(GroupVar, AnalyticVars)]
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
  if (!ByGroup)  #  FALSE
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
  #
  fcnDateVersion<-c(doc,date(),R.Version()$version.string)
  #
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(ByGroup,PlotMedians)
  names(params_logical)<-c("ByGroup","PlotMedians")
  params_numeric<-SymbolSize
  names(params_numeric)<-"SymbolSize"
  params<-list(grouping=params_grouping,logical=params_logical,numeric=params_numeric,colors=Colors)
  #
  if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep]
  else dataNA <- NA
  #
  if (!Itemize)
    out<-list(usage=fcnDateVersion,
            dataUsed=dataUsed,
            dataNA=dataNA,
            params=params,
            analyticVars=AnalyticVars)
  if (Itemize)
    out<-list(usage=fcnDateVersion,
              dataUsed=dataUsed,
              dataNA=dataNA,
              params=params,
              analyticVars=AnalyticVars,
              dataCheck=dataCheck)
  out
    invisible()
    }
