#'
#' ps_2dPlotGauss
#'
#' Plot and check specified pairs of analysis variables for bivariate normality.
#'
#' @param doc  A string documenting use written to the output list; default is the function name
#' @param data  A matrix or data frame containing the data to be analyzed
#' @param GroupVar  The name for the variable defining grouping, " " if no grouping
#' @param ID  The name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  A vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars  A vector with the names of all analytic variables of interest
#' @param analysisVars A vector of length 2 with the names of the variables to be shown in the plots
#' @param scatterPlot  Logical (default is TRUE): specify whether to show scatter plots when QQPlot = FALSE
#' @param QQPlot  Logical (default is TRUE): specify whether to show the q-q plots with the bootstrap
#' envelopes and multivariate plots
#' @param pvalue_digits  Numeric (default is 3): number of significant digits retained in tests for normality
#' @param Identify  Logical(default is FALSE): if TRUE, user can identify points of interest in the plots
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage:}{  String with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{analyticVars:}{  The contents of the argument AnalyticVars}
#' \item{params:}{  A list with the values of grouping, logical, and numberic arguments}
#' \item{pvalues:}{  A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{dataCheck:}{ If Identify=TRUE, a data frame with the information on user-identified points of interest;
#' \item{location:}{ The value of the parameter folder}
#' }
#'
#' @section Details:
#'  If QQPlot = TRUE, by default each page has panes in two rows and three columns
#'  (but there is a new page for successive groups). The function stops after producing each row
#'   of each plot.  Enter c ("continue") at the prompt to get the next plot.
#'   If QQPlot = FALSE, only the standard qq plots are shown, and the function stops after producing each
#'   page.
#'   See the vignette for more information: visualizing each plot, the information obtained
#'    by using the package qqtest, the tests for bivariate normality, and identifying points of interest.
#'
#' @import MASS  qqtest  MVN  nortest
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Nb","Zr","Y","Sr")
#' plot_2d_Gauss<-ps_2dPlotGauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups=c("A","B"),
#'    AnalyticVars= analyticVars, analysisVars=c("Rb","Zr"))
#'
#' @export
#'
ps_2dPlotGauss <- function (doc = "ps_2dPlotGauss",
                             data,
                             GroupVar,
                             ID,
                             Groups,
                             AnalyticVars,
                             analysisVars,
                             scatterPlot=TRUE,
                             QQPlot = TRUE,
                             pvalue_digits=3,
                             Identify=FALSE,
                             folder=" "
                            )
{
  if (length(analysisVars)!=2)  stop("length of AnalyticVars must be 2")
  #
  if (Groups[1] != "All") {
    Use_rows <- (data[, GroupVar] %in% Groups)
    dataUsed <- data[Use_rows, ]
  }
  else dataUsed <- data
  #
  dataKeep <- rep(T, nrow(dataUsed)) # will contain indices for observations with
  # no missing values
  for (i in 1:length(AnalyticVars))
    dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- FALSE
  #
  if (Groups[1] == "All")
    groups <- as.character(unique(dataUsed[, GroupVar]))
  else groups <- as.character(Groups)
  #
  GroupIndex <- rep(NA, nrow(dataUsed))
  for (i in 1:nrow(dataUsed)) {
    for (j in 1:length(groups)) if (dataUsed[i, GroupVar] == groups[j])
      GroupIndex[i] <- j
    }
  #
  if (Identify) dataCheck<-dataUsed[1,]  # set up data frame to store identified points
  pvalues <- matrix(NA, nrow=length(groups), ncol = 9) # matrix to store p-values
  #
  #  specify numbers of frames per page
  #
  if (QQPlot)  par(mfrow = c(2,3))
  else par(mfrow = c(2,2))
  #
  for (i in 1:length(groups)) {
    data_i <- dataUsed[dataUsed[, GroupVar] == groups[i],AnalyticVars]
    temp_i <- ps_plotGauss(
                 data = data_i,
                 ps_groupVar = GroupVar,
                 analysisVars = AnalysisVars,
                 ps_analyticVars = analyticVars,
                 ps_scatterplot = scatterplot,
                 ps_qqPlot = QQPlot,
                 ps_identify = Identify
                  )
    pvalues[i,] <- temp_i$pvalues
    pvalues[i,2:9] <- round(pvalues[i,2:9],digits=pvalue_digits)
    pvalues[pvalues[i,] < 0] <- NA
    if ( Identify) {
       dataCheck <- rbind(dataCheck, temp_i$dataCheck)
    }
    browser()  # pause to enable plot to be saved
      } # end of loop on i
  #
  colnames(pvalues) <- c("n", paste("AD_",analysisVars,sep=""), paste("SW_",analysisVars,sep=""),
                                             "maridaSkew", "mardiaKurtosis", "HZ", "Royston")
  pvalues <- data.frame(group=groups,pvalues)
  #
  #  remove duplicated observations from dataCheck
  #
  if (Identify) {
    dataCheck<-dataCheck[-1,]  # remove dummy first row
    if (ID != " ") index<-duplicated(dataCheck[,ID])
    else  index<-duplicated(dataCheck[,c(GroupVar,AnalyticVars)])
    if (length(index) > 0)  dataCheck<-dataCheck[!index,]
    if (ID != " ") {
      index_ID<-order(dataCheck[,ID])
      dataCheck<-dataCheck[index_ID,]
    }
  } # end of code for Identify=T
  if (!Identify)  dataCheck <- NA
  #
  fcnDateVersion<-c(doc,date(),R.Version()$version.string)
  #
  params_numeric<-pvalue_digits
  names(params_numeric)<-"pvalue_digits"
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(scatterPlot,QQPlot,Identify)
  names(params_logical)<-c("scatterPlot","QQPlot","Identify")
  params<-list(grouping=params_grouping,numeric=params_numeric,logical=params_logical)
    #
  if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
  else dataNA <- NA
  Folder<-folder
  #
  list(usage=fcnDateVersion,
                      dataUsed=dataUsed,
                      dataNA=dataNA,
                      analyticVars=AnalyticVars,
                      params=params,
                      pvalues=pvalues,
                      dataCheck=dataCheck,
                      location=Folder)
  }
