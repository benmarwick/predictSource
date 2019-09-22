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
#' @param variablePair A vector of length 2 with the names of the variables to be shown in the plots
#' @param scatterplot  Logical (default is TRUE): specify whether to show scatter plots when QQPlot = FALSE
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
#' \item{analyticVars:}{  A vector with the contents of the argument analyticVars}
#' \item{variablePair:}{  A vector with the contents of the argument variablePair}
#' \item{params:}{  A list with the values of grouping, logical, and numberic arguments}
#' \item{pvalues:}{  A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{dataCheck:}{ If Identify=TRUE, a data frame with the information on user-identified points of interest}
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
#' @import MASS  qqtest  nortest graphics stats assertthat
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Nb","Zr","Y","Sr")
#' plot_2d_Gauss<-ps_2dPlotGauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups=c("A","B"),
#'    AnalyticVars= analyticVars, variablePair=c("Rb","Zr"))
#'
#' @export
#'
ps_2dPlotGauss <- function (doc = "ps_2dPlotGauss",
                             data,
                             GroupVar,
                             ID,
                             Groups,
                             AnalyticVars,
                             variablePair,
                             scatterplot=TRUE,
                             QQPlot = TRUE,
                             pvalue_digits=3,
                             Identify=FALSE,
                             folder=" "
                            )
{
  #
  #  check for valid parameters
  #
  assert_that(is.data.frame(data), msg="parameter data not a data frame")
  assert_that(is.character(GroupVar), msg="parameter GroupVar not character")
  assert_that(is.character(Groups), msg="parameter Groups not character")
  assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
              msg="parameter AnalyticVars not a character vector")
  assert_that(is.character(ID), msg="parameter ID not a character name")
  assert_that(is.character(variablePair), msg="type of parameter variablePair not character")
  assert_that(is.vector(variablePair)&(length(variablePair)==2),
              msg="vector variablePair not a vector with length 2")
  assert_that(is.logical(scatterplot), msg="type of parameter scatterplot not logical")
  assert_that(is.logical(QQPlot), msg="type of parameter QQPlot not logical")
  assert_that(is.numeric(pvalue_digits) & (round(pvalue_digits,0)==pvalue_digits) & (pvalue_digits > 0),
              msg="parameter pvalue_digits not a positive integer")
  assert_that(is.logical(Identify), msg="type of parameter Identify is not logical")
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
    data_i <- dataUsed[dataUsed[, GroupVar] == groups[i],]
    #
    temp_i <- ps_plotGauss(
                 data = data_i,
                 ps_groupVar = GroupVar,
                 ps_pair = variablePair,
                 ps_scatterplot = scatterplot,
                 ps_qqPlot = QQPlot,
                 ps_identify = Identify
                  )
    pvalues[i,] <- temp_i$pvalues
    pvalues[i,2:9] <- round(pvalues[i,2:9],digits=pvalue_digits)
    pvalues[pvalues[i,] < 0] <- NA
    if ( Identify)  dataCheck <- rbind(dataCheck, temp_i$dataCheck)
    if ((i < length(groups)) & QQPlot) {
 #     browser()  # pause to enable plot to be saved
       plot.new()
    }
 #  if (((i %% 2)==0) & !QQPlot) browser()  # no QQ plot
      } # end of loop on i
  #
  colnames(pvalues) <- c("n", paste("AD_",variablePair,sep=""), paste("SW_",variablePair,sep=""),
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
  params_logical<-c(scatterplot,QQPlot,Identify)
  names(params_logical)<-c("scatterplot","QQPlot","Identify")
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
                      variablePair=variablePair,
                      params=params,
                      pvalues=pvalues,
                      dataCheck=dataCheck,
                      location=Folder)
  }
