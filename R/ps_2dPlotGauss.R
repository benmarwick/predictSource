#'
#'
#' Plot and check specified pairs of analysis variables for bivariate normality.
#'
#' @param doc  A string documenting use written to the output list; default is the function name
#' @param data  A matrix or data frame containing the data to be analyzed
#' @param GroupVar  The name for the variable defining grouping, " " if no grouping
#' @param ID  The name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  A vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars  A vector of length two with the names of two analytic variables
#'  to be shown in the plots
#' @param scatterPlot  Logical (default is TRUE): specify whether to show scatter plots when qqPlot = FALSE
#' @param qqPlot  Logical (default is TRUE): specify whether to show the q-q plots with the bootstrap
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
#' \item{dataCheck:}{ A data frame with the information on user-identified points of interest;
#'     value is c(NA, NA) if no data identified}
#' \item{location:}{ The value of the parameter folder}
#' }
#'
#' @section Details:
#'  If qqPlot = TRUE, by default each page has panes in two rows and three columns
#'  (but there is a new page for successive groups). The function stops after producing each row
#'   of each plot.  Enter c ("continue") at the prompt to get the next plot.
#'   If qqPlot = FALSE, only the standard qq plots are shown, and the function stops after producing each
#'   page.
#'   See the vignette for more information: visualizing each plot, the information obtained
#'    by using the package qqtest, the tests for bivariate normality, and identifying points of interest.
#'
#' @import MASS  qqtest  MVN  nortest
#'
#' @examples
#' data(ObsidianSources)
#' plot_2d_Gauss<-ps_2dPlotGauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups=c("A","B"),
#'    AnalyticVars=c("Rb","Zr"))
#'
#' @export
#'
ps_2dPlotGauss <- function (doc = "ps_2dPlotGauss",
                             data,
                             GroupVar,
                             ID,
                             Groups,
                             AnalyticVars,
                             scatterPlot=TRUE,
                             qqPlot = TRUE,
                             pvalue_digits=3,
                             Identify=FALSE,
                             folder=" "
                            )
{
  if (length(AnalyticVars)!=2)  stop("length of AnalyticVars must be 2")
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
    dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- F
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
  if (Identify) dataCheck<-dataUsed[1,]
      else  dataCheck <- c(NA, NA)  # set up data frame to store identified points
  #
  fnPlot <- function() {
    temp <- dataUsed[dataUsed[, GroupVar] == groups[i_group],AnalyticVars]
    temp1 <- temp[, AnalyticVars[1]]
    temp1 <- temp1[!is.na(temp[,AnalyticVars[1]])]
    if (qqPlot | Identify) {
      qqtest(data = temp1, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[1],"source", groups[i_group]))
      qqnorm_pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i_group]))
      qqline(temp1)
    }
    else if (scatterPlot) {  # TRUE
      qqnorm_pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i_group]))
      qqline(temp1)
      }
    if (Identify) {
      index<-identify(qqnorm_pts)
      data_grp<-dataUsed[dataUsed[,GroupVar]==groups[i_group],]
      dataCheck<<-rbind(dataCheck,data_grp[index,])
      }
    temp2 <- temp[, AnalyticVars[2]]
    temp2 <- temp2[!is.na(temp[,AnalyticVars[2]])]
    if (qqPlot | Identify) {
      qqtest(data = temp2, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[2],"source", groups[i_group]))
      qqnorm_pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i_group]))
      qqline(temp2)
    }
    else if (scatterPlot) {
      qqnorm_pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i_group]))
      qqline(temp2)
     }
    if (Identify) {
      index<-identify(qqnorm_pts)
      data_grp<-dataUsed[dataUsed[,GroupVar]==groups[i_group],]
      dataCheck<<-rbind(dataCheck,data_grp[index,])
    }
    ADp1 <- ad.test(temp1)$p.value
    ADp2 <- ad.test(temp2)$p.value
    SWp1 <- shapiro.test(temp1)$p.value
    SWp2 <- shapiro.test(temp2)$p.value
    temp <- temp[!is.na(temp[,AnalyticVars[1]]) & !is.na(temp[,AnalyticVars[2]]),]
    n_samples <- nrow(temp)
    mardia <- MVN::mvn(data = temp, mvnTest="mardia")
    p_mardia_skew <- as.numeric(as.character(mardia[[1]][[3]][[1]],mode="character"))
    p_mardia_kurtosis <- as.numeric(as.character(mardia[[1]][[3]][[2]],mode="character"))
    if (qqPlot)
      HZ <- MVN::mvn(data=temp, mvnTest="hz",multivariatePlot="qq")
      else  HZ <- MVN::mvn(data=temp, mvnTest="hz")
    #
    p_HZ <- as.numeric(HZ[[1]][[3]], mode = "numeric")
    royston <- MVN::mvn(data=temp, mvnTest="royston")
    p_Royston <- as.numeric(royston[[1]][[3]], mode = "numeric")
    p_temp <- c(n_samples, ADp1, ADp2, SWp1, SWp2, p_mardia_skew, p_mardia_kurtosis, p_HZ, p_Royston)
    browser()
    # return p-values
    p_temp
  } # end of definition of function
  #
  pvalues <- matrix(NA, nrow=length(groups), ncol = 9)
  #
  if (qqPlot)  par(mfrow = c(2,3))
  else par(mfrow = c(2,2))
  #
  iPlot<-0 # counter used when qqPlot=FALSE
  #
  for (i_group in 1:length(groups)) {
    iPlot <- iPlot+1
    pvalues[i_group, ] <- fnPlot() # plot for this group and compute p-values
    if (qqPlot)  {  #  TRUE
      plot.new() # blank plot so next group starts in new window
    }
    # two plots per frame if qqPlot=FALSE
    if ((!qqPlot) & (floor(i_group/2)==i_group/2))  browser()
  } # end of loop on i_group
  #
  numeric_pvalues<-as.numeric(pvalues)
  numeric_pvalues<-round(numeric_pvalues,digits=pvalue_digits)
  numeric_pvalues[numeric_pvalues < 0] <- NA
  return_pvalues<-matrix(numeric_pvalues,nrow=length(groups),ncol=9)
  dimnames(return_pvalues) <- list(groups, c("n", paste("AD_",AnalyticVars,sep=""), paste("SW_",AnalyticVars,sep=""),
                                             "maridaSkew", "mardiaKurtosis", "HZ", "Royston"))
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
  #
  fcnDateVersion<-c(doc,date(),R.Version()$version.string)
  #
  params_numeric<-pvalue_digits
  names(params_numeric)<-"pvalue_digits"
  params_grouping<-list(GroupVar,Groups)
  names(params_grouping)<-c("GroupVar","Groups")
  params_logical<-c(scatterPlot,qqPlot,Identify)
  names(params_logical)<-c("scatterPlot","qqPlot","Identify")
  params<-list(grouping=params_grouping,numeric=params_numeric,logical=params_logical)
    #
  if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
  else dataNA <- NA
  #
  list(         usage=fcnDateVersion,
                dataUsed=dataUsed,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params=params,
                pvalues=return_pvalues,
                dataCheck=dataCheck,
                location=folder)
  }
