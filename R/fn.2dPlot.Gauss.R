#' fn.2dPlot.Gauss
#'
#' Plot and check specified pairs of analysis variables for bivariate normality. The function stops after producing each plot.  Enter c ("continue") at the prompt to get the next plot.
#'
#' @param doc  A string documenting use written to the output list; default is the function name
#' @param data  R matrix or data frame containing the data to be analyzed
#' @param GroupVar  Name for the variable defining grouping, " " if no grouping
#' @param labID  Name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  Vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars  Names of two analytic variables to be shown in the plots, vector of length 2
#' @param qqPlot  Logical (default is T): specify whether to show q-q plots, including plots based on simulation and multivariate plots
#' @param pvalue.digits  Numeric (default is 3): number of significant digits retained in tests for normality
#' @param Identify  Logical(default is F): if T, user can identify points of interest in the plots; qqPlot must be T
#' @param folder  In Windows, path to the folder containing an excel file with the identified points, ending with two forward slashes;
#'         if " ", no data set is written
#' @param ds.identified  Excel file name with extension .csv containing information on the identified points
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage:}{  Vector with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#' \item{analyticVars:}{  The contents of the argument AnalyticVars}
#' \item{parameters:}{  A vector with argument values for GroupVar, Groups, pvalue.digits, and QQtest}
#' \item{pvalues:}{  A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{data.check:}{ If Identify = T, a data frame with the information on user-identified points of interest}
#' \item{file:}{ If folder != " ", a string or list: the path to the excel file or files with pvalues and, if Identify = T,
#'  the information on user-identified points of interest}
#' }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot, the information obtained by using the package qqtest,
#'   the tests for bivariate normality, and identifying points of interest.
#'
#' @import MASS  qqtest  MVN
#' @examples
#' data(ObsidianSources)
#' plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", labID="ID", Groups=c("A","B"),
#'    AnalyticVars=c("Rb","Zr"))
#'
#' @export
#'
fn.2dPlot.Gauss <- function (doc = "fn.2dPlot.Gauss", data, GroupVar,labID, Groups,
          AnalyticVars, qqPlot = T, pvalue.digits=3, Identify=F, folder=" ", ds.pvalues, ds.data.check)
{
  if (length(AnalyticVars)!=2)  stop("length of AnalyticVars must be 2")
  #
  if (Groups[1] != "All") {
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows, ]
  }
  else data.Used <- data
  #
  if (Groups[1] == "All")
    groups <- as.character(unique(data.Used[, GroupVar]))
  else groups <- as.character(Groups)
  #
  GroupIndex <- rep(NA, nrow(data.Used))
  for (i in 1:nrow(data.Used)) {
    for (j in 1:length(groups)) if (data.Used[i, GroupVar] ==
                                    groups[j])
      GroupIndex[i] <- j
  }
  #
  if (Identify) data.check<-data.Used[1,]  # set up data frame to store identified points
  #
  fn.plot <- function() {
    temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars]
    temp1 <- temp[, AnalyticVars[1]]
    if (qqPlot) {
      qqnorm.pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i.group]))
      qqline(temp1)
      qqtest(data = temp1, dist = "normal", drawPercentiles = T,
                       main = paste(AnalyticVars[1],"source", groups[i.group]))
      }
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
      }
    temp2 <- temp[, AnalyticVars[2]]
    if (qqPlot) {
      qqnorm.pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i.group]))
      qqline(temp2)
      qqtest(data = temp2, dist = "normal", drawPercentiles = T,
           main = paste(AnalyticVars[2],"source", groups[i.group]))
      browser()
      }
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
    }
    ADp1 <- ad.test(temp1)$p.value
    ADp2 <- ad.test(temp2)$p.value
    SWp1 <- shapiro.test(temp1)$p.value
    SWp2 <- shapiro.test(temp2)$p.value
    mardia <- MVN::mvn(data = temp, mvnTest="mardia")
    p.mardia.skew <- as.numeric(as.character(mardia[[1]][[3]][[1]], mode="numeric"))
    p.mardia.kurtosis <- as.numeric(as.character(mardia[[1]][[3]][[2]], mode="numeric"))
    if (qqPlot) {
      hz <- MVN::mvn(data=temp, mvnTest="hz",multivariatePlot="qq")
      browser()
      }
      else  hz <- MVN::mvn(data=temp, mvnTest="hz")
    p.hz <- as.numeric(hz[[1]][[3]], mode = "numeric")
    royston <- MVN::mvn(data=temp, mvnTest="royston")
    p.royston <- as.numeric(royston[[1]][[3]], mode = "numeric")
    p.temp <- c(ADp1, ADp2, SWp1, SWp2, p.mardia.skew, p.mardia.kurtosis, p.hz, p.royston)  # return p-values
    p.temp
  } # end of definition of function
  #
  pvalues <- matrix(NA, nrow=length(groups), ncol = 8)
  par(mfrow = c(2, 2))
  for (i.group in 1:length(groups)) {
    if ((i.group > 1) & (qqPlot==T)) {
      plot.new()
      plot.new()
      plot.new() # three blank plots, so next group starts in new window
      }
    pvalues[i.group, ] <- fn.plot()
    }
  #
  numeric.pvalues<-as.numeric(pvalues)
  numeric.pvalues<-round(numeric.pvalues,dig=pvalue.digits)
  numeric.pvalues[numeric.pvalues < 0] <- NA
  return.pvalues<-matrix(numeric.pvalues,nrow=length(groups),ncol=8)
  dimnames(return.pvalues) <- list(groups, c(paste("AD.",AnalyticVars,sep=""), paste("SW.",AnalyticVars,sep=""),
                                             "Mardia.skew", "Mardia.kurtosis", "hz", "Royston"))
  #
  if (substr(folder,1,1) != " ")
    if (substr(ds.pvalues,1,1) != " ") write.csv(returnname.pvalues, file = paste(folder, ds.pvalues, sep = ""))
  #
  #  remove duplicated observations from data.check
  #
  if (Identify) {
    data.check<-data.check[-1,]  # remove dummy first row
    if (labID != " ") index<-duplicated(data.check[,labID])
    else  index<-duplicated(data.check[,c(GroupVar,AnalyticVars)])
    if (length(index) > 0)  data.check<-data.check[!index,]
    if (labID != " ") {
      index.ID<-order(data.check[,labID])
      data.check<-data.check[index.ID,]
    }
  }
  #
  fcn.date.ver<-c(doc,date(),R.Version()$version.string)
  parameters<-c(groupVar=GroupVar,groups=Groups,digits.pvalue=pvalue.digits,qqPlot=qqPlot, Identify=Identify)
  if (substr(folder,1,1) == " ") {
    if ((!Identify)) out<-list(usage=fcn.date.ver,dataUsed=data.Used,analyticVars=AnalyticVars,parameters=parameters,
                               pvalues=return.pvalues)
    if (( Identify)) out<-list(usage=fcn.date.ver,dataUsed=data.Used,analyticVars=AnalyticVars,parameters=parameters,
                               pvalues=return.pvalues,data.check=data.check)
    }
  if (substr(folder,1,1) != " ") {
    if (!Identify) {
      file<-paste(folder,ds.pvalues,sep="")
      out<-list(usage=fcn.date.ver,data.Used=dataUsed,analyticVars=AnalyticVars,parameters=parameters,
                pvalues=return.pvalues,file=file)
      }
    if ( Identify) {
      file<-list(pvalues=paste(folder,ds.pvalues,sep=""),data,check=paste(folder,ds.data.check,sep=""))
      out<-list(usager=fcn.date.ver,data.Used=dataUsed,analyticVars=AnalyticVars, parameters=parameters,
                pvalues=return.pvalues,data.check=data.check,file=file)
      }
    }
  out
}
