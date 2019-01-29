#' fn.2dPlot.Gauss
#'
#' Plot and check specified pairs of analysis variables for bivariate normality.
#'
#' @param doc  A string documenting use written to the output list; default is the function name
#' @param data  R matrix or data frame containing the data to be analyzed
#' @param GroupVar  Name for the variable defining grouping, " " if no grouping
#' @param ID  Name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  Vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars  Vector of length two with the names of two analytic variables
#'  to be shown in the plots
#' @param scatterPlot  Logical (default is T): specify whether to show scatter plots when qqPlot = F
#' @param qqPlot  Logical (default is T): specify whether to show the q-q plots with the bootstrap
#' envelopes and multivariate plots
#' @param pvalue.digits  Numeric (default is 3): number of significant digits retained in tests for normality
#' @param Identify  Logical(default is F): if T, user can identify points of interest in the plots
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage:}{  String with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{analyticVars:}{  The contents of the argument AnalyticVars}
#' \item{params:}{  A vector with argument values for GroupVar, Groups, pvalue.digits, scatterPlot, qqPlot}
#' \item{pvalues:}{  A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{data.check:}{ A data frame with the information on user-identified points of interest;
#'     value is c(NA, NA) if no data identified}
#' \item{location:}{ The value of the parameter folder}
#' }
#'
#' @section Details:
#'  If qqPlot = T, by default each page has panes in two rows and three columns
#'  (but there is a new page for successive groups). The function stops after producing each row
#'   of each plot.  Enter c ("continue") at the prompt to get the next plot.
#'   If qqPlot = F, only the standard qq plots are shown, and the function stops after producing each
#'   page.
#'   See the vignette for more information: visualizing each plot, the information obtained
#'    by using the package qqtest, the tests for bivariate normality, and identifying points of interest.
#'
#' @import MASS  qqtest  MVN
#'
#' @examples
#' data(ObsidianSources)
#' plot.2d.Gauss<-fn.2dPlot.Gauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups=c("A","B"),
#'    AnalyticVars=c("Rb","Zr"))
#'
#' @export
#'
fn.2dPlot.Gauss <- function (doc = "fn.2dPlot.Gauss",
                             data,
                             GroupVar,
                             ID,
                             Groups,
                             AnalyticVars,
                             scatterPlot=T,
                             qqPlot = T,
                             pvalue.digits=3,
                             Identify=F,
                             folder=" "
                            )
{
  if (length(AnalyticVars)!=2)  stop("length of AnalyticVars must be 2")
  #
  if (Groups[1] != "All") {
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows, ]
  }
  else data.Used <- data
  #
  dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with
  # no missing values
  for (i in 1:length(AnalyticVars))
    dataKeep[is.na(data.Used[,AnalyticVars[i]])] <- F
  #
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
  if (Identify) data.check<-data.Used[1,]
      else  data.check <- c(NA, NA)  # set up data frame to store identified points
  #
  fn.plot <- function() {
    temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars]
    temp1 <- temp[, AnalyticVars[1]]
    temp1 <- temp1[!is.na(temp[,AnalyticVars[1]])]
    if (qqPlot | Identify) {
      qqtest(data = temp1, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[1],"source", groups[i.group]))
      qqnorm.pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i.group]))
      qqline(temp1)
    }
    else if (scatterPlot) {
      qqnorm.pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i.group]))
      qqline(temp1)
      }
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
      }
    temp2 <- temp[, AnalyticVars[2]]
    temp2 <- temp2[!is.na(temp[,AnalyticVars[2]])]
    if (qqPlot | Identify) {
      qqtest(data = temp2, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[2],"source", groups[i.group]))
      qqnorm.pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i.group]))
      qqline(temp2)
    }
    else if (scatterPlot) {
      qqnorm.pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i.group]))
      qqline(temp2)
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
    temp <- temp[!is.na(temp[,AnalyticVars[1]]) & !is.na(temp[,AnalyticVars[2]]),]
    n.samples <- nrow(temp)
    mardia <- MVN::mvn(data = temp, mvnTest="mardia")
    p.mardia.skew <- as.numeric(as.character(mardia[[1]][[3]][[1]],mode="character"))
    p.mardia.kurtosis <- as.numeric(as.character(mardia[[1]][[3]][[2]],mode="character"))
    if (qqPlot)
      HZ <- MVN::mvn(data=temp, mvnTest="hz",multivariatePlot="qq")
      else  HZ <- MVN::mvn(data=temp, mvnTest="hz")
    #
    p.HZ <- as.numeric(HZ[[1]][[3]], mode = "numeric")
    royston <- MVN::mvn(data=temp, mvnTest="royston")
    p.Royston <- as.numeric(royston[[1]][[3]], mode = "numeric")
    p.temp <- c(n.samples, ADp1, ADp2, SWp1, SWp2, p.mardia.skew, p.mardia.kurtosis, p.HZ, p.Royston)
    # return p-values
    p.temp
  } # end of definition of function
  #
  pvalues <- matrix(NA, nrow=length(groups), ncol = 9)
  #
  if (qqPlot)  par(mfrow = c(2,3))
  else par(mfrow = c(2,2))
  #
  iPlot<-0 # counter used when qqPlot=F
  #
  for (i.group in 1:length(groups)) {
    iPlot <- iPlot+1
    pvalues[i.group, ] <- fn.plot() # plot for this group and compute p-values
    if (qqPlot==T)  {
      browser()
      plot.new() # blank plot so next group starts in new window
    }
    # two plots per frame if qqPlot=F
    if ((qqPlot==F) & (floor(i.group/2)==i.group/2))  browser()
  } # end of loop on i.group
#     if ((qqPlot==F) & (as.integer(i.group/2)==i.group/2)) {
 #       plot.new() # blank plots so next group starts in new window
  #    pvalues[i.group, ] <- fn.plot()
   # }
  #
  numeric.pvalues<-as.numeric(pvalues)
  numeric.pvalues<-round(numeric.pvalues,dig=pvalue.digits)
  numeric.pvalues[numeric.pvalues < 0] <- NA
  return.pvalues<-matrix(numeric.pvalues,nrow=length(groups),ncol=9)
  dimnames(return.pvalues) <- list(groups, c("n", paste("AD.",AnalyticVars,sep=""), paste("SW.",AnalyticVars,sep=""),
                                             "maridaSkew", "mardiaKurtosis", "HZ", "Royston"))
  #
  #  remove duplicated observations from data.check
  #
  if (Identify) {
    data.check<-data.check[-1,]  # remove dummy first row
    if (ID != " ") index<-duplicated(data.check[,ID])
    else  index<-duplicated(data.check[,c(GroupVar,AnalyticVars)])
    if (length(index) > 0)  data.check<-data.check[!index,]
    if (ID != " ") {
      index.ID<-order(data.check[,ID])
      data.check<-data.check[index.ID,]
    }
  } # end of code for Identify=T
  #
  fcn.date.ver<-c(doc,date(),R.Version()$version.string)
  params<-c(groupVar=GroupVar,groups=Groups,digits.pvalue=pvalue.digits,
                scattaerPlot=scatterPlot, qqPlot=qqPlot, Identify=Identify)
  if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep,]
  else dataNA <- NA
  #
  list(         usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params=params,
                pvalues=return.pvalues,
                data.check=data.check,
                location=folder)
  }
