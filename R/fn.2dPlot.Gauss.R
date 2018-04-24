#' fn.2dPlot.Gauss
#'
#' Plot and check specified pairs of analysis variables for bivariate normality. The function stops after producing each plot.  Enter c ("continue") at the prompt to get the next plot.  If this function is run using Rstudio, each plot appears in a separate window, not in the Rstudio plot pane.
#'
#' @param doc  A string documenting use written to the output list; default is the function name
#' @param data  R matrix or data frame containing the data to be analyzed
#' @param GroupVar  Name for the variable defining grouping, " " if no grouping
#' @param labID  Name for the variable with a lab ID, " " if no lab ID is used
#' @param Groups  Vector of values of the group variable for which plots are to be done;
#'    if "All", use all groups; if " ", no grouping
#' @param AnalyticVars  Names of two analytic variables to be shown in the plots, vector of length 2 or matrix with 2 columns;
#'     if a matrix, the set of plots is produced for each row
#' @param QQtest  Logical (default is T): specify whether to run the package qqtest
#' @param pvalue.digits  Numeric (default is 3): number of significant digits retained in tests for normality
#' @param Identify  Logical(default is F): if T, user can identify points of interest in the plots
#' @param folder  In Windows, path to the folder containing an excel file with the identified points, ending with two forward slashes;
#'         if " ", no data set is written
#' @param ds.identified  Excel file name with extension .csv containing information on the identified points
#'
#' @return   A list with the following components:
#'  \itemize{
#' \item{usage}{  Vector with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed}{ The contents of the argument data restricted to the groups used}
#' \item{analyticVars}{  The contents of the argument AnalyticVars}
#' \item{parameters}{  A vector with argument values for GroupVar, Groups, pvalue.digits, and QQtest}
#' \item{pvalues}{  A matrix with the p-values for univariate and bivariate tests of normality}
#' \item{data.check}{ If Identify = T, a data frame with the information on user-identified points of interest}
#' \item{file}{ If folder != " ", a string or list: the path to the excel file or files with pvalues and, if Identify = T,
#'  the information on user-identified points of interest}
#' }
#'
#' @section Details:
#'  See the vignette for more information: visualizing each plot, the information obtained by using the package qqtest,
#'   the tests for bivariate normality, and identifying points of interest.
#'
#' @examples
#' data(ObsidianSources)
#' fn.2dPlot.Gauss(data = ObsidianSources,
#'           Groups = " ",
#'           GroupVar = " ",
#'           ByGroup = FALSE,
#'           Selections = c(4,5,6))
#'
#' @import MASS  qqtest
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' 2dPlot.Gauss<-fn.2dPlot.Gauss(data = ObsidianSources,
#'                              GroupVar = "Code",
#'                              labID = "ID",
#'                              Groups = c("A","B"),
#'                              AnalyticVars = rbind(analyticVars[1:2,],analyticVars[c(1,3),]))
#'
#' @export
#'
fn.2dPlot.Gauss <- function (doc = "fn.2dPlot.Gauss", data, GroupVar,labID, Groups,
          AnalyticVars, QQtest = T, pvalue.digits=3, Identify=F, folder=" ", ds.pvalues, ds.data.check)
{
  #
  #   doc: character variable returned with p values
  #   data: data frame containing character group variable and analytic variables
  #   GroupVar: name of grouping variable
  #   Groups: define groups to be used, can be a vector of character values
  #   AnalyticVars: names of two analytic variables to be considered
  #   QQtest: if true (default), use bootstrapping to show plots evaluating the qqlines for the variables in each group
  #   pvalue.digits: number of significant digits for p-values to be returned
  #   folder: character value, path to folder containing excel file with p-values, ending in \\
  #     if " ", no file is written
  #   ds.pvalues: character value with name of file, extension .csv
  #
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
  pvalues <- matrix(NA, length(groups), 6)
  dimnames(pvalues) <- list(groups, c(paste("AD.",AnalyticVars[1],sep=""), paste("AD.",AnalyticVars[2],sep=""),
                                      paste("SW.",AnalyticVars[1],sep=""),paste("SW.",AnalyticVars[2],sep=""), "Mardia.skew", "Mardia.kurtosis"))
  n.pages <- round(0.01+length(groups)/2, dig = 0)  # number of pages (rounds up with an odd number of groups)
  #
  if (Identify) data.check<-data.Used[1,]  # set up data frame to store identified points
  i.group <- 0
  fn.plot <- function() {
    temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
    temp1 <- temp[, AnalyticVars[1]]
    qqnorm.pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i.group]))
    qqline(temp1)
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
    }
    temp2 <- temp[, AnalyticVars[2]]
    qqnorm.pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i.group]))
    qqline(temp2)
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
    }
    ADp1 <- ad.test(temp1)$p.value
    ADp2 <- ad.test(temp2)$p.value
    SWp1 <- uniNorm(temp1, type = "SW")[[2]]$p.value
    SWp2 <- uniNorm(temp2, type = "SW")[[2]]$p.value
    mardia <- mardiaTest(data = temp)
    if (nrow(temp) >= 20)
      p.kurtosis <- mardia@p.value.kurt
    else p.kurtosis <- mardia@p.value.small
    c(ADp1, ADp2, SWp1, SWp2, mardia@p.value.skew, p.kurtosis)
  }
  for (page in 1:n.pages) {
    plot.new()
    par(mfrow = c(2, 2))
    i.group <- i.group + 1
    pvalues[i.group, ] <- fn.plot()
    i.group <- i.group + 1
    if (i.group <= length(groups))
      pvalues[i.group, ] <- fn.plot()
    browser()
  }
  #    fn.Mardia.plot <- function() {
  #        temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
  #        mardia <- mardiaTest(data = temp)
  #        mvnPlot(mardia, type = "persp")
  #        mvnPlot(mardia, type = "contour")
  #    }
  #    i.group <- 0
  #   for (page in 1:n.pages) {
  #        plot.new()
  #        par(mfrow = c(2, 2))
  #        i.group <- i.group + 1
  #        fn.Mardia.plot()
  #        i.group <- i.group + 1
  #        if (i.group <= length(groups))
  #            fn.Mardia.plot()
  #        browser()
  #    }
  if (QQtest) {
    fn.qqtest <- function() {
      temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
      temp1 <- temp[,AnalyticVars[1]]
      qqtest(data = temp1, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[1],"source", groups[i.group]))
      temp2 <- temp[,AnalyticVars[2]]
      qqtest(data = temp2, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[2],"source", groups[i.group]))
    }
    i.group <- 0
    for (page in 1:n.pages) {
      plot.new()
      par(mfrow = c(2, 2))
      i.group <- i.group + 1
      fn.qqtest()
      i.group <- i.group + 1
      if (i.group <= length(groups))
        fn.qqtest()
      browser()
    }
  }
  pvalues<-round(pvalues,dig=pvalue.digits)
  #
  if (substr(folder,1,1) != " ")
    if (substr(ds.pvalues,1,1) != " ") write.csv(pvalues, file = paste(folder, ds.pvalues, sep = ""))
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
  fcn.date.ver<-c(doc,date(),R.Version())
  parameters<-c(groupVar=GroupVar,groups=Groups,digits.pvalue=pvalue.digits,qqtest=QQtest)
  if (substr(folder,1,1) == " ") {
    if ((!Identify)) out<-list(usage=fcn.date.ver,dataUsed=data.Used,analyticVars=AnalyticVars,parameters=parameters,
                               pvalues=pvalues)
    if (( Identify)) out<-list(usage=fcn.date.ver,dataUsed=data.Used,analyticVars=AnalyticVars,parameters=parameters,
                               pvalues=pvalues,data.check=data.check)
    }
  if (substr(folder,1,1) != " ") {
    if (!Identify) {
      file<-paste(folder,ds.pvalues,sep="")
      out<-list(usage=fcn.date.ver,data.Used=dataUsed,analyticVars=AnalyticVars,parameters=parameters,
                pvalues=pvalues,file=file)
      }
    if ( Identify) {
      file<-list(pvalues=paste(folder,ds.pvalues,sep=""),data,check=paste(folder,ds.data.check,sep=""))
      out<-list(usager=fcn.date.ver,data.Used=dataUsed,analyticVars=AnalyticVars, parameters=parameters,
                pvalues=pvalues,data.check=data.check,file=file)
      }
    }
  out
}
