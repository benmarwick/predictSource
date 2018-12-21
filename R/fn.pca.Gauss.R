#' fn.pca.Gauss
#'
#' Check whether first two principal components are Gaussian
#'
#' @param doc  documentation for the analysis, default if the function name
#' @param data  R matrix or data frame containing the data to be analyzed
#' @param GroupVar  name for variable defining grouping; a group variable is required
#' @param Groups  vector of values of group variable for which plots are to be done;
#' if "All"', use all groups
#' @param gaussID: optional name for an ID, default is " " if no ID
#' @param AnalyticVars  vector of names (character values) of analytic results
#' @param qqPlot  Logical, should Q-Q plots (univariate with the bootstrap envelope, multivariate)
#'  be shown; default is T
#' @param gaussIdentify  Logical, should user identify points of interest, default is F
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section  Details:
#' This function uses the function fn.2dPlot.Gauss().  The function produces p-values
#'  from univariate and multivariate tests of normality.  It produces Q-Q plots
#'  of the first two principal components for each group, as well as those plots with bootstrap
#'  envelopes and the bivariate Q-Q plot if qqPlot=T.
#'
#' @return  The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params.grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params.logical:}{ The value of QQtest}
#'   \item{p.values:}{ A data frame with the p-values for the Gaussian assumptions for each
#'    group specified}
#'  \item{data.check:}{  A data frame with data identified as generating points of interest;
#'  value is NA if no points are identified}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#'@section  Details
#'
#' The function uses the function fn.2dPlot.Gauss().
#'
#' @import MASS nortest qqtest MVN
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' pca.Gauss <- fn.pca.Gauss(data=ObsidianSources, GroupVar="Code",Groups=c("A","B"),
#'   AnalyticVars=analyticVars)
#'
#' @export
#'

fn.pca.Gauss <-
  function(doc = "fn.pca.Gauss",
           data,
           GroupVar,
           Groups,
           gaussID = " ",
           AnalyticVars,
           qqPlot = T,
           gaussIdentify = F,
           folder = " ")
    {
    # restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, ]
    } else
      data.Used <- data[, ]
    #
    dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with data kept
    data.Used <- data.Used[dataKeep,]
    #
    #  sort on GroupVar and ID if specified
    #
    if (GroupVar[1] != " ") {
      rowsSort <- order(data.Used[,GroupVar])
      data.Used <- data.Used[rowsSort,]
    }
    if (gaussID[1] != " ") {
      rowsSort <- order(data.Used[,gaussID])
      data.Used <- data.Used[rowsSort,]
    }
    #
    # define variable groups as groups used in analysis
    if (Groups[1] == "All")
      groups <-
        as.character(unique(data.Used[, GroupVar]))
    else
      groups <- as.character(Groups)
    #
    pca <- prcomp(data.Used[, AnalyticVars], scale = TRUE)
    # predicted values for first two components
    predict.pc1 <- predict(pca)[, 1]
    predict.pc2 <- predict(pca)[, 2]
    # add numeric code for group to data set
    GroupIndex <- rep(NA, nrow(data.Used))
    for (i in 1:nrow(data.Used)) {
      for (j in 1:length(groups))
        if (data.Used[i, GroupVar] == groups[j])
          GroupIndex[i] <- j
    }

    pvalues <-
      matrix(NA, length(groups), 8)  # Anderson-Darling, Shapiro-Wilk, multivariate test p-values
    dimnames(pvalues) <-
      list(
        groups,
        c(
          "ADpc.1",
          "ADpc.2",
          "SWpc.1",
          "SWpc.2",
          "Mardia.skew",
          "Mardia.kurtosis",
          "HZ",
          "Royston"
        )
      )
    DataPlusPredicted <- data.frame(data.Used,predict(pca))
    pc.GroupVar <- GroupVar
    pc.Groups <- Groups
    pc.qqPlot <- qqPlot
    pc.digits <- 3
    #
    outGauss <- fn.2dPlot.Gauss(data=DataPlusPredicted, GroupVar=pc.GroupVar, Groups=pc.Groups,
                           AnalyticVars=c("PC1", "PC2"), ID=gaussID, qqPlot=pc.qqPlot, pvalue.digits=pc.digits,
                           Identify=gaussIdentify)
    browser()
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(qqPlot, gaussIdentify)
    names(params.logical)<-c("qqPlot","gaussIdentify")
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep]
    else dataNA <- NA
    #
    if (gaussIdentify == T) {
    if (gaussID == " ") data.check<-outGauss$data.check[,c(GroupVar, AnalyticVars)]
    else  data.check<-outGauss$data.check[,c(GroupVar, gaussID, AnalyticVars)]
    }
    else  data.check <- NA
    #
    out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                params.logical=params.logical,
                p.values = outGauss$pvalues,
                data.check = data.check,
                location=folder)
    out
  }
