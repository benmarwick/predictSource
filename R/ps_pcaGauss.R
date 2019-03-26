#' ps_pcaGauss
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
#' This function uses the function ps_2dPlotGauss().  The function produces p-values
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
#'   \item{params_grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params_logical:}{ The value of QQtest}
#'   \item{p_values:}{ A data frame with the p-values for the Gaussian assumptions for each
#'    group specified}
#'  \item{data_check:}{  A data frame with data identified as generating points of interest;
#'  value is NA if no points are identified}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#' @import MASS nortest qqtest MVN
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' pca_Gauss <- ps_pcaGauss(data=ObsidianSources, GroupVar="Code",Groups=c("A","B"),
#'   AnalyticVars=analyticVars)
#'
#' @export
#'

ps_pcaGauss <-
  function(doc = "fn_pca_Gauss",
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
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows, ]
    } else
      dataUsed <- data[, ]
    #
    #  sort on GroupVar and ID if specified
    #
    if (GroupVar[1] != " ") {
      rowsSort <- order(dataUsed[,GroupVar])
      dataUsed <- dataUsed[rowsSort,]
    }
    if (gaussID[1] != " ") {
      rowsSort <- order(dataUsed[,gaussID])
      dataUsed <- dataUsed[rowsSort,]
    }
    dataKeep <- rep(T, nrow(dataUsed)) # will contain indices for observations with no missing data
    for (i in 1:nrow(dataUsed)) {
      for (j in 1:length(AnalyticVars))
        if (is.na(dataUsed[,AnalyticVars][i,j]))  dataKeep[i] <- F
    }
    if (sum(dataKeep) < nrow(dataUsed))  dataNA<-dataUsed[!dataKeep,]
      else  dataNA<-NA
    #
    dataUsed <- dataUsed[dataKeep,]
    #
    # define variable groups as groups used in analysis
    if (Groups[1] == "All")
      groups <-
        as.character(unique(dataUsed[, GroupVar]))
    else
      groups <- as.character(Groups)
    #
    pca <- prcomp(dataUsed[, AnalyticVars], scale = TRUE)
    # predicted values for first two components
    predict_pc1 <- predict(pca)[, 1]
    predict_pc2 <- predict(pca)[, 2]
    # add numeric code for group to data set
    GroupIndex <- rep(NA, nrow(dataUsed))
    for (i in 1:nrow(dataUsed)) {
      for (j in 1:length(groups))
        if (dataUsed[i, GroupVar] == groups[j])
          GroupIndex[i] <- j
    }

    pvalues <-
      matrix(NA, length(groups), 8)  # Anderson-Darling, Shapiro-Wilk, multivariate test p-values
    dimnames(pvalues) <-
      list(
        groups,
        c(
          "ADpc_1",
          "ADpc_2",
          "SWpc_1",
          "SWpc_2",
          "Mardia_skew",
          "Mardia_kurtosis",
          "HZ",
          "Royston"
        )
      )
    DataPlusPredicted <- data.frame(dataUsed,predict(pca))
    pc_GroupVar <- GroupVar
    pc_Groups <- Groups
    pc_qqPlot <- qqPlot
    pc_digits <- 3
    #
    outGauss <- ps_2dPlotGauss(data=DataPlusPredicted, GroupVar=pc_GroupVar, Groups=pc_Groups,
                           AnalyticVars=c("PC1", "PC2"), ID=gaussID, qqPlot=pc_qqPlot, pvalue_digits=pc_digits,
                           Identify=gaussIdentify)
    browser()
    #
    fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-c(qqPlot, gaussIdentify)
    names(params_logical)<-c("qqPlot","gaussIdentify")
    #
    if (gaussIdentify == T) {
    if (gaussID == " ") data_check<-outGauss$data_check[,c(GroupVar, AnalyticVars)]
    else  data_check<-outGauss$data_check[,c(GroupVar, gaussID, AnalyticVars)]
    }
    else  data_check <- NA
    #
    out<-list(usage=fcnDateVersion,
                dataUsed=dataUsed,
                dataNA=dataNA,
                analyticVars=AnalyticVars,
                params_grouping=params_grouping,
                params_logical=params_logical,
                p_values = outGauss$pvalues,
                data_check = data_check,
                location=folder)
    out
  }
