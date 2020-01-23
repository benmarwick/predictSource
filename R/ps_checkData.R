#'  ps_checkData
#'
#' Data checks and summaries: duplicate records, negative analytic values,
#'  numbers of analytic results, percentiles of results
#'
#' @param doc A character string written to the output list; default is the function name
#' @param data  An R object (data frame) containing analytic data
#' @param CheckDupVars  A vector with names of identifying variables, typically group and lab ID
#' @param GroupVar  The name of variable defining the groups (required)
#' @param Groups  A character vector of groups by which numbers of samples and statistics will be
#'  tabulated or "All"
#' @param ByGroup  Logical: default is TRUE. If FALSE, tabulations are for all groups combined
#' @param ID  The name of lab ID, default is " " (no lab ID)
#' @param AnalyticVars  A character vector of names of analytic variables for which tabulations are done
#' @param folder  The path to the folder in which data frames will be saved; default is " ", no path
#'
#' @return  The function returns a list with the following components
#'  \itemize{
#' \item{usage:}{  A string with the contents of the argument doc, date run, R version used}
#' \item{dataUsed:}{  The data frame specified by the argument data and GroupVar}
#' \item{params:}{  A character vector with the values of CheckDupVars, GroupVar, and Groups}
#' \item{analyticVars:}{  The vector of names specified by the argument AnalyticVars}
#' \item{Duplicates:}{  A data frame containing the observations with duplicate values}
#' \item{NegativeValues:}{  A data frame containing the observations with at least one negative
#'  value for a variable in AnalyticVars}
#' \item{Nvalues:}{  A data frame contain the number of observations with a value for each analytic variable}
#' \item{statistics:}{  A data frame containing the descriptive statistics (by group, if ByGroup = TRUE)}
#' \item{location:}{  The value of the parameter folder}
#' }
#'
#' @section Detail:
#' AnalyticVars must be a vector of length at least 2.  If Groups specifies selected groups (is
#' not equal to "All"), it must be a vector of length at least 2.  The function returns a list
#' with four data frames: duplicate observations, observations with negative
#' values for one or more analytic variables, numbers of observations for each analytic
#' variable, and descriptive statistics (quantiles and number missing).
#' If the largest values is < 10 (true if use log10 transforms), the descriptive statistics
#' are rounded to 2 digits, otherwise to integers.
#' If ByGroup=TRUE, numbers of observations and statistics statistics are by group.
#'
#' @import  assertthat
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' dataCheck<-ps_checkData(data=ObsidianSources,CheckDupVars=analyticVars,GroupVar="Code",Groups="All",
#' ByGroup=TRUE, ID = "ID", AnalyticVars=analyticVars)
#'
#' @export

ps_checkData <-
  function(doc = "ps_checkData",
           data,
           CheckDupVars,
           GroupVar,
           Groups = "All",
           ByGroup = TRUE,
           ID = " ",
           AnalyticVars,
           folder = " ") {
    #
    #  check for valid parameters
    #
    assert_that(is.data.frame(data), msg="parameter data not a data frame")
    assert_that(is.character(CheckDupVars) & is.vector(CheckDupVars),
                msg="parameter CheckDupVars not a vector of names")
    assert_that(is.character(GroupVar), msg="parameter GroupVar not character")
    assert_that(is.character(Groups), msg="parameter Groups not character")
    assert_that(is.logical(ByGroup), msg="parameter ByGroup not logical")
    assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
                msg="parameter AnalyticVars not a character vector")
    assert_that(is.character(ID), msg="parameter ID not character valued")
    #
    #
    #  restrict data if specified
    #
    if (Groups[1] != "All") {
      UseRows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[UseRows, ]
    }
    else  dataUsed <- data
    #
    #  sort on GroupVar and ID if specified
    #
      rowsSort <- order(dataUsed[,GroupVar])
      dataUsed <- dataUsed[rowsSort,]
     if (ID[1] != " ") {
      rowsSort <- order(dataUsed[,ID])
      dataUsed <- dataUsed[rowsSort,]
    }
    #
    #  check for duplicates
    #
    DupRows <- duplicated(dataUsed[, CheckDupVars])
    DupRowNumbers <- (1:nrow(dataUsed))[DupRows]
    if (sum(DupRowNumbers) == 0)
        duplicates <- NA
    else  duplicates <- data[DupRows, ]
    #
    #  data summaries
    #
    MinimumValues <- apply(dataUsed[, AnalyticVars], 1, min, na.rm = T)
    NegRows <- (MinimumValues < 0)
    if (sum(NegRows) == 0)
      NegativeValues <- NA
    else    NegativeValues <- data[NegRows, ]
    #
      if (!ByGroup) {
      Nvalues <- rep(0, length(AnalyticVars))
      for (i in 1:length(AnalyticVars))
        Nvalues[i] <- sum(!is.na(data[, AnalyticVars[i]]))
      names(Nvalues)<-AnalyticVars
      }
    #
    if (ByGroup & (Groups[1] == "All")) {
      groups <- as.character(unique(dataUsed[, GroupVar]))
      n_groups <- length(groups)
      nvalues <- matrix(0, nrow = n_groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n_groups) {
        data_groupi <- dataUsed[dataUsed[, GroupVar] == groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <- sum(!is.na(data_groupi[, AnalyticVars[j]]))
      }  # end of loop on i
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(groups, "Total"), nvalues)
    } # end of code for Groups == "All"
    #
    if (ByGroup & (Groups[1] != "All")) {
      n_groups <- length(Groups)
      nvalues <- matrix(0, nrow = n_groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n_groups) {
        data_groupi <- dataUsed[dataUsed[, GroupVar] == Groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <- sum(!is.na(data_groupi[, AnalyticVars[j]]))
      } # end of loop on i
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(Groups, "Total"), nvalues)
    } # end of loop for restricted groups
    #
    #  descriptive statistics
    #
    if (!ByGroup) {
      statistics <- matrix(NA, nrow = length(AnalyticVars), ncol = 7)
      for (j in 1:length(AnalyticVars)) {
        statisticsj <- summary(dataUsed[, AnalyticVars[j]])
        statistics[j, 1:length(statisticsj)] <- statisticsj
      } # end of loop on j
      statistics_values <- statistics
      }
      # end of code for ByGroup = FALSE
    else if (ByGroup) {
      if (Groups[1] == "All")  groups <- as.character(unique(dataUsed[, GroupVar]))
        else  groups <- Groups
      n_groups <- length(groups)
      n_vars <- length(AnalyticVars)
      statistics_values <- matrix(NA, nrow = (n_groups + 1) *
                                 n_vars, ncol = 7)
      row <- 0
      vector_groups <- rep(c(groups, " "), n_vars)
      vector_values <- rep(" ", (n_groups + 1) * n_vars)
      for (i in 1:n_vars) {
        vector_values[(row + 1):(row + n_groups)] <- AnalyticVars[i]
        for (j in 1:n_groups) {
          data_valuesij <- dataUsed[dataUsed[, GroupVar] == groups[j],
                                AnalyticVars[i]]
          statisticsij <- summary(data_valuesij)
          statistics_values[row + j, 1:length(statisticsij)] <- statisticsij
        }  # end of loop on j
        row <- row + n_groups + 1
      }  # end of loop on i
      }  # end of code for specified groups
      #
      #  round descriptive statistics to 2 digits if all values < 10, else round to integers
      #
      if (max(statistics_values[,-7],na.rm=TRUE) < 10)
        statistics_values[,-7]<-round(statistics_values[,-7],digits = 2)
      else  statistics_values[,-7]<-round(statistics_values[,-7],digits = 0)
      colnames(statistics_values) <- c("min", "Q1", "median",
                                    "mean", "Q3", "max", "n_missing")
      #  if no missing values, replace NA by 0
      statistics_values[is.na(statistics_values[, 7]), 7] <- 0
      #
      if (!ByGroup)
        statistics <- data.frame(Analysis = AnalyticVars, statistics_values)
      else  statistics <- data.frame(Analysis = vector_values, Group = vector_groups,
                            statistics_values)
     #
    fcnDateVersion<-c(doc,date(),R.Version()$version.string)
    params<-list(CheckDupVars,GroupVar,Groups)
    names(params)<-c("CheckDupVars","GroupVar","Groups")
    statistics[,"mean"] <- round(statistics[,"mean"], digits = 0)
  #
 list(usage=fcnDateVersion,
           dataUsed=data,
           params=params,
           analyticVars=AnalyticVars,
           Duplicates = duplicates,
           NegativeValues = NegativeValues,
           Nvalues = Nvalues,
           statistics = statistics,
           location=folder)
  }
