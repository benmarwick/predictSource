#'  fn.CheckData
#'
#' Data checks and summaries: duplicate records, negative analytic values, numbers of analytic results, percentiles of results
#'
#' @param doc: a character string written to the output list; default is the function name
#' @param data: R object (data frame) containing analytic data
#' @param CheckDupVars: vector with names of identifying variables, typically group and lab ID
#' @param GroupVar: if there are groups, name of variable defining the groups, default value of " ": no grouping
#' @param Groups: character vector of groups by which numbers of samples and statistics statistics will be
#'  tabulated
#'   default value of " ": tabulations are done for the entire data set
#'   value = "All": tabulation for each distinct code in GroupVar
#' @param ID: name of lab ID, default is " " (no lab ID)
#' @param AnalyticVars: character vector of names of analytic variables for which tabulations are done
#' @param folder:  the path to a folder in which data frames will be saved; default is " "
#' #``
#' @return
#'   Four data frames with duplicate observations, observations with negative
#'       values for one or more analytic variables, numbers of observations for each analytic
#'       variable, and statistics statistics (quartiles and number missing)
#'       if Groups != " ", numbers of observations and statistics statistics are by group
#'   A list with the following components:
#'   \itemize{
#' \item{usage:}{  A string with the contents of the argument doc, date run, R version used}
#' \item{dataUsed:}{  The data frame specified by the argument data and GroupVar}|
#' \item{params:}{  A character vector with the values of CheckDupVars, GroupVar, and Groups}
#' \item{analyticVars:}{  The vector of names specified by the argument AnalyticVars}
#' \item{Duplicates:}{  A data frame containing the observations with duplicate values}
#' \item{NegativeValues:}{  A data frame containing the observations with at least one negative value for a variable in AnalyticVars}
#' \item{Nvalues:}{  A data frame contain the number of observations with a value for each analytic variable}
#' \item{statistics:}{  A data frame containing the statistics statistics (by group, if Group is specified)}
#' \item{location:}{  If folder != " ", the path to the folder in which data frames will be saved}
#'   }
#'
#' @section Detail:
#' AnalyticVars must be a vector of length at least 2.  If Groups specifies selected groups (is
#' not equal to "All" or " "), if must be a vector of length at least 2.
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' CheckData<-fn.CheckData(data=ObsidianSources,CheckDupVars=c("Code","ID"),GroupVar="Code",Groups="All",
#'   ID = "ID", AnalyticVars=analyticVars)
#'
#' @export

fn.CheckData <-
  function(doc = "fn.CheckData",
           data,
           CheckDupVars,
           GroupVar = " ",
           Groups,
           ID = " ",
           AnalyticVars,
           folder = " ") {
    #
    #  restrict data if specified
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      UseRows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[UseRows, ]
    }
    else  data.Used <- data
    #
    #  sort on GroupVar and ID if specified
    #
    if (GroupVar[1] != " ") {
      rowsSort <- order(data.Used[,GroupVar])
      data.Used <- data.Used[rowsSort,]
    }
    if (ID[1] != " ") {
      rowsSort <- order(data.Used[,ID])
      data.Used <- data.Used[rowsSort,]
    }
    #
    #  check for duplicates
    #
    DupRows <- duplicated(data.Used[, CheckDupVars])
    DupRowNumbers <- (1:nrow(data.Used))[DupRows]
    if (sum(DupRowNumbers) == 0)
        duplicates <- NA
    else  duplicates <- data[DupRows, ]
    #
    #  data summaries
    #
    MinimumValues <- apply(data.Used[, AnalyticVars], 1, min, na.rm = T)
    NegRows <- (MinimumValues < 0)
    if (sum(NegRows) == 0)
      NegativeValues <- NA
    else    NegativeValues <- data[NegRows, ]
 #
      if ((GroupVar[1] == " ") | (Groups[1] == " ")) {
      Nvalues <- rep(0, length(AnalyticVars))
      for (i in 1:length(AnalyticVars))
        Nvalues[i] <- sum(!is.na(data[, AnalyticVars[i]]))
      }
    #
    if (Groups[1] == "All") {
      groups <- as.character(unique(data.Used[, GroupVar]))
      n.groups <- length(groups)
      nvalues <- matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n.groups) {
        data.groupi <- data.Used[data.Used[, GroupVar] == groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <- sum(!is.na(data.groupi[, AnalyticVars[j]]))
      }  # end of loop on i
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(groups, "Total"), nvalues)
    } # end of code for Groups == "All"
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      n.groups <- length(Groups)
      nvalues <- matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n.groups) {
        data.groupi <- data.Used[data.Used[, GroupVar] == Groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <- sum(!is.na(data.groupi[, AnalyticVars[j]]))
      } # end of loop on i
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(Groups, "Total"), nvalues)
    } # end of loop for restricted groups
#
    if (GroupVar == " ") {
      statistics <- matrix(NA, nrow = length(AnalyticVars), ncol = 7)
      for (j in 1:length(AnalyticVars)) {
        statisticsj <- summary(data.Used[, AnalyticVars[j]])
        statistics[j, 1:length(statisticsj)] <- statisticsj
      } # end of loop on j
      statistics.values <- round(statistics, dig = 0)
      colnames(statistics.values) <- c("min", "Q1", "median",
                                       "mean", "Q3", "max", "n.missing")
      statistics.values[is.na(statistics.values[, 7]), 7] <- 0
     } # end of code for GroupVar == " "
    else if (Groups[1] != " ") {
      if (Groups[1] == "All")  groups <- as.character(unique(data.Used[, GroupVar]))
        else  groups <- Groups
      n.groups <- length(groups)
      n.vars <- length(AnalyticVars)
      statistics.values <- matrix(NA, nrow = (n.groups + 1) *
                                 n.vars, ncol = 7)
      row <- 0
      vector.groups <- rep(c(groups, " "), n.vars)
      vector.values <- rep(" ", (n.groups + 1) * n.vars)
      for (i in 1:n.vars) {
        vector.values[(row + 1):(row + n.groups)] <- AnalyticVars[i]
        for (j in 1:n.groups) {
          data.valuesij <- data.Used[data.Used[, GroupVar] == groups[j],
                                AnalyticVars[i]]
          statisticsij <- summary(data.valuesij)
          statistics.values[row + j, 1:length(statisticsij)] <- statisticsij
        }  # end of loop on j
        row <- row + n.groups + 1
      }  # end of loop on i
      }  # end of code for specified groups
      statistics.values <- round(statistics.values, dig = 0)
      colnames(statistics.values) <- c("min", "Q1", "median",
                                    "mean", "Q3", "max", "n.missing")
      statistics.values[is.na(statistics.values[, 7]), 7] <- 0
      if (GroupVar == " ")
        statistics <- data.frame(Analysis = AnalyticVars, statistics.values)
      else  statistics <- data.frame(Analysis = vector.values, Group = vector.groups,
                            statistics.values)
      #
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    params<-list(CheckDupVars,GroupVar,Groups)
    names(params)<-c("CheckDupVars","GroupVar","Groups")
    statistics[,"mean"] <- round(statistics[,"mean"], dig = 0)
  #
 list(usage=fcn.date.ver,
                                 dataUsed=data,params=params,
                                 analyticVars=AnalyticVars,
                                 Duplicates = duplicates,
                                 NegativeValues = NegativeValues,
                                 Nvalues = Nvalues,
                                 statistics = statistics,
                                 location=folder)
  }
