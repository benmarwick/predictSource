#'  fn.CheckData
#'
#'
#' data checks and summaries: duplicate records, negative analytic values, numbers of analytic results, percentiles of results
#'
#' @param doc: a character string written to the output list; default is the function name
#' @param data: R object (data frame) containing analytic data
#' @param CheckDupVars: vector with names of identifying variables, typically group and lab ID
#' @param GroupVar: if there are groups, name of variable defining the groups, default value of " ": no grouping
#' @param Groups: character vector of groups by which numbers of samples and summary statistics will be tabulated default value of " ": tabulations are done for the entire data set value = "All": tabulation for each distinct code in GroupVar
#' @param AnalyticVars: character vector of names of analytic variables for which tabulations are done
#' @param folder: path to folder in which to store excel files with results (e.g. "C:\\project\\results\\") or " "
#' @param ds.duplicates: file name for excel file with duplicate records, with extension csv (e.g. "duplicates.csv")
#' @param ds.Nsamples: file name for excel file with numbers of samples, with extension csv (e.g. "Nresults.csv")
#' @param ds.summary: file with summary statistics for each analytic variable check matrix stored in the object data for duplicates based on variables in the argument fields
#'
#' @return
#'   If folder != " ", four excel files with duplicate observations, observations with negative
#'       values for one or more analytic variables, numbers of observations for each analytic
#'       variable, and summary statistics (quartiles and number missing)
#'       if Groups != " ", numbers of observations and summary statistics are by group
#'   A list with the following components:
#' \item{usage}{a string with the contents of the argument doc, date run, R version used}
#' \item{dataUsed}{the data frame specified by the argument data}|
#' \item{params}{}
#' \item{analyticVars}{the vector of names specified by the argument AnalyticVars}
#' \item{Duplicates}{a data frame containing the observations with duplicate values}
#' \item{NegativeValues}{a data frame containing the observations with at least one negative value
#'          for a variable in AnalyticVars}
#' \item{Nvalues}{a data frame contain the number of observations with a value for each analytic variable}
#' \item{Summary}{a data frame containing the summary statistics (by group, if Group is specified)}
#'  \item{files}{if folder != " ", a list containing the folder name and the name of each of
#'         the excel files}
#'
#' @section Detail:
#' AnalyticVars must be a vector of length at least 2.
#' If folder = " ", no excel files are written.  If not, the path must end with \\.
#'
#' @examples
#'
#' @export

fn.CheckData <-
  function(doc = "fn.CheckData",
           data,
           CheckDupVars,
           GroupVar,
           Groups,
           AnalyticVars,
           folder = " ",
           ds.duplicates,
           ds.NegValues,
           ds.Nsamples,
           ds.summary) {
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      UseRows <- (data[, GroupVar] %in% Groups)
      data.subset <- data[UseRows, ]
      DupRows <- duplicated(data.subset[, CheckDupVars])
      DupRowNumbers <- (1:nrow(data.subset))[DupRows]
      if (sum(DupRowNumbers) == 0)
        duplicates <- NA
      else  duplicates <- data[DupRows, ]
    }
    else {
      DupRows <- duplicated(data[, CheckDupVars])
      DupRowNumbers <- (1:nrow(data))[DupRows]
      if (sum(DupRowNumbers) == 0)
        duplicates <- NA
      else duplicates <- data[DupRows, ]
    }
    if (substr(folder,1,1) != " ")  write.csv(duplicates, paste(folder, ds.duplicates,
                                                                sep = ""))
    #
    MinimumValues <- apply(data[, AnalyticVars], 1, min, na.rm = T)
    NegRows <- (MinimumValues < 0)
    if (sum(NegRows) == 0)
      NegativeValues <- NA
    else {
      NegativeValues <- data[NegRows, ]
      if (substr(folder,1,1) != " ")  write.csv(NegativeValues, paste(folder, ds.NegValues,
                                                                      sep = ""))
    }
    if (GroupVar == " ") {
      Nvalues <- rep(0, length(AnalyticVars))
      for (i in 1:length(AnalyticVars)) Nvalues[i] <- sum(!is.na(data[,
                                                                      AnalyticVars[i]]))
    }
    if (Groups[1] == "All") {
      groups <- as.character(unique(data[, GroupVar]))
      n.groups <- length(groups)
      nvalues <- matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n.groups) {
        data.groupi <- data[data[, GroupVar] == groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars)) nvalues[i, j] <- sum(!is.na(data.groupi[,
                                                                                  AnalyticVars[j]]))
      }
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(groups, "Total"), nvalues)
    }
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      n.groups <- length(Groups)
      nvalues <- matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars))
      for (i in 1:n.groups) {
        data.groupi <- data[data[, GroupVar] == Groups[i],
                            AnalyticVars]
        for (j in 1:length(AnalyticVars)) nvalues[i, j] <- sum(!is.na(data.groupi[,
                                                                                  AnalyticVars[j]]))
      }
      nvalues[nrow(nvalues), ] <- apply(nvalues[-nrow(nvalues),
                                                ], 2, sum)
      colnames(nvalues) <- AnalyticVars
      Nvalues <- data.frame(Group = c(Groups, "Total"), nvalues)
    }
    if (substr(folder,1,1) != " ")  write.csv(Nvalues, paste(folder, ds.Nsamples, sep = ""))
    if (GroupVar == " ") {
      summary <- matrix(NA, nrow = length(AnalyticVars), ncol = 7)
      for (j in 1:length(AnalyticVars)) {
        summaryj <- summary(data[, AnalyticVars[j]])
        summary[j, 1:length(summaryj)] <- summaryj
      }
      summary[is.na(summary[, 7]), 7] <- 0
      colnames(summary) <- c("min", "Q1", "median", "mean",
                             "Q3", "max", "n.missing")
      Summary <- data.frame(Analysis = AnalyticVars, summary)
    }
    else {
      if (Groups[1] == "All")
        groups <- as.character(unique(data[, GroupVar]))
      if ((Groups[1] != " ") & (Groups[1] != "All"))
        groups <- Groups
      n.groups <- length(groups)
      n.vars <- length(AnalyticVars)
      summary.values <- matrix(NA, nrow = (n.groups + 1) *
                                 n.vars, ncol = 7)
      row <- 0
      vector.groups <- rep(c(groups, " "), n.vars)
      vector.values <- rep(" ", (n.groups + 1) * n.vars)
      for (i in 1:n.vars) {
        vector.values[(row + 1):(row + n.groups)] <- AnalyticVars[i]
        for (j in 1:n.groups) {
          data.valuesij <- data[data[, GroupVar] == groups[j],
                                AnalyticVars[i]]
          summaryij <- summary(data.valuesij)
          summary.values[row + j, 1:length(summaryij)] <- summaryij
        }
        row <- row + n.groups + 1
      }
      summary.values <- round(summary.values, dig = 0)
      colnames(summary.values) <- c("min", "Q1", "median",
                                    "mean", "Q3", "max", "n.missing")
      summary.values[is.na(summary.values[, 7]), 7] <- 0
      Summary <- data.frame(Analysis = vector.values, Group = vector.groups,
                            summary.values)
    }
    if (substr(folder,1,1) != " ")  write.csv(Summary, paste(folder, ds.summary, sep = ""))
    #
    fcn.date.ver<-c(doc,date(),R.Version)
    params<-list(CheckDupVars,GroupVar,Groups)
    names(params)<-c("CheckDupVars","GroupVar","Groups")
    if (folder != " ")  {
      files<-list(folder, ds.duplicates, ds.NegValues, ds.Nsamples, ds.summary)
      names(files)<-c("folder","ds.duplicates","ds.NegValues","ds.Nsamples","ds.summary")
    }
    if (folder == " ") out<-list(usage=fcn.date.ver,
                                 dataUsed=data,
                                 params=params,
                                 analyticVars=AnalyticVars,
                                 Duplicates = duplicates,
                                 NegativeValues = NegativeValues,
                                 Nvalues = Nvalues,
                                 Summary = Summary)
    if (folder != " ") out<-list(usage?=fcn.date.ver,
                                 dataUsed=data,params=params,
                                 analyticVars=AnalyticVars,
                                 Duplicates = duplicates,
                                 NegativeValues = NegativeValues,
                                 Nvalues = Nvalues,
                                 Summary = Summary,
                                 files=files)
    out
  }
