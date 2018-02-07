#'  fn.CheckData
#'
#'
#' basic data check: duplicate records, negative analytic values, numbers of analytic results, ranges of results
#'
#'
#' @param data: R object (data frame) containing analytic data
#' @param CheckDupVars: vector with names of identifying variables, typically group and lab ID
#' @param GroupVar: if there are groups, name of variable defining the groups, default value of " ": no grouping
#' @param Groups: character vector of groups by which numbers of samples and summary statistics will be tabulated default value of " ": tabulations are done for the entire data set value = "All": tabulation for each distinct code in GroupVar
#' @param AnalyticVars: character vector of names of analytic variables for which tabulations are done
#' @param folder: path to folder in which to store excel files with results (e.g. "C:\\project\\results\\")
#' @param ds.duplicates: file name for excel file with duplicate records, with extension csv (e.g. "duplicates.csv")
#' @param ds.Nsamples: file name for excel file with numbers of samples, with extension csv (e.g. "Nresults.csv")
#' @param ds.summary: file with summary statistics for each analytic variable check matrix stored in the object data for duplicates based on variables in the argument fields
#' @return
#'   If folder != " ", four excel files with duplicate observations, observations with negative
#'       values for one or more analytic variables, numbers of observations for each analytic
#'       variable, and summary statistics (quartiles and number missing)
#'       if Groups != " ", numbers of observations and summary statistics are by group
#'   A list with the following components:
#'      fcn.date.ver: a string with the contents of the parameter doc, date run, R version used
#'      dataUsed: the data frame specified by the parameter data
#'      params:
#'      analyticVars: the vector of names specified by the parameter AnalyticVars
#'      Duplicates: a data frame containing the observations with duplicate values
#'      NegativeValues: a data frame containing the observations with at least one negative value
#'          for a variable in AnalyticVars
#'      Nvalues: a data frame contain the number of observations with a value for each analytic variable
#'      Summary: a data frame containing the summary statistics (by group, if Group is specified)
#'      if folder != " ": excel.files, a list containing the folder name and the name of each of
#'         the excel files
#'
#'   USAGE
#' @export


fn.CheckData <-
  function(doc = "fn.CheckData",
           data,
           CheckDupVars,
           GroupVar,
           Groups,
           AnalyticVars,
           folder,
           ds.duplicates,
           ds.NegValues,
           ds.Nsamples,
           ds.summary) {
    #

    #
    if ((Groups[1] != " ") &
        (Groups[1] != "All")) {
      # check only records for specified subset of groups
      UseRows <- (data[, GroupVar] %in% Groups)
      data.subset <- data[UseRows, ]
      DupRows <-
        duplicated(data.subset[, CheckDupVars])  # row numbers with duplicates for variables CheckDupVars
      DupRowNumbers <- (1:nrow(data.subset))[DupRows]
      if (sum(DupRowNumbers) == 0)
        duplicates <- NA   #  no duplicates
      else {
        duplicates <- data[DupRows, ]  # rows with duplicates
        write.csv(duplicates, paste(folder, ds.duplicates, sep = "")) # write excel file with duplicates
      }
    }
    else {
      # check entire matrix
      DupRows <-
        duplicated(data[, CheckDupVars])  # row numbers with duplicates for variables CheckDupVars
      DupRowNumbers <- (1:nrow(data))[DupRows]
      if (sum(DupRowNumbers) == 0)
        duplicates <- NA   #  no duplicates
      else {
        duplicates <- data[DupRows, ]  # rows with duplicates
        write.csv(duplicates, paste(folder, ds.duplicates, sep = "")) # write excel file with duplicates
      }
      browser()
    }
    #
    #  check for negative analytic values
    #
    MinimumValues <-
      apply(data[, AnalyticVars], 1, min, na.rm = T)  # minimum of values in each row, omitting NA values
    NegRows <-
      (MinimumValues < 0)  #  row numbers with at least one negative analytic value
    if (sum(NegRows) == 0)
      NegativeValues <- NA # no samples with at least one negative value
    else  {
      NegativeValues <- data[NegRows, ]
      write.csv(NegativeValues, paste(folder, ds.NegValues, sep = ""))  # write excel file with samples with negative value
    }
    #
    #  number of analytic values, by group and element
    #
    if (GroupVar == " ") {
      # no grouping
      Nvalues <-
        rep(0, length(AnalyticVars))  # vector to store numbers of values
      for (i in 1:length(AnalyticVars))
        Nvalues[i] <- sum(!is.na(data[, AnalyticVars[i]]))
    }
    #
    if (Groups[1] == "All") {
      # all groups
      groups <- as.character(unique(data[, GroupVar]))
      n.groups <- length(groups)
      nvalues <-
        matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars)) # matrix to store numbers of values and total for each variable
      for (i in 1:n.groups) {
        data.groupi <-
          data[data[, GroupVar] == groups[i], AnalyticVars]   # rows in matrix data for ith group
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <-
            sum(!is.na(data.groupi[, AnalyticVars[j]])) # number of values for each element
      }
      nvalues[nrow(nvalues), ] <-
        apply(nvalues[-nrow(nvalues), ], 2, sum)  # total for each element
      colnames(nvalues) <- AnalyticVars
      Nvalues <-
        data.frame(Group = c(groups, "Total"), nvalues)  # data frame with first column identifying the grouping
    }
    if ((Groups[1] != " ") &
        (Groups[1] != "All")) {
      #  number by group for specified subset of groups
      n.groups <- length(Groups)
      nvalues <-
        matrix(0, nrow = n.groups + 1, ncol = length(AnalyticVars)) # matrix to store numbers of values and total for each variable
      for (i in 1:n.groups) {
        data.groupi <-
          data[data[, GroupVar] == Groups[i], AnalyticVars]   # rows in matrix data for ith group
        for (j in 1:length(AnalyticVars))
          nvalues[i, j] <-
            sum(!is.na(data.groupi[, AnalyticVars[j]])) # number of values for each element
      }
      nvalues[nrow(nvalues), ] <-
        apply(nvalues[-nrow(nvalues), ], 2, sum)  # total for each element
      colnames(nvalues) <- AnalyticVars
      Nvalues <-
        data.frame(Group = c(Groups, "Total"), nvalues)  # data frame with first column identifying the grouping
    }
    write.csv(Nvalues, paste(folder, ds.Nsamples, sep = ""))
    #
    # summary statistics for analytic variables
    #
    if (GroupVar == " ") {
      # no grouping
      summary <-
        matrix(NA, nrow = length(AnalyticVars), ncol = 7)  # matrix to store ranges
      for (j in 1:length(AnalyticVars)) {
        summaryj <- summary(data[, AnalyticVars[j]])
        summary[j, 1:length(summaryj)] <- summaryj
      }
      summary[is.na(summary[, 7]), 7] <- 0
      colnames(summary) <-
        c("min", "Q1", "median", "mean", "Q3", "max", "n.missing")
      Summary <-
        data.frame(Analysis = AnalyticVars, summary)  #  data frame with code for analysis in column 1
    }
    #
    else {
      if (Groups[1] == "All")
        groups <- as.character(unique(data[, GroupVar]))  # use all groups
      if ((Groups[1] != " ") &
          (Groups[1] != "All"))
        groups <- Groups  # use specified subset of grops
      n.groups <- length(groups)
      n.vars <- length(AnalyticVars)
      summary.values <- matrix(NA, nrow = (n.groups + 1) * n.vars, ncol = 7)
      row <- 0  # index for first row minus 1 for each group
      vector.groups <- rep(c(groups, " "), n.vars)
      vector.values <- rep(" ", (n.groups + 1) * n.vars)
      for (i in 1:n.vars) {
        vector.values[(row + 1):(row + n.groups)] <- AnalyticVars[i]
        for (j in 1:n.groups) {
          data.valuesij <- data[data[, GroupVar] == groups[j], AnalyticVars[i]]
          summaryij <- summary(data.valuesij)
          summary.values[row + j, 1:length(summaryij)] <- summaryij
        }
        row <- row + n.groups + 1
      }
      summary.values <- round(summary.values, dig = 0)
      colnames(summary.values) <-
        c("min", "Q1", "median", "mean", "Q3", "max", "n.missing")
      summary.values[is.na(summary.values[, 7]), 7] <- 0
      Summary <-
        data.frame(Analysis = vector.values, Group = vector.groups, summary.values)
    }
    #
    write.csv(Summary, paste(folder, ds.summary, sep = ""))
    #
    list(
      Documentation = doc,
      Duplicates = duplicates,
      NegativeValues = NegativeValues,
      Nvalues = Nvalues,
      Summary = Summary
    )
  }
