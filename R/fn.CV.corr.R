#' fn.CV.corr
#'
#' Compute coefficients of variation and correlations for specified
#' analytic values, by specified groups.  Observations with missing values
#' are removed from computations using them.
#'
#' @param data: R matrix or data frame containing the data to be analyzedfn
#' @param GroupVar: name for variable defining grouping; if " ", if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done. if "All" (the default),
#'  use all groups, if " ", no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Transpose: see Details
#' @param CV.digits: number of significant digits in CV estimates, default is 2
#' @param corr.digits: number of significant digits in correlation estimates, default is 2
#' @param folder: location to store excel files with coefficients of variation and Spearman correlations
#'  if " " (the default), no excel files are created
#' @param ds.CV: file name for coefficients of variation, with extension .csv
#' @param ds.corr: file name for Spearman correlation coefficients, with extension .csv
#'
#' @section Details:
#'   If Transpose=T, the correlation matrix has rows defined by the group variable and columns defined by the pairs of analytic variables.  If Transpose=F, the rows are defined by pairs of analytic variables and the columns are defined by the groups.
#
#' @return
#'
#'   A list with the following components:
#'   \itemize{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params.numeric:}{  A vector with the values of the arguments CV.digits and corr.digits}
#'   \item{params.grouping:}{  A list with the values of the argument GroupVar and Groups}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{CV:}{  A data frame with the coefficients of variation for each analytic variable in each group}
#'   \item{corr:}{  A data frame with the correlations between pairs of variables in each group}

#'   \item{files:}{  If folder != " ": a list with path and data set names to the excel files containing
#'       the coefficients of variations and the correlations}
#'       }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar="Code", Groups = "All", AnalyticVars=analyticVars)
#'
#' @export

fn.CV.corr <-
  function(doc = "fn.CV.corr",
           data,
           GroupVar,
           Groups="All",
           AnalyticVars,
           Transpose = T,
           CV.digits = 2,
           corr.digits = 2,
           folder = " ",
           ds.CV,
           ds.corr) {

    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
      }
    else if (GroupVar[1] == " ")
      data.Used <- data[, AnalyticVars]
    else
      data.Used <- data[, c(GroupVar, AnalyticVars)] # includes observations with missing values
    #
    # sort data by GroupVar
    #
    sortByGroup <- order(data.Used[,GroupVar])
    data.Used <- data.Used[sortByGroup,]
    #
    dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with
        # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(data.Used[,AnalyticVars[i]])] <- F
    #
    # no grouping
    #
    if (GroupVar[1] == " ") {
      #
      #  coefficient of variation
      #
      CV <- rep(NA, length(AnalyticVars))
      means <- apply(data.Used, 2, mean, na.rm=TRUE)
      std <- sqrt(apply(data.Used, 2, var, na.rm=TRUE))
      CV <- round(std / means, dig = CV.digits)
      names(CV) <- AnalyticVars
      #
      #  Spearman correlations
      #
      Corrs <-
        round(cor(
          x = data.Used,
          method = "spearman",
          use = "pairwise.complete.obs"
        ),
        dig = corr.digits)
    } # end of code for no grouping
    #
    #  grouping
    #
    else {
      if (Groups[1] == "All")
        groups <- unique(data.Used[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      #
      compute.CV <-
        matrix(NA,
               nrow = length(groups),
               ncol = length(AnalyticVars))  # matrix to store values of CV for each group
      for (i in 1:length(groups)) {
        rows.i <-
          (data.Used[, GroupVar] %in% groups[i])  # rows from group i
        data.i <-
          data.Used[rows.i, AnalyticVars]  # data restricted to group i
        #
        #  indices with missing data for at least one variable
        #
        dataKeep.i <-
        #
        #  coefficients of variation, with NA observations removed
        #
        means.i <- apply(data.i, 2, mean, na.rm = TRUE)
        std.i <- sqrt(apply(data.i, 2, var, na.rm = TRUE))
        compute.CV[i,] <- round(std.i / means.i, dig = CV.digits)
        CV <- data.frame(groups, compute.CV)
        colnames(CV) <- c(GroupVar, AnalyticVars)
        #
        # Spearman correlations
        #
      Corrs <-
        matrix(NA,
               nrow = length(AnalyticVars) * (length(AnalyticVars) - 1) / 2,
               ncol = length(groups))
      colnames(Corrs) <- groups
      Rows <-
        paste(AnalyticVars[1], "/", AnalyticVars[2:length(AnalyticVars)], sep = "")
      for (i in 2:(length(AnalyticVars) - 1))
        Rows <- c(Rows, paste(AnalyticVars[i],
                              "/", AnalyticVars[(i + 1):length(AnalyticVars)], sep = ""))
      rownames(Corrs) <- Rows
      # data frame with Code and desired elements
      DataEls <- data.Used[, "Code"]
      for (i in 1:length(AnalyticVars))
        DataEls <- data.frame(DataEls, data[, AnalyticVars[i]])
      colnames(DataEls) <- c("Code", AnalyticVars)
      # compute correlations and store in Corrs
      for (j in 1:length(groups)) {
        SourceData <- DataEls[DataEls[, GroupVar] == groups[j],]
        SourceCorr <-
          round(cor(
            x = SourceData[, -1],
            method = "spearman",
            use = "pairwise.complete.obs"
          ),
          dig = corr.digits)
        # load correlations into Corrs
        Row <- 0  # row in which to load correlations
        for (k in 1:(length(AnalyticVars) - 1)) {
          Corrs[(Row + 1):(Row + length(AnalyticVars) - k), j] <-
            SourceCorr[k,
                       (k + 1):length(AnalyticVars)]
          Row <- Row + length(AnalyticVars) - k
        }  # end of loop on k
      } # end of loop on j
      }  # end of loop on i
    }
#
    if (Transpose == T)
      Corrs <- t(Corrs)
    if (folder != " ") {
      write.csv(Corrs, file = paste(folder, ds.corr, sep = ""))
      write.csv(CV, paste(folder, ds.CV, sep = ""))
      files <- list(Cv = paste(folder, ds.CV, sep = ""), Corrs = paste(folder, ds.corr, sep = ""))
    }
    #
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    params.numeric<-c(digits.CV=CV.digits,digits.corr=corr.digits)
    names(params.numeric)<-c("CV.digits","corr.digits")
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep]
      else dataNA <- NA
    #
    if (substr(folder,1,1) == " ")
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA = dataNA,
                params.numeric=params.numeric,
                params.grouping=params.grouping,
                analyticVars=AnalyticVars,
                CV=CV,
                corr=Corrs)
    else
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA = dataNA,
                params.numeric=params.numeric,
                params.grouping=params.grouping,
                analyticVars=AnalyticVars,
                CV=CV,
                corr=Corrs,
                files=files)
    out
  }
