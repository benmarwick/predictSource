#' fn.CV.corr
#'
#' Compute coefficients of variation and correlations for specified
#' analytic values, by specified groups
#'
#' @param data: R matrix or data frame containing the data to be analyzedfn
#' @param GroupVar: name for variable defining grouping; if " ", if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done
#'   if "All", use all groups, if " ", no grouping
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
#'
#'   If Transpose=T, the correlation matrix has rows defined by the group variable and columns
#'   defined by the pairs of analytic variables.  If Transpose=F, the rows are defined by
#'   pairs of analytic variables and the columns are defined by the groups.
#
#' @return
#'
#'   A list with the following components:
#'   \item{usage}{a vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed}{the contents of the argument data restricted to the groups used}
#'   \item{params.numeric}{a vector with the values of the arguments CV.digits and corr.digits}
#'   \item{params.grouping}{a list with the values of the argument GroupVar and Groups}
#'   \item{analyticVars}{a vector with the value of the argument AnalyticVars}
#'   \item{CV}{a data frame with the coefficients of variation for each analytic variable in each group}
#'   \item{corr}{a data frame with the correlations between pairs of variables in each group}
#'   \item{files}{if folder != " ": a list with path and data set names to the excel files containing
#'       the coefficients of variations and the correlations}
#'
#' @examples
#'
#' @export

fn.CV.corr <-
  function(doc = "fn.CV.corr",
           data,
           GroupVar,
           Groups,
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
    } else if (GroupVar[1] == " ")
      data.Used <-
        data[, AnalyticVars]
    else
      data.Used <- data[, c(GroupVar, AnalyticVars)]
    # coefficient of variation no grouping
    if (GroupVar[1] == " ") {
      CV <- rep(NA, length(AnalyticVars))
      means <- apply(data.Used, 2, mean)
      std <- sqrt(apply(data.Used, 2, var))
      CV <- round(std / means, dig = CV.digits)
      names(CV) <- AnalyticVars
    } else {
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
        means.i <- apply(data.i, 2, mean)
        std.i <- sqrt(apply(data.i, 2, var))
        compute.CV[i,] <- round(std.i / means.i, dig = CV.digits)
      }
      CV <- data.frame(groups, compute.CV)
      colnames(CV) <- c(GroupVar, AnalyticVars)
    }
    # Spearman correlations
    if (GroupVar[1] == " ") {
      Corrs <-
        round(cor(
          x = data.Used,
          method = "spearman",
          use = "pairwise.complete.obs"
        ),
        dig = corr.digits)
    } else {
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
      for (i in 1:length(groups)) {
        SourceData <- DataEls[DataEls[, GroupVar] == groups[i],]
        SourceCorr <-
          round(cor(
            x = SourceData[, -1],
            method = "spearman",
            use = "pairwise.complete.obs"
          ),
          dig = corr.digits)
        # load correlations into Corrs
        Row <- 0  # row in which to load correlations
        for (j in 1:(length(AnalyticVars) - 1)) {
          Corrs[(Row + 1):(Row + length(AnalyticVars) - j), i] <-
            SourceCorr[j,
                       (j + 1):length(AnalyticVars)]
          Row <- Row + length(AnalyticVars) - j
        }
      }
    }
    if (Transpose == T)
      Corrs <- t(Corrs)
    if (folder != " ") {
      write.csv(Corrs, file = paste(folder, ds.corr, sep = ""))
      write.csv(CV, paste(folder, ds.CV, sep = ""))
      files <- list(Cv = paste(folder, ds.CV, sep = ""), Corrs = paste(folder, ds.corr, sep = ""))
    }
    #
    fcn.date.ver<-c(doc,date(),R.Version())
    params.numeric<-c(digits.CV=CV.digits,digits.corr=corr.digits)
    names(params.numeric)<-c("CV.digits","corr.digits")
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    if (substr(folder,1,1) == " ")
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                params.numeric=params.numeric,
                params.grouping=params.grouping,
                analyticVars=AnalyticVars,
                CV=CV,
                corr=Corrs)
    else
      out<-list(usage=fcn.date.ver,
                dataUsed=data.Used,
                params.numeric=params.numeric,
                params.grouping=params.grouping,
                analyticVars=AnalyticVars,
                CV=CV,corr=Corrs,
                files=files)
    out
  }
