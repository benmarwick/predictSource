#' fn.CV.corr
#'
#' Compute coefficients of variation and correlations for specified
#' analytic values, by specified groups, and plots to visualize correlations.
#' Observations with missing values are removed from computations using them.
#'
#' @param doc A string with documentation, default is the function name
#' @param data AR matrix or data frame containing the data to be analyzedfn
#' @param GroupVar The name for the variable defining grouping; if " ", no grouping
#' @param Groups Character valued defining the the groups used.  Options are a Vector of values
#'  of the group variable; "All" (the default; use all groups); " ", no grouping
#' @param ID The name of a variable with a lab ID (used for sorting data), default is " "
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param Transpose See Details
#' @param CV.digits The number of significant digits in CV estimates, default is 2
#' @param corr.digits The number of significant digits in correlation estimates, default is 2
#' @param plotCorrs Logical, if T (the default), create a matrix of plots describing correlations
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section Details:
#'   If Transpose=T, the correlation matrix has rows defined by the group variable and
#'    columns defined by the pairs of analytic variables.  If Transpose=F,
#'     the rows are defined by pairs of analytic variables and the columns are defined by the groups.
#
#' @return
#'
#'   A list with the following components:
#'   \itemize{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{  A list containing the values of the grouping,logical, and numeric parameters}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{CV:}{  A data frame with the coefficients of variation for each analytic variable in each group}
#'   \item{corr:}{  A data frame with the correlations between pairs of variables in each group}
#'   \item{location:}{  The value of the parameter folder}
#'       }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' CV.corr<-fn.CV.corr(data = ObsidianSources, GroupVar="Code", Groups = "All",
#'  AnalyticVars=analyticVars)
#'
#' @import  corrplot
#'
#' @export

fn.CV.corr <-
  function(doc = "fn.CV.corr",
           data,
           GroupVar,
           Groups="All",
           ID = " ",
           AnalyticVars,
           Transpose = T,
           CV.digits = 2,
           corr.digits = 2,
           plotCorrs = T,
           folder = " ")
    {
    library(corrplot)
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, ]
      sources <- Groups
      }
    else  {data.Used <- data
          sources <- unique(data[,GroupVar])
    }
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
     # no grouping
    #
    if (Groups[1] == "All") {
      #
      #  coefficient of variation
      #
      CV <- rep(NA, length(AnalyticVars))
      means <- apply(data.Used[, AnalyticVars], 2, mean, na.rm=TRUE)
      std <- sqrt(apply(data.Used[,AnalyticVars], 2, var, na.rm=TRUE))
      CV <- round(std / means, dig = CV.digits)
      names(CV) <- AnalyticVars
      #
      #  Spearman correlations
      #
      Corrs <-
        round(cor(
          x = data.Used[,AnalyticVars],
          method = "spearman",
          use = "pairwise.complete.obs"
        ),
        dig = corr.digits)
      if (plotCorrs)
        corrplot(cor(data.Used[,AnalyticVars]),type="upper",method="ellipse",
                 title="All groups")
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
        #  coefficients of variation, with NA observations removed
        #
        means.i <- apply(data.i[,AnalyticVars], 2, mean, na.rm = TRUE)
        std.i <- sqrt(apply(data.i[,AnalyticVars], 2, var, na.rm = TRUE))
        compute.CV[i,] <- round(std.i / means.i, dig = CV.digits)
        CV <- data.frame(groups, compute.CV)
        colnames(CV) <- c(GroupVar, AnalyticVars)
        #
        # Spearman correlations, with NA observations removed
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
      #
      for (i in 1:length(AnalyticVars))
        DataEls <- data.frame(DataEls, data.Used[, AnalyticVars[i]])
      colnames(DataEls) <- c("Code", AnalyticVars)
      # compute correlations and store in Corrs
      for (j in 1:length(groups)) {
        SourceData <- DataEls[DataEls[, GroupVar] == groups[j],]
        SourceCorr <-
          round(cor(
            x = SourceData[,AnalyticVars],
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
      if (plotCorrs) {
        for (i in 1:length(sources)) {
          rows.i <-
            (data.Used[, GroupVar] %in% groups[i])  # rows from group i
          data.i <-
            data.Used[rows.i, AnalyticVars]  # data restricted to group i
          corrplot(cor(data.i[,AnalyticVars]),type="upper",method="ellipse",
                   title=groups[i])
          browser()
        } # end of loop on i
      }  # end of code for plotting correlations
      if (plotCorrs) {
        for (i in 1:length(groups)) {
          rows.i <-
            (data.Used[, GroupVar] %in% groups[i])  # rows from group i
          data.i <-
            data.Used[rows.i, AnalyticVars]  # data restricted to group i
          corrplot(cor(data.i[,AnalyticVars]),type="upper",method="ellipse",
                 title=paste("group", sources[i]))
        } # end of loop on i
      }  # end of code to plot correlations
    } # end of code for computation by group
#
    if (Transpose == T)
      Corrs <- t(Corrs)
    #
    fcn.date.ver<-c(doc,date(),R.Version()$version.string)
    #
    params.numeric<-c(digits.CV=CV.digits,digits.corr=corr.digits)
    names(params.numeric)<-c("CV.digits","corr.digits")
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(Transpose,plotCorrs)
    names(params.logical)<-c("Transpose","plotCorrs")
    params<-list(grouping=params.grouping,logical=params.logical,numeric=params.numeric)
    #
    dataKeep <- rep(T, nrow(data.Used)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(data.Used[,AnalyticVars[i]])] <- F
    #
    if (sum(dataKeep) < nrow(data.Used)) dataNA <- data.Used[!dataKeep,]
      else dataNA <- NA
    #
    list(usage=fcn.date.ver,
                dataUsed=data.Used,
                dataNA = dataNA,
                params=params,
                analyticVars=AnalyticVars,
                CV=CV,
                corr=Corrs,
                location=folder)
   }
