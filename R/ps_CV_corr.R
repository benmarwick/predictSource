#' ps_CV_corr
#'
#' Compute coefficients of variation and correlations for specified
#' analytic values, by specified groups, and plots to visualize correlations.
#' Observations with missing values are removed from computations using them.
#'
#' @param doc A string with documentation, default is the function name
#' @param data AR matrix or data frame containing the data to be analyzedfn
#' @param GroupVar The name for the variable defining grouping; if " ", no grouping
#' @param Groups Character valued defining the the groups used_  Options are a Vector of values
#'  of the group variable; "All" (the default; use all groups); " ", no grouping
#' @param ByGroup  If TRUE (the default), results are returned for each group in Groups;
#' if FALSE, groups are combined
#' @param ID The name of a variable with a lab ID (used for sorting data), default is " "
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param Transpose See Details
#' @param CV_digits The number of significant digits in CV estimates, default is 2
#' @param corr_digits The number of significant digits in correlation estimates, default is 2
#' @param plotCorrs Logical, if TRUE (the default), create a matrix of plots describing correlations
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @section Details:
#'   If Transpose=TRUE, the correlation matrix has rows defined by the group variable and
#'    columns defined by the pairs of analytic variables_  If Transpose=FALSE,
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
#' #  All sources combined
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' CV_corr<-ps_CV_corr(data = ObsidianSources, GroupVar="Code", Groups = "All",
#'  AnalyticVars=analyticVars, ByGroup=FALSE)
#'
#' #  By source, restricted to two sources
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' CV_corr<-ps_CV_corr(data = ObsidianSources, GroupVar="Code", Groups = c("A","B"),
#'  AnalyticVars=analyticVars, ByGroup=TRUE)
#'
#' @import  corrplot stats
#'
#' @export

ps_CV_corr <-
  function(doc = "ps_CV_corr",
           data,
           GroupVar,
           Groups="All",
           ByGroup=TRUE,
           ID = " ",
           AnalyticVars,
           Transpose = TRUE,
           CV_digits = 2,
           corr_digits = 2,
           plotCorrs = TRUE,
           folder = " ")
    {
    #
    #  check for valid parameters
    #
    assert_that(is.data.frame(data), msg="parameter data not a data.frame")
    assert_that(is.character(GroupVar), msg="paramter GroupVar not character")
    assert_that(is.character(Groups), msg="parameter Groups not character")
    assert_that(is.logical(ByGroup), msg="parameter ByGroup not logical")
    assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
                msg="parameter AnalyticVars not a character vector")
    assert_that(is.character(ID), msg="ID not a character name")
    assert_that(is.logical(Transpose), msg="parameter Transpose not logical")
    assert_that(is.logical(plotCorrs), msg="parameter plotCorrs not logical")
    assert_that((round(CV_digits,0)==CV_digits)&(CV_digits > 0), msg="CV_digits not a positive integer")
    assert_that((round(corr_digits,0)==corr_digits)&(corr_digits > 0), msg="corr_digits not a positive integer")
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows, ]
      sources <- Groups
      }
    else  {dataUsed <- data
          sources <- unique(data[,GroupVar])
    }
    #
    #  sort on GroupVar and ID if specified
    #
    if (GroupVar[1] != " ") {
      rowsSort <- order(dataUsed[,GroupVar])
      dataUsed <- dataUsed[rowsSort,]
    }
    if (ID[1] != " ") {
      rowsSort <- order(dataUsed[,ID])
      dataUsed <- dataUsed[rowsSort,]
    }
    #
     # no grouping
    #
    if (!ByGroup) {  #  FALSE
      #
      #  coefficient of variation
      #
      CV <- rep(NA, length(AnalyticVars))
      means <- apply(dataUsed[, AnalyticVars], 2, mean, na.rm=TRUE)
      std <- sqrt(apply(dataUsed[,AnalyticVars], 2, var, na.rm=TRUE))
      CV <- round(std / means, digits = CV_digits)
      names(CV) <- AnalyticVars
      #
      #  Spearman correlations
      #
      Corrs <-
        round(cor(
          x = dataUsed[,AnalyticVars],
          method = "spearman",
          use = "pairwise.complete.obs"
        ),
        digits = corr_digits)
      if (plotCorrs) {
        if (Groups[1]=="All")  plotTitle <- "All groups"
        else
          if (length(Groups)==1)  plotTitle <- Groups
        else  {
          plotTitle <- paste("Groups  ",Groups[1], sep="")
          for (iGroup in (2:length(Groups)))
              plotTitle <- paste(plotTitle,"  ",Groups[iGroup],sep="")
        }
        corrplot(cor(dataUsed[,AnalyticVars],method="s"),type="upper",method="ellipse",
                 title=plotTitle)
        } # end of code to plot correlations
    } # end of code for no grouping
    #
    #  grouping
    #
    else {
      # ByGroup = TRUE
      #
      if (Groups[1] == "All")
        groups <- unique(dataUsed[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      #
      compute_CV <-
        matrix(NA,
               nrow = length(groups),
               ncol = length(AnalyticVars))  # matrix to store values of CV for each group
      for (i in 1:length(groups)) {
        rows_i <-
          (dataUsed[, GroupVar] %in% groups[i])  # rows from group i
        data_i <-
          dataUsed[rows_i, AnalyticVars]  # data restricted to group i
        #
        #  coefficients of variation, with NA observations removed
        #
        means_i <- apply(data_i[,AnalyticVars], 2, mean, na.rm = TRUE)
        std_i <- sqrt(apply(data_i[,AnalyticVars], 2, var, na.rm = TRUE))
        compute_CV[i,] <- round(std_i / means_i, digits = CV_digits)
        CV <- data.frame(groups, compute_CV)
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
      DataEls <- dataUsed[, "Code"]
      #
      for (i in 1:length(AnalyticVars))
        DataEls <- data.frame(DataEls, dataUsed[, AnalyticVars[i]])
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
          digits = corr_digits)
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
          rows_i <-
            (dataUsed[, GroupVar] %in% groups[i])  # rows from group i
          data_i <-
            dataUsed[rows_i, AnalyticVars]  # data restricted to group i
          corrplot(cor(data_i[,AnalyticVars],method="s"),type="upper",method="ellipse",
                   title=groups[i])
          browser()
        } # end of loop on i
      }  # end of code for plotting correlations
     } # end of code for computation by group
#
    if (Transpose == T)
      Corrs <- t(Corrs)
    #
    fcnDateVersion<-c(doc,date(),R.Version()$version.string)
    #
    params_numeric<-c(digits_CV=CV_digits,digits_corr=corr_digits)
    names(params_numeric)<-c("CV_digits","corr_digits")
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-c(ByGroup,Transpose,plotCorrs)
    names(params_logical)<-c("ByGroup","Transpose","plotCorrs")
    params<-list(grouping=params_grouping,logical=params_logical,numeric=params_numeric)
    #
    dataKeep <- rep(T, nrow(dataUsed)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(dataUsed[,AnalyticVars[i]])] <- F
    #
    if (sum(dataKeep) < nrow(dataUsed)) dataNA <- dataUsed[!dataKeep,]
      else dataNA <- NA
    #
    list(usage=fcnDateVersion,
                dataUsed=dataUsed,
                dataNA = dataNA,
                params=params,
                analyticVars=AnalyticVars,
                CV=CV,
                corr=Corrs,
                location=folder)
   }
