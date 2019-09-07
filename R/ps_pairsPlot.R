#'   ps_pairsPlot
#'
#' Pairs plots of specified analytic values, by specified groups
#'
#' @param doc A string documenting use added to the output list, default is the function name
#' @param data A matrix or data frame containing the data to be analyzed
#' @param GroupVar The name for variable defining grouping (required)
#' @param Groups A vector of values of group variable for which plots are to be done;
#'    if "All": use all groups; if " ": no grouping
#' @param AnalyticVars A vector of names (character values) of analytic results
#' @param Span A value >0, <=1 defining the proportion of data used to estimate the lowess smooth.
#' The default value (2/3) is the default value for the lowess function.
#'
#' @section  DETAILS:
#'  The function produces a pairs plot with a lowess smooth through the scatter plot for
#'   each pair of variables in AnalyticVars.  If Groups != " ", there is a scatter plot for
#'   each group in Groups.  If Groups=" ", there is one pairs plot with the data for all groups.
#'   Executing the function produces warnings ("span is not a graphical parameter") that can be
#'   ignored (changing the value of Span does change the lowess smooths).
#' As coded, in RStudio all plots are produced without a pause; use the back arrow in the plot pane to see
#'  the plots.  In base R, remove the comment symbol (#) from the browser command at the end of the final loop,
#'  so that the function will stop after producing each plot.
#'
#' @return A set of pairs plots as described above and a list with the following components:
#' \itemize{
#' \item{usage:}{  A vector with the value of the argument doc, date run, version of R used}
#' \item{dataUsed:}{  A data frame with the observations in data restricted to the groups analyzed}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{analyticVars:}{  The vector specified by the parameter AnalyticVars}
#' \item{params:}{  A list with the values of the grouping and numeric arguments}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#' }
#'
#' @examples
#'
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' pairsPlot <- ps_pairsPlot(data=ObsidianSources, GroupVar="Code", Groups="All",
#' AnalyticVars=analyticVars)
#'
#' @import graphics
#'
#' @export

ps_pairsPlot <-
  function(doc = "ps_pairsPlot",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Span = 2 / 3) {
    #
    #  check for valid parameters
    #
    assert_that(is.data.frame(data), msg="parameter data not a data.frame")
    assert_that(is.character(GroupVar), msg="paramter GroupVar not character")
    assert_that(is.character(Groups), msg="parameter Groups not character")
    assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
                msg="parameter AnalyticVars not a character vector")
    assert_that(is.numeric(Span) & (Span > 0) & (Span < 1), msg="invalid value of parameter Span")
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Plot_rows <- (data[, GroupVar] %in% Groups)
      dataPlot <- data[Plot_rows, c(GroupVar, AnalyticVars)]
    }
    else dataPlot <- data
    #
    sortOnGroup <- order(dataPlot[,GroupVar])
    dataPlot <- dataPlot[sortOnGroup,]
    #
    dataKeep <- rep(T, nrow(dataPlot)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(dataPlot[,AnalyticVars[i]])] <- F
    #
    if (Groups[1] == " ")
      pairs(dataPlot[, AnalyticVars], panel = panel.smooth,
            span = Span)
    else {
      if (Groups[1] == "All")
        groups <- unique(dataPlot[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      for (i in 1:length(groups)) {
        rows_temp <- (dataPlot[, GroupVar] %in% groups[i])
        data_temp <- dataPlot[rows_temp, AnalyticVars]
        pairs(data_temp, panel = panel.smooth, span = Span,
              main = paste("group", groups[i]))
        browser()
       }
    }
    fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
    #
    params_numeric<-Span
    names(params_numeric)<-"Span"
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params<-list(grouping=params_grouping, numeric=params_numeric)
    #
    if (sum(dataKeep) < nrow(dataPlot)) dataNA <- dataPlot[!dataKeep,]
    else dataNA <- NA
    #
    list(usage=fcnDateVersion,
              dataUsed=data,
              dataNA = dataNA,
              params=params,
              analyticVars=AnalyticVars)
    }
