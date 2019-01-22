#'   fn.PairsPlot
#'
#' Pairs plots of specified analytic values, by specified groups
#'
#' @param doc: a string documenting use added to the output list, default is the function name
#' @param data: R matrix or data frame containing the data to be analyzed
#' @param GroupVar: name for variable defining grouping (required)
#' @param Groups: vector of values of group variable for which plots are to be done.
#'    if "All": use all groups; if " ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Span: value >0, <=1 defining the proportion of data used to estimate the lowess smooth
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
#' \item{params.numeric:}{  The value of the argument Span}
#' \item{params.grouping:}{  A vector with the values of the arguments GroupVar and Groups}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#' }
#'
#' @examples
#'
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' pairsPlot <- fn.PairsPlot(data=ObsidianSources, GroupVar="Code", Groups="All",
#' AnalyticVars=analyticVars)
#'
#' @export

fn.PairsPlot <-
  function(doc = "fn.PairsPlot",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Span = 2 / 3) {
    #
    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Plot.rows <- (data[, GroupVar] %in% Groups)
      data.Plot <- data[Plot.rows, c(GroupVar, AnalyticVars)]
    }
    else data.Plot <- data
    #
    sortOnGroup <- order(data.Plot[,GroupVar])
    data.Plot <- data.Plot[sortOnGroup,]
    #
    dataKeep <- rep(T, nrow(data.Plot)) # will contain indices for observations with
    # no missing values
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(data.Plot[,AnalyticVars[i]])] <- F
    #
    if (Groups[1] == " ")
      pairs(data.Plot[, AnalyticVars], panel = panel.smooth,
            span = Span)
    else {
      if (Groups[1] == "All")
        groups <- unique(data.Plot[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      for (i in 1:length(groups)) {
        rows.temp <- (data.Plot[, GroupVar] %in% groups[i])
        data.temp <- data.Plot[rows.temp, AnalyticVars]
        pairs(data.temp, panel = panel.smooth, span = Span,
              main = paste("group", groups[i]))
        browser()
       }
    }
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.numeric<-Span
    names(params.numeric)<-"Span"
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    if (sum(dataKeep) < nrow(data.Plot)) dataNA <- data.Plot[!dataKeep,]
    else dataNA <- NA
    #
    list(usage=fcn.date.ver,
              dataUsed=data,
              dataNA = dataNA,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              analyticVars=AnalyticVars)
    }
