#'   fn.PairsPlot
#'
#'       pairs plots of specified analytic values, by specified groups
#'
#' @param doc: a string documenting use added to the output list, default is the function name
#' @param data: R matrix or data frame containing the data to be analyzedfn
#' @param GroupVar: name for variable defining grouping, " " if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done.
#'    if "All": use all groups; if " ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Span: value >0, <=1 defining the proportion of data used to estimate the lowess smooth
#'
#' @section  DETAILS:
#' \itemize{
#' \item{"x"}{The function produces a pairs plot with a lowess smooth through the scatter plot for
#'   each pair of variables in AnalyticVars.  If Groups != " ", there is a scatter plot for
#'   each group in Groups.  If Groups=" ", there is one pairs plot with the data for all groups.}
#'
#' \item{"x"}{In Rstudio, each plot is in a separate window, not in the Plot pane.}
#'
#' \item{"x"}{Executing the function produces warnings ("span is not a graphical parameter") that can be
#'   ignored (changing the value of Span does change the lowess smooths).}
#'   }
#'
#' @return A set of pairs plots as described above and a list with the following components:
#' \itemize{
#' \item{"usage"}{a vector with the value of the argument doc, date run, version of R used}
#' \item{"dataUsed"}{a data frame with the observations in data restricted to the groups analyzed}
#' \item{"analyticVars"}{the vector specified by the parameter AnalyticVars}
#' \item{"params.numeric"}{the value of the argument Span}
#' \item{"params.grouping"}{a vector with the values of the arguments GroupVar and Groups}
#' \item{"analyticVars"}{a vector with the value of the argument AnalyticVars}
#' }
#'
#' @export

fn.PairsPlot <-
  function(doc = "fn.PairsPlot version 0.1",
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
        win.graph()
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
    out<-list(usage=fcn.date.ver,
              dataUsed=data,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              analyticVars=AnalyticVars)
    out
  }
