#'   fn.PairsPlot
#'
#'       pairs plots of specified analytic values, by specified groups
#'
#' @param data: R matrix or data frame containing the data to be analyzedfn
#' @param GroupVar: name for variable defining grouping, " " if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done
#'    "All": use all groups
#'    " ": no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Span: value >0, <1 to define proportion of data used to estimate robust smooth
#'
#' @export
#'

fn.PairsPlot <-
  function(data,
           GroupVar,
           Groups,
           AnalyticVars,
           Span = 2 / 3) {
    #

    if ((Groups[1] != " ") &
        (Groups[1] != "All")) {
      # restrict to desired set of groups
      Plot.rows <- (data[, GroupVar] %in% Groups)
      data.Plot <- data[Plot.rows, c(GroupVar, AnalyticVars)]
    }
    else
      data.Plot <- data
    #
    if (Groups[1] == " ")
      # use all data
      pairs(data.Plot[, AnalyticVars], panel = panel.smooth, span = Span)
    #
    else {
      if (Groups[1] == "All")
        groups <- unique(data.Plot[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      #
      for (i in 1:length(groups)) {
        rows.temp <-
          (data.Plot[, GroupVar] %in% groups[i])  # rows from ith group
        data.temp <- data.Plot[rows.temp, AnalyticVars]
        win.graph() # open new graph window
        pairs(
          data.temp,
          panel = panel.smooth,
          span = Span,
          main = paste("group", groups[i])
        )
        browser()  # pause to allow saving plot
      }
    }
  }
