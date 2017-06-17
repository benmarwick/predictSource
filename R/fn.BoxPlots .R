#' fn.BoxPlots
#'
#' box plots of specified analytic values, by specified groups
#'
#' @param GroupVar: name for variable defining grouping, ' ' if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done 'All': use all groups ' ': no
#' grouping
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Nrow: number of rows of plots for each page
#' @param Ncol: number of columns of plots for each page restrict to desired set of groups
#'
#' @export


fn.BoxPlots <-
  function(data,
           GroupVar,
           Groups,
           AnalyticVars,
           Nrow,
           Ncol) {

    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      BP.rows <- (data[, GroupVar] %in% Groups)
      data.BP <- data[BP.rows, c(GroupVar, AnalyticVars)]
    } else
      data.BP <- data
    if (Groups[1] != " ") {
      # use grouping create box plots
      par(mfrow = c(Nrow, Ncol))  # set up format for plots
      plots.per.page <- Nrow * Ncol
      plot.index <- 0
      n.plots <- length(AnalyticVars)
      n.plots.remaining <-
        n.plots  # if 0, don't create blank page
      for (i in 1:(round(n.plots / plots.per.page) + 1)) {
        for (j in 1:min(plots.per.page, n.plots.remaining)) {
          if (n.plots.remaining > 0) {
            analysis.no <- plot.index + j
            boxplot(
              split(data.BP[, AnalyticVars[analysis.no]], as.vector(data.BP[,
                                                                            GroupVar])),
              ylim = c(0, max(data.BP[, AnalyticVars[analysis.no]],
                              na.rm = T)),
              notch = T,
              main = paste("Analysis of ", AnalyticVars[analysis.no],
                           sep = ""),
              sub = "Code for group"
            )
          }
        }
        plot.index <- plot.index + plots.per.page
        n.plots.remaining <- n.plots.remaining - plots.per.page
        browser()
      }
    }
    if (Groups[1] == " ") {
      # box plots with all analytic values in one plot
      boxplot(data.BP[, AnalyticVars],
              ylim = c(0, max(data.BP[, AnalyticVars], na.rm = T)),
              notch = T,
              sub = "Analysis")
    }
  }
