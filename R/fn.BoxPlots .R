#' fn.BoxPlots
#'
#' box plots of specified analytic values, by specified groups
#'
#' @param doc: string documenting use, included in list returned
#' @param data: data frame or matrix with data to be analyzed
#' @param GroupVar: name for variable defining grouping, ' ' if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done; if 'All', use all groups:
#'                if ' ', no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results to be analyzed
#' @param Nrow: number of rows of plots for each page
#' @param Ncol: number of columns of plots for each page restrict to desired set of groups
#'
#' @export A list with the following components:
#'         fcn.date.ver: a vector with the value of the argument doc, date run, version of R used
#'         dataUsed: a data frame with the observations in data restricted to the groups analyzed
#'         analyticVars: the vector specified by the parameter AnalyticVars
#'         params.numeric: a vector with the values of the arguments Nrow and Ncol
#'         params.grouping: a vector with the values of the arguments GroupVar and Groups
#'         analyticVars: a vector with the value of the argument AnalyticVars
#'
fn.BoxPlots <-
  function(doc = "fn.BoxPlots version 0.1",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Nrow,
           Ncol) {

    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      BP.rows <- (data[, GroupVar] %in% Groups)
      data.BP <- data[BP.rows, c(GroupVar, AnalyticVars)]
    }
    else data.BP <- data
    if (Groups[1] != " ") {
      par(mfrow = c(Nrow, Ncol))
      plots.per.page <- Nrow * Ncol
      plot.index <- 0
      n.plots <- length(AnalyticVars)
      n.plots.remaining <- n.plots
      for (i in 1:(round(n.plots/plots.per.page) + 1)) {
        for (j in 1:min(plots.per.page, n.plots.remaining)) {
          if (n.plots.remaining > 0) {
            analysis.no <- plot.index + j
            boxplot(split(data.BP[, AnalyticVars[analysis.no]],
                          as.vector(data.BP[, GroupVar])), ylim = c(0,
                          max(data.BP[, AnalyticVars[analysis.no]],
                          na.rm = T)), notch = T, main = paste("Analysis of ",
                          AnalyticVars[analysis.no], sep = ""), sub = "Code for group")
          }
        }
        plot.index <- plot.index + plots.per.page
        n.plots.remaining <- n.plots.remaining - plots.per.page
        browser()
      }
    }
    if (Groups[1] == " ") {
      boxplot(data.BP[, AnalyticVars],
              ylim = c(0, max(data.BP[,AnalyticVars], na.rm = T)), notch = T, sub = "Analysis")
    }
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.numeric<-c(Nrow,Ncol)
    names(params.numeric)<-c("Nrow","Ncol")
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    out<-list(fcn.date.ver=fcn.date.ver,
              dataUsed=data.BP,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              analyticVars=AnalyticVars)
    out
  }
