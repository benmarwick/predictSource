#' fn.BoxPlots
#'
#' Box plots of specified analytic values, by specified groups
#'
#' @param doc: string documenting use, included in list returned
#' @param data: data frame or matrix with data to be analyzed
#' @param GroupVar: name for variable defining grouping, ' ' if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done; if 'All', use all groups:
#'                if ' ', no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results to be analyzed
#' @param Nrow: number of rows of plots for each page
#' @param Ncol: number of columns of plots for each page
#'
#' @return A list with the following components:
#'  \itemize{
#' \item{"usage"}{a vector with the value of the argument doc, date run, version of R used}
#' \item{"analyticVars"}{the vector specified by the parameter AnalyticVars}
#' \item{"params.numeric"}{a vector with the values of the arguments Nrow and Ncol}
#' \item{"params.grouping"}{a vector with the values of the arguments GroupVar and Groups}
#' \item{"analyticVars"}{a vector with the value of the argument AnalyticVars}
#'}
#'
#' \section  DETAILS:
#' Only the last plot is displayed in the plot window when the function is run.  Use the back arrow
#' to display previous plots; click on the zoom button to create a new window with an enlarged version of the plot window.
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' boxPlots<-fn.BoxPlots(data=ObsidianSources, GroupVar="Code", Groups=c("A","B"),AnalyticVars=analyticVars,Nrow=2,Ncol=2)
#'
#' @export

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
    out<-list(usage=fcn.date.ver,
              dataUsed=data.BP,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              analyticVars=AnalyticVars)
    out
  }
