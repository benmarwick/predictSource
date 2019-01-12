#' fn.BoxPlots
#'
#' Box plots of specified analytic values, by specified groups.
#'
#' @param doc: string documenting use, included in list returned
#' @param data: data frame or matrix with data to be analyzed
#' @param GroupVar: name for variable defining grouping (required)
#' @param Groups: vector of values of group variable for which plots are to be done; if 'All', use all groups:
#'                if ' ', no grouping
#' @param AnalyticVars: vector of names (character values) of analytic results to be analyzed
#' @param Nrow: number of rows of plots for each page
#' @param Ncol: number of columns of plots for each page
#'
#' @return A list with the following components:
#'  \itemize{
#' \item{usage:}{  A vector with the value of the argument doc, date run, version of R used}
#' \item{dataUsed:}{  A data frame with data restricted to data used for the box plots}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params.numeric:}{  A vector with the values of the arguments Nrow and Ncol}
#' \item{params.grouping:}{  A vector with the values of the arguments GroupVar and Groups}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'}
#'
#' @section  DETAILS:
#' If the function creates more than one plot, the code stops after each is displayed; enter c
#' (continue) at the prompt to display the next plot.  If Groups specifies only one group, the code
#' for that group is not displayed under a plot.  If an observation has a variable value that is
#'  missing, the variables without missing values are used in the corresponding box plots.
#'
#' @examples
#' data(ObsidianSources)
#'
#'
#' # side-by-side box plots of each source and artifacts assigned to that source
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' ObsidianSources<-ObsidianSources[,c("Code",analyticVars)]
#' Artifacts <- ObsidianArtifacts[,c("Code",analyticVars)]
#' SourcesCode <- as.vector(ObsidianSources[,"Code"], mode="character")
#' ArtifactsCode <- as.vector(paste(Artifacts[,"Code"],"A",sep="."),mode="character")
#' Codes <- c(SourcesCode, ArtifactsCode)
#' SourcesArtifacts <- data.frame(rbind(ObsidianSources,Artifacts)[,analyticVars], Code = Codes)
#' boxPlots<-fn.BoxPlots(data=SourcesArtifacts, GroupVar="Code", Groups="All",AnalyticVars=c("Rb","Nb"),Nrow=2,Ncol=1)
#'
#' @export
#'
fn.BoxPlots <-
  function(doc = "fn.BoxPlots",
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
    #
    sortGroup<-order(data.BP[,GroupVar])
    data.BP <- data.BP[sortGroup,]
    #
    dataKeep <- rep(T, nrow(data.BP)) # will contain indices for observations with
    # no missing values, used only in list returned
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(data.BP[,AnalyticVars[i]])] <- F
    #
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
        if (n.plots.remaining > 0) browser()
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
    if (sum(dataKeep) < nrow(data.BP)) dataNA <- data.BP[!dataKeep,]
    else dataNA <- NA
    #
   list(usage=fcn.date.ver,
              dataUsed=data.BP,
              dataNA = dataNA,
              params.numeric=params.numeric,
              params.grouping=params.grouping,
              analyticVars=AnalyticVars)
    }
