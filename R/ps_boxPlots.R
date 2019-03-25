#' ps_boxPlots
#'
#' Box plots of specified analytic values, by specified groups_
#'
#' @param doc A string documenting use, included in list returned; default value is the function name
#' @param data A data frame or matrix with data to be analyzed
#' @param GroupVar The name for the variable defining grouping (required)
#' @param Groups A vector of values of group variable for which plots are to be done; if 'All', use all groups:
#'                if ' ', no grouping
#' @param AnalyticVars A vector of names (character values) of analytic results to be analyzed
#' @param Nrow The number of rows of plots for each page
#' @param Ncol The number of columns of plots for each page
#'
#' @return A list with the following components:
#'  \itemize{
#' \item{usage:}{  A string with the value of the argument doc, date run, version of R used}
#' \item{dataUsed:}{  A data frame with data restricted to data used for the box plots}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{params:}{  A list with elements containing the values of the grouping and numeric parameters}
#' \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'}
#'
#' @section  DETAILS:
#' If the function creates more than one plot, the code stops after each is displayed; enter c
#' (continue) at the prompt to display the next plot_  If Groups specifies only one group, the code
#' for that group is not displayed under a plot_  If an observation has a variable value that is
#'  missing, the variables without missing values are used in the corresponding box plots_
#'
#' @examples
#' data(ObsidianSources)
#' boxPlots<-ps_boxPlots(data=ObsidianSources, GroupVar="Code", Groups="All",
#' AnalyticVars=c("Rb","Nb", "Zr","Sr"),Nrow=2,Ncol=2)
#'
#' # side-by-side box plots of each source and artifacts assigned to that source
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' ObsidianSources<-ObsidianSources[,c("Code",analyticVars)]
#' Artifacts <- ObsidianArtifacts[,c("Code",analyticVars)]
#' SourcesCode <- as.vector(ObsidianSources[,"Code"], mode="character")
#' ArtifactsCode <- as.vector(paste(Artifacts[,"Code"],"A",sep="_"),mode="character")
#' Codes <- c(SourcesCode, ArtifactsCode)
#' SourcesArtifacts <- data.frame(rbind(ObsidianSources,Artifacts)[,analyticVars], Code = Codes)
#' boxPlots<-ps_boxPlots(data=SourcesArtifacts, GroupVar="Code", Groups="All",AnalyticVars="Rb",Nrow=1,Ncol=1)
#'
#' @export
#'
ps_boxPlots <-
  function(doc = "ps_boxPlots",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Nrow,
           Ncol) {

    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      BP_rows <- (data[, GroupVar] %in% Groups)
      dataBP <- data[BP_rows, c(GroupVar, AnalyticVars)]
    }
    else dataBP <- data
    #
    sortGroup<-order(dataBP[,GroupVar])
    dataBP <- dataBP[sortGroup,]
    #
    dataKeep <- rep(T, nrow(dataBP)) # will contain indices for observations with
    # no missing values, used only in list returned
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(dataBP[,AnalyticVars[i]])] <- F
    #
    if (Groups[1] != " ") {
      par(mfrow = c(Nrow, Ncol))
      plots_per_page <- Nrow * Ncol
      plot_index <- 0
      n_plots <- length(AnalyticVars)
      n_plots_remaining <- n_plots
      for (i in 1:(round(n_plots/plots_per_page) + 1)) {
        for (j in 1:min(plots_per_page, n_plots_remaining)) {
          if (n_plots_remaining > 0) {
            analysis_no <- plot_index + j
            boxplot(split(dataBP[, AnalyticVars[analysis_no]],
                          as.vector(dataBP[, GroupVar])), ylim = c(0,
                          max(dataBP[, AnalyticVars[analysis_no]],
                          na_rm = T)), notch = T, main = paste("Analysis of ",
                          AnalyticVars[analysis_no], sep = ""), sub = "Code for group")
          }
        }
        plot_index <- plot_index + plots_per_page
        n_plots_remaining <- n_plots_remaining - plots_per_page
        if (n_plots_remaining > 0) browser()
      }
    }
    if (Groups[1] == " ") {
      boxplot(dataBP[, AnalyticVars],
              ylim = c(0, max(dataBP[,AnalyticVars], na_rm = T)), notch = T, sub = "Analysis")
    }
    fcn_date_ver<-paste(doc,date(),R.Version()$version.string)
    #
    params_numeric<-c(Nrow,Ncol)
    names(params_numeric)<-c("Nrow","Ncol")
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params<-list(grouping=params_grouping, numeric=params_numeric)
    #
    if (sum(dataKeep) < nrow(dataBP)) dataNA <- dataBP[!dataKeep,]
    else dataNA <- NA
    #
   list(usage=fcn_date_ver,
              dataUsed=dataBP,
              dataNA = dataNA,
              params=params,
              analyticVars=AnalyticVars)
    }
