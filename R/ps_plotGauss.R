#'
#' ps_plotGauss
#'
#' Function to plot and check a specified pair of analysis variables for bivariate normality.
#'
#' @param data  A matrix or data frame containing the data to be analyzed
#' @param ps_groupVar  The name for the variable defining grouping, " " if no grouping
#' @param analysisVars A vector with the name of all analysis variables.
#' @param ps_analyticVars  A vector of length two with the names of two analytic variables
#'  to be shown in the plots
#' @param ps_scatterPlot  Logical (default is TRUE): specify whether to show scatter plots when ps_qqPlot = FALSE
#' @param ps_qqPlot  Logical (default is TRUE): specify whether to show the q-q plots with the bootstrap
#' envelopes and multivariate plots
#' @param ps_identify  Logical(default is FALSE): if TRUE, user can identify points of interest in the plots
#'
#'#' @return   A data frame if ps_identify = FALSE) or list with the following components:
#'  \itemize{
#' \item{pvalues}{ A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{dataCheck}{ If ps_identify = TRUE, a data frame with the information on user-identified points of interest}
#' }
#'
ps_plotGauss <- function(
  data,
  ps_groupVar,
  analysisVars,
  ps_analyticVars,
  ps_scatterplot,
  ps_qqPlot,
  ps_identify
)
  {
  data1 <- data[, ps_analyticVars[1]]
  data1 <- data1[!is.na(data1)]
  if (ps_qqPlot | ps_identify) {
    qqtest(data = data1, dist = "normal", drawPercentiles = TRUE,
           main = paste(ps_analyticVars[1],"source", data[1,ps_groupVar]))
    qqnorm_pts<-qqnorm(data1, main = paste(ps_analyticVars[1],"source", data[1,ps_groupVar]))
    qqline(data1)
  }
    else if (ps_scatterPlot) {  # TRUE
    qqnorm_pts<-qqnorm(data1, main = paste(ps_analyticVars[1],"source", data[1,ps_groupVar]))
    qqline(data1)
    }
  if (ps_identify) {
    index<-ps_identify(qqnorm_pts)
#    data_grp<-data[data[,ps_groupVar]==groups[i_group],]
#
    dataCheck<<-rbind(dataCheck,data[index,])
  }
  data2 <- data[, ps_analyticVars[2]]
  data2 <- data2[!is.na(data2)]
  if (ps_qqPlot | ps_identify) {
    qqtest(data = data2, dist = "normal", drawPercentiles = T,
           main = paste(ps_analyticVars[2],"source", data[1,ps_groupVar]))
    qqnorm_pts<-qqnorm(data2, main = paste(ps_analyticVars[2],"source", data[1,ps_groupVar]))
    qqline(data2)
  }
  else if (scatterPlot) {
    qqnorm_pts<-qqnorm(data2, main = paste(ps_analyticVars[2],"source", data[1,ps_groupVar]))
    qqline(data2)
  }
  if (ps_identify) {
    index<-ps_identify(qqnorm_pts)
#    data_grp<-data[data[,ps_groupVar]==groups[i_group],]
    dataCheck<<-rbind(dataCheck,data[index,])
  }
  ADp1 <- ad.test(data1)$p.value
  ADp2 <- ad.test(data2)$p.value
  SWp1 <- shapiro.test(data1)$p.value
  SWp2 <- shapiro.test(data2)$p.value
  data <- data[!is.na(data[,ps_analyticVars[1]]) & !is.na(data[,ps_analyticVars[2]]),]
  n_samples <- nrow(data)
  mardia <- MVN::mvn(data = data, mvnTest="mardia")
  p_mardia_skew <- as.numeric(as.character(mardia[[1]][[3]][[1]],mode="character"))
  p_mardia_kurtosis <- as.numeric(as.character(mardia[[1]][[3]][[2]],mode="character"))
  if (ps_qqPlot)
    HZ <- MVN::mvn(data=data, mvnTest="hz",multivariatePlot="qq")
  else  HZ <- MVN::mvn(data=data, mvnTest="hz")
  #
  p_HZ <- as.numeric(HZ[[1]][[3]], mode = "numeric")
  royston <- MVN::mvn(data=data, mvnTest="royston")
  p_Royston <- as.numeric(royston[[1]][[3]], mode = "numeric")
  pvalues <- c(n_samples, ADp1, ADp2, SWp1, SWp2, p_mardia_skew, p_mardia_kurtosis, p_HZ, p_Royston)
  # return p-values and identified points if specified
  if (!ps_identify) dataCheck <- NA
  list(pvalues=pvalues, dataCheck=dataCheck)
}