#'
#' ps_plotGauss
#'
#' Function to plot and check a specified pair of analysis variables for bivariate normality.
#'
#' @param data  A matrix or data frame containing the data to be analyzed
#' @param ps_groupVar  The name for the variable defining grouping, " " if no grouping
#' @param ps_pair  A vector of length two with the names of two analytic variables
#'  to be shown in the plots
#' @param ps_scatterplot  Logical: specify whether to show scatter plots when ps_qqPlot = FALSE
#' @param ps_qqPlot  Logical: specify whether to show the q-q plots with the bootstrap
#' envelopes and multivariate plots
#' @param ps_identify  Logical: if TRUE, user can identify points of interest in the plots
#'
#'#' @return   A data frame if ps_identify = FALSE) or list with the following components:
#'  \itemize{
#' \item{pvalues}{ A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{dataCheck}{ If ps_identify = TRUE, a data frame with the information on user-identified points of interest;
#' otherwise, NA}
#' }
#'
ps_plotGauss <- function(
  data,
  ps_groupVar,
  ps_pair,
  ps_scatterplot,
  ps_qqPlot,
  ps_identify
)
  {
  group <- data[1,ps_groupVar] # name for group analyzed
  if (ps_identify) dataCheck <- data[1,] # dummy row
  #
  data1 <- data[, ps_pair[1]]
  data1 <- data1[!is.na(data1)]
  if (ps_qqPlot | ps_identify) {
    qqtest(data = data1, dist = "normal", drawPercentiles = TRUE,
           main = paste(ps_pair[1],"group", group))
    qqnorm_pts<-qqnorm(data1, main = paste(ps_pair[1],"group", group))
    qqline(data1)
  }
    else if (ps_scatterplot) {  # TRUE
    qqnorm_pts<-qqnorm(data1, main = paste(ps_pair[1],"group", group))
    qqline(data1)
    }
  if (ps_identify) {
    pts_index<-identify(qqnorm_pts)
    dataCheck<-rbind(dataCheck,data[pts_index,])
  }
  data2 <- data[, ps_pair[2]]
  data2 <- data2[!is.na(data2)]
  if (ps_qqPlot | ps_identify) {
    qqtest(data = data2, dist = "normal", drawPercentiles = T,
           main = paste(ps_pair[2],"group", data[1,ps_groupVar]))
    qqnorm_pts<-qqnorm(data2, main = paste(ps_pair[2],"group", group))
    qqline(data2)
  }
  else if (ps_scatterplot) {
    qqnorm_pts<-qqnorm(data2, main = paste(ps_pair[2],"group", group))
    qqline(data2)
  }
  if (ps_identify) {
    pts_index<-identify(qqnorm_pts)
    dataCheck<-rbind(dataCheck,data[pts_index,])
  }
  ADp1 <- ad.test(data1)$p.value
  ADp2 <- ad.test(data2)$p.value
  SWp1 <- shapiro.test(data1)$p.value
  SWp2 <- shapiro.test(data2)$p.value
  data <- data[!is.na(data1) & !is.na(data2),]
  n_samples <- nrow(data)
  mardia <- MVN::mvn(data = data[,ps_pair], mvnTest="mardia")
  p_mardia_skew <- as.numeric(as.character(mardia[[1]][[3]][[1]],mode="character"))
  p_mardia_kurtosis <- as.numeric(as.character(mardia[[1]][[3]][[2]],mode="character"))
  if (ps_qqPlot)
    HZ <- MVN::mvn(data=data[,ps_pair], mvnTest="hz",multivariatePlot="qq")
  else  HZ <- MVN::mvn(data=data[,ps_pair], mvnTest="hz")
  #
  p_HZ <- as.numeric(HZ[[1]][[3]], mode = "numeric")
  royston <- MVN::mvn(data=data[,ps_pair], mvnTest="royston")
  p_Royston <- as.numeric(royston[[1]][[3]], mode = "numeric")
  pvalues <- c(n_samples, ADp1, ADp2, SWp1, SWp2, p_mardia_skew, p_mardia_kurtosis, p_HZ, p_Royston)
  # return p-values and identified points if specified
  if (!ps_identify) dataCheck <- NA
     else  dataCheck <- dataCheck[-1,]  # remove dummy row
  list(pvalues=pvalues, dataCheck=dataCheck)
}