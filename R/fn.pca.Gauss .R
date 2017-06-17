#' fn.PCA.Gauss
#'
#' check whether first two components are Gaussian
#'
#' @param data: R matrix or data
#' frame containing the data to be analyzed
#' @param GroupVar: name for variable defining
#' grouping, ' ' if no grouping
#' @param Groups: vector of values of group variable for which
#' plots are to be done 'All': use all groups grouping is assumed
#' @param AnalyticVars: vector
#' of names (character values) of analytic results
#' @param folder: folder in which excel file
#' is to be stored
#' @param ds.pvalues: excel file with Anderson-Darling and Mardia p-values,
#' extension .csv create dataset data. Used based on grouping
#'
#' @import MASS nortest qqtest MVN
#'
#' @export

fn.pca.Gauss <-
  function(doc = "fn.pca.Gauss",
           data,
           GroupVar = "Code",
           Groups = "All",
           AnalyticVars,
           QQtest = T,
           folder,
           ds.pvalues ) {


    # restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    } else
      data.Used <- data[, c(GroupVar, AnalyticVars)]
    # define variable groups as groups used in analysis
    if (Groups[1] == "All")
      groups <-
        as.character(unique(data.Used[, GroupVar]))
    else
      groups <- as.character(Groups)
    #
    pca <- prcomp(data.Used[, AnalyticVars], scale = TRUE)
    # predicted values for first two components
    predict.pc1 <- predict(pca)[, 1]
    predict.pc2 <- predict(pca)[, 2]
    # add numeric code for group to data set
    GroupIndex <- rep(NA, nrow(data.Used))
    for (i in 1:nrow(data.Used)) {
      for (j in 1:length(groups))
        if (data.Used[i, GroupVar] == groups[j])
          GroupIndex[i] <- j
    }
    #
    pvalues <-
      matrix(NA, length(groups), 6)  # Anderson-Darling, Shapiro-Wilk, Mardia test p-values
    dimnames(pvalues) <-
      list(
        groups,
        c(
          "ADpc.1",
          "ADpc.2",
          "SWpc.1",
          "SWpc.2",
          "Mardia.skew",
          "Mardia.kurtosis"
        )
      )
    #
    Predicted <-
      data.frame(group = as.character(data.Used[, GroupVar]),
                 GroupIndex = GroupIndex,
                 predict(pca))
    #
    n.pages <-
      round((length(groups) + 1) / 2, dig = 0)  # number of pages of plots, 2 groups to a page
    i.group <- 0  # initialize choice for group
    # qq plots and Anderson-Darling p-values
    fn.plot <- function() {
      temp <-
        Predicted[Predicted[, "group"] == groups[i.group], c("PC1", "PC2")]
      temp1 <- temp[, "PC1"]
      qqnorm(temp1, main = paste("pc.1: source", groups[i.group]))
      qqline(temp1)
      temp2 <- temp[, "PC2"]
      qqnorm(temp2, main = paste("pc.2: source", groups[i.group]))
      qqline(temp2)
      ADp1 <- round(ad.test(temp1)$p.value, dig = 3)
      ADp2 <- round(ad.test(temp2)$p.value, dig = 3)
      SWp1 <-
        round(uniNorm(temp1, type = "SW")[[2]]$p.value, dig = 3)
      SWp2 <-
        round(uniNorm(temp2, type = "SW")[[2]]$p.value, dig = 3)
      browser()
      mardia <- mardiaTest(data = temp)
      if (nrow(temp) >= 20)
        p.kurtosis <-
        round(mardia@p.value.kurt, dig = 3)
      else
        p.kurtosis <- round(mardia@p.value.small, dig = 3)
      c(ADp1,
        ADp2,
        SWp1,
        SWp2,
        round(mardia@p.value.skew, dig = 3),
        p.kurtosis)
    }
    for (page in 1:n.pages) {
      plot.new()
      par(mfrow = c(2, 2))
      i.group <- i.group + 1  #  first group for this row and page
      pvalues[i.group,] <- fn.plot()
      i.group <- i.group + 1  # second group
      if (i.group <= length(groups))
        pvalues[i.group,] <- fn.plot()
      browser()
    }
    # diagnostic plots from Mardia test multivariate diagnostic plots
    fn.Mardia.plot <- function() {
      temp <-
        Predicted[Predicted[, "group"] == groups[i.group], c("PC1", "PC2")]
      mardia <- mardiaTest(data = temp)
      mvnPlot(mardia, type = "persp")
      mvnPlot(mardia, type = "contour")
    }
    i.group <- 0
    for (page in 1:n.pages) {
      plot.new()
      par(mfrow = c(2, 2))
      i.group <- i.group + 1  #  first group for this row and page
      fn.Mardia.plot()
      i.group <- i.group + 1  # second group
      if (i.group <= length(groups))
        fn.Mardia.plot()
      browser()
    }
    if (QQtest) {
      # plots using qqtest() qq plots and Anderson-Darling p-values
      fn.qqtest <- function() {
        temp <-
          Predicted[Predicted[, "group"] == groups[i.group], c("PC1", "PC2")]
        temp1 <- temp[, "PC1"]
        qqtest(
          data = temp1,
          dist = "normal",
          drawPercentiles = T,
          main = paste("pc.1: source",
                       groups[i.group])
        )
        temp2 <- temp[, "PC2"]
        qqtest(
          data = temp2,
          dist = "normal",
          drawPercentiles = T,
          main = paste("pc.2: source",
                       groups[i.group])
        )
      }
      i.group <- 0  # initialize choice for group
      for (page in 1:n.pages) {
        plot.new()
        par(mfrow = c(2, 2))
        i.group <-
          i.group + 1  #  first group for this row and page
        fn.qqtest()
        i.group <- i.group + 1  # second group
        if (i.group <= length(groups))
          fn.qqtest()
        browser()
      }
    }
    write.csv(pvalues, paste(folder, ds.pvalues, sep = ""))
    list(Documentation = doc, p.valuesp = pvalues)
  }
