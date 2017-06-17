#' fn.CV.corr
#'
#' Compute coefficients of variation and correlations for specified
#' analytic values, by specified groups
#'
#' @param data: R matrix or data frame containing the
#' data to be analyzedfn
#' @param GroupVar: name for variable defining grouping, ' ' if no
#' grouping
#' @param Groups: vector of values of group variable for which plots are to be done
#' 'All': use all groups ' ': no grouping AnalyticVars: vector of names (character
#' values) of analytic results
#' @param Transpose: if T, one row for the correlations between
#' each pair of analyses (columns are groups) if F, one row for each group, column for
#' correlations between a pair of analyses
#' @param folder: location to store excel files with
#' coefficients of variation and Spearman correlations
#' @param ds.CV: file name for
#' coefficients of variation, with extension .csv
#' @param ds.corr: file name for Spearman
#' correlation coefficients, with extension .csv restrict to desired set of groups
#'
#' @export

fn.CV.corr <-
  function(doc = "fn.CV.corr",
           data,
           GroupVar,
           Groups,
           AnalyticVars,
           Transpose = T,
           folder,
           ds.CV,
           ds.corr) {

    if ((Groups[1] != " ") & (Groups[1] != "All")) {
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    } else if (GroupVar[1] == " ")
      data.Used <-
        data[, AnalyticVars]
    else
      data.Used <- data[, c(GroupVar, AnalyticVars)]
    # coefficient of variation no grouping
    if (GroupVar[1] == " ") {
      CV <- rep(NA, length(AnalyticVars))
      means <- apply(data.Used, 2, mean)
      std <- sqrt(apply(data.Used, 2, var))
      CV <- round(std / means, dig = 2)
      names(CV) <- AnalyticVars
    } else {
      if (Groups[1] == "All")
        groups <- unique(data.Used[, GroupVar])
      if (Groups[1] != "All")
        groups <- Groups
      #
      compute.CV <-
        matrix(NA,
               nrow = length(groups),
               ncol = length(AnalyticVars))  # matrix to store values of CV for each group
      for (i in 1:length(groups)) {
        rows.i <-
          (data.Used[, GroupVar] %in% groups[i])  # rows from group i
        data.i <-
          data.Used[rows.i, AnalyticVars]  # data restricted to group i
        means.i <- apply(data.i, 2, mean)
        std.i <- sqrt(apply(data.i, 2, var))
        compute.CV[i,] <- round(std.i / means.i, dig = 2)
      }
      CV <- data.frame(groups, compute.CV)
      colnames(CV) <- c(GroupVar, AnalyticVars)
    }
    write.csv(CV, paste(folder, ds.CV, sep = ""))
    # Spearman correlations
    if (GroupVar[1] == " ") {
      Corrs <-
        round(cor(
          x = data.Used,
          method = "spearman",
          use = "pairwise.complete.obs"
        ),
        dig = 2)
    } else {
      Corrs <-
        matrix(NA,
               nrow = length(AnalyticVars) * (length(AnalyticVars) - 1) / 2,
               ncol = length(groups))
      colnames(Corrs) <- groups
      Rows <-
        paste(AnalyticVars[1], "/", AnalyticVars[2:length(AnalyticVars)], sep = "")
      for (i in 2:(length(AnalyticVars) - 1))
        Rows <- c(Rows, paste(AnalyticVars[i],
                              "/", AnalyticVars[(i + 1):length(AnalyticVars)], sep = ""))
      rownames(Corrs) <- Rows
      # data frame with Code and desired elements
      DataEls <- data.Used[, "Code"]
      for (i in 1:length(AnalyticVars))
        DataEls <- data.frame(DataEls, data[, AnalyticVars[i]])
      colnames(DataEls) <- c("Code", AnalyticVars)
      # compute correlations and store in Corrs
      for (i in 1:length(groups)) {
        SourceData <- DataEls[DataEls[, GroupVar] == groups[i],]
        SourceCorr <-
          round(cor(
            x = SourceData[, -1],
            method = "spearman",
            use = "pairwise.complete.obs"
          ),
          dig = 2)
        # load correlations into Corrs
        Row <- 0  # row in which to load correlations
        for (j in 1:(length(AnalyticVars) - 1)) {
          Corrs[(Row + 1):(Row + length(AnalyticVars) - j), i] <-
            SourceCorr[j,
                       (j + 1):length(AnalyticVars)]
          Row <- Row + length(AnalyticVars) - j
        }
      }
    }
    if (Transpose == T)
      Corrs <- t(Corrs)
    write.csv(Corrs, file = paste(folder, ds.corr, sep = ""))
    #
    list(Documentation = doc,
         CV = CV,
         corr = Corrs)
  }
