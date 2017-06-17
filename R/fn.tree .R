#' fn.tree
#'
#' fit recursive partitioning model doc: documentation
#'
#' @param doc .
#' @param data .
#' @param GroupVar  name of variable defining groups, grouping is required
#' @param Groups vector of codes for groups to be used, 'All' if use all groups
#' @param AnalyticVars  names of analytic variables wts: option to weight the observations, if used, vector with length nrow(data) if NA, assume equal weights
#' @param wts .
#' @param PlotTree if T (true), plot the recursive partitioning tree
#' @param Model .
#' @param folder  path to folder containing excel files, must end with '\\'
#' @param outClassify file containing results of classifying the data
#' @param outCpTable file containing the values of Cp at successive splits
#' @param outCVtable .
#' @param outOptSplit  file containing predictors are considered in the tree model in the order in which they appeat in AnalyticVars
#'
#' @import rpart partykit Formula
#'
#' @export

fn.tree <-
  function(doc = "fn.tree",
           data,
           GroupVar = "Code",
           Groups = "All",
           AnalyticVars ,
           wts = NA,
           PlotTree = T,
           Model = "Rb+Y+Nb+Zr+Sr",
           folder,
           outClassify,
           outCpTable,
           outCVtable,
           outOptSplit) {


    # create dataset Data based on grouping restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      Data <- data[Use.rows, c(GroupVar, AnalyticVars)]
    } else
      Data <- data[, c(GroupVar, AnalyticVars)]
    # define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <-
        as.character(unique(Data[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    Data <- data[, c("Code", AnalyticVars)]
    if (is.na(wts))
      Weights = rep(1, nrow(Data))
    else
      Weights = wts
    #
    Sources <- factor(Data[, GroupVar])
    formula.rhs <-
      paste(AnalyticVars, collapse = "+")  # right hand side of formula
    formula.tree <-
      as.formula(paste("Sources", formula.rhs, sep = " ~ "))
    Tree <-
      rpart(formula.tree,
            data = Data,
            weights = Weights,
            method = "class")
    if (PlotTree == T)
      plot(as.party(Tree), tp_args = list(id = FALSE))
    browser()
    # classification
    Classification <- table(Tree$y, Sources)
    write.csv(t(Classification), paste(folder, outClassify, sep = ""))
    # evaluate tree size
    CpTable <- Tree$cptable
    write.csv(CpTable, paste(folder, outCpTable, sep = ""))
    #
    plot(
      x = CpTable[, "nsplit"],
      y = CpTable[, "xerror"],
      ylim = c(0, 1),
      xlab = "number of splits",
      ylab = "rpart cross-validation error estimate",
      pch = 1
    )
    lines(x = CpTable[, "nsplit"], y = CpTable[, "xerror"], lty = 1)
    legend(x = "bottomleft", legend = formula.rhs, bty = "n")
    browser()
    # optimal number of splits
    nsplitopt <- vector(mode = "integer", length = 25)
    for (i in 1:length(nsplitopt)) {
      cp <- Tree$cptable
      nsplitopt[i] <- cp[which.min(cp[, "xerror"]), "nsplit"]
    }
    nsplitopt <- cbind(Model = rep(0, 25), Splits = nsplitopt)
    browser()
    Nsplitopt <-
      table(Model = nsplitopt[, "Model"], Splits = nsplitopt[, "Splits"])
    write.csv(table(Nsplitopt), paste(folder, outOptSplit, sep = ""))
    #
    list(
      Documentation = doc,
      Tree = Tree,
      Classification = Classification,
      CpTable = CpTable,
      NOptSplit = nsplitopt
    )
  }
