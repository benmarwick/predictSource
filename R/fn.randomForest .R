#' fn.randomForest
#'
#'   random forest analysis doc: documentation data: analysis data
#'

#' @param doc .
#' @param data .
#' @param GroupVar: name of variable defining groups, grouping is required
#' @param Groups: vector of
#' codes for groups to be used, 'All' if use all groups
#' @param AnalyticVars: vector with
#' names of analytic variables
#' @param Ntrees: number of trees grown, default value of 500 is
#' that for the randomForest function
#' @param NvarUsed: if not NA, number of variables to use
#' in each random forest call to rpart
#' @param Seed: if not NA, random number generator seed
#' to produce reproducible results
#' @param folder: path to folder containing excel files, must
#' end with '\\'
#' @param ds.importance: excel file with importance measures, extension .csv
#' @param ds.confusion: excel file with confusion matrix, extension.csv
#'
#' @import  MASS randomForest rpart
#'
#' @export

fn.randomForest <-
  function(doc = "fn.randomForest",
           data,
           GroupVar = "Code",
           Groups = "All",
           AnalyticVars,
           Ntrees = 500,
           NvarUsed = 3,
           Seed = 11111,
           folder ,
           ds.importance ,
           ds.confusion ) {

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
    #
    if (!is.na(Seed))
      set.seed(Seed)  # create reproducible analysis
    #
    Sources <- factor(Data[, GroupVar])
    formula.rhs <-
      paste(AnalyticVars, collapse = "+")  # right hand side of formula
    formula.rf <-
      as.formula(paste("Sources", formula.rhs, sep = " ~ "))
    if (is.na(NvarUsed))
      fit.rf <-
      randomForest(formula.rf, data = Data, ntree = Ntrees)
    else
      fit.rf <-
      randomForest(formula.rf,
                   data = Data,
                   mtry = NvarUsed,
                   ntree = Ntrees)
    # specify number of variables in each call to rpart
    plot(fit.rf, main = "Estimated error rate by number of trees")
    browser()
    #
    importance.rf <- importance(fit.rf)
    varImpPlot(fit.rf, main = "Variable importance")
    write.csv(importance.rf, file = paste(folder, ds.importance, sep = ""))
    #
    write.csv(fit.rf$confusion, file = paste(folder, ds.confusion, sep = ""))
    #
    list(
      Documentation = doc,
      formula.rf = formula.rf,
      forest = fit.rf,
      importance = importance.rf,
      confusion = fit.rf$confusion
    )
  }
