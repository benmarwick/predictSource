#' fn.randomForest
#'
#'   Random forest analysis
#'

#' @param doc Documentation for the function use, default value is the function name
#' @param data:  Data frame with the data to be analyzed
#' @param GroupVar: Name of variable defining groups, grouping is required
#' @param Groups: Vector of codes for groups to be used, 'All' if use all groups
#' @param ID: if not " " (the default), the name of the variable with sample ID
#' @param AnalyticVars: Vector with names of analytic variables
#' @param Ntrees: Number of trees grown, default value of 500 is that for the randomForest function
#' @param NvarUsed: If not NA, number of variables to use in each random forest call to rpart;
#'                  if NA, rpart uses the default (the square root of the number of candidate variables)
#' @param Seed: If not NA, a random number generator seed to produce reproducible results
#' @param digitsImportance:  Significant digits for the importance measure, default is 1
#' @param plotErrorRate: Logical, whether to show the error rate plot, default is T
#' @param plotImportance: Logical, whether to show the plot of variable importance, default is T
#' @param predictSources: Logical; if T, predict sources for the data in predictData; default is F
#' @param predictData: data frame or matrix with data used to predict sources for observations,
#'    must contain all variables in AnalyticVars
#' @param folder: If not " " (the default), the path to folder containing excel files, must end with '\\'
#' @param ds.importance: Excel file with importance measures, extension .csv
#' @param ds.confusion: Excel file with confusion matrix, extension.csv
#' @param ds.predictedSources: If predictSources = T, an excel file with the information in predictedSources
#' @param ds.predictedTotals: If predictSources = T, an excel file with the vector predictedTotals
#'
#' @return The function implements a random forest analysis.  For each analysis, the variables used in the
#'           analysis are considered in the order in which they appear in AnalyticVars (from left to right);
#'           See the vignette for more details.
#'           The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{params.grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params.numeric:}{ A numeric vector with the values of the arguments Ntrees, NvarUsed, and Seed}
#'   \item{formula.rf:}  {The formula used in the analysis (the variables specified in the argument AnalyticVars
#'                        separated by + signs)}
#'   \item{forest:}{  The random forest}
#'   \item{importance:}{  A data frame with information on the importance of each variable in AnalyticVars}
#'   \item{confusion:}{  A data frame with the estimate of the confusion matrix}
#'   \item{predictedSources:}{  A data frame with prediction information, sample ID (if requested),
#'      and values of AnalyticVars}
#'   \item{predictedTotals:}{  A vector with the predicted totals for each group (source)}
#'   \item{files:}{ If folder != " ", a character string with the path to the file containing the excel files
#'                  with the importance and confusion estimates}
#'  }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
#'   NvarUsed=3)
#' #
#' # predicted sources for artifacts
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
#'   NvarUsed=3, predictSources=T,predictData=ObsidianArtifacts)
#'
#' @import  MASS randomForest rpart
#'
#' @export

fn.randomForest <-
  function(doc = "fn.randomForest",
           data,
           GroupVar,
           Groups = "All",
           AnalyticVars,
           ID = " ",
           Ntrees = 500,
           NvarUsed = NA,
           Seed = 11111,
           digitsImportance = 1,
           plotErrorRate = T,
           plotImportance = T,
           predictSources = F,
           predictData,
           folder = " ",
           ds.importance ,
           ds.confusion ) {

    # create dataset Data based on grouping restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      Data.used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    } else
      Data.used <- data[, c(GroupVar, AnalyticVars)]
    # define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <-
        as.character(unique(Data.used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    if (!is.na(Seed))
      set.seed(Seed)  # create reproducible analysis
    #
    Sources <- factor(Data.used[, GroupVar])
    formula.rhs <-
      paste(AnalyticVars, collapse = "+")  # right hand side of formula
    formula.rf <-
      as.formula(paste("Sources", formula.rhs, sep = " ~ "))
    if (is.na(NvarUsed))
      fit.rf <-
      randomForest(formula.rf, data = Data.used, ntree = Ntrees)
    else
      fit.rf <-
      randomForest(formula.rf,
                   data = Data.used,
                   mtry = NvarUsed,
                   ntree = Ntrees)
    # specify number of variables in each call to rpart
    if (plotErrorRate) {
      plot(fit.rf, main = "Estimated error rate by number of trees")
      browser()
    }
    #
    importance.rf <- importance(fit.rf)
    if (plotImportance)  varImpPlot(fit.rf, main = "Variable importance")
    if (substr(folder,1,1) != " ")  write.csv(importance.rf, file = paste(folder, ds.importance, sep = ""))
    #
    if (substr(folder,1,1) != " ")  write.csv(fit.rf$confusion, file = paste(folder, ds.confusion, sep = ""))
    #
    if (predictSources == T) {
      response <- predict(object=fit.rf, newdata=predictData, type="response")
      probMatrix <- predict(object=fit.rf, newdata=predictData, type="prob")
      pred.source <- table(response)
      pred.probs <- apply(probMatrix,2,sum)
      predictedTotals <- list(source = pred.source, probs = pred.probs)
      if (ID == " ")
        predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                  predictData[,AnalyticVars])
      if (ID != " ")
        predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                predictData[,c(ID,AnalyticVars)])
    }
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.numeric<-c(NvarUsed, Ntrees, Seed, digitsImportance)
    names(params.numeric)<-c("NvarUsed", "Ntrees", "Seed", "digitsImportance")
    params.logical<-c("plotErrorRate","plotImportance")
    names(params.logical) <- c("plotErrorRate","plotImportance")
    importance.rf <- round(importance.rf, dig=digitsImportance)
    #
    if (folder != " ") {
      if (predictSources == F)
      fileNames <- list(paste(folder,ds.importance,sep=""),paste(folder,ds.confusion,sep=""))
    }
    if (folder != " ") {
      if (predictSources == T)
        fileNames <- list(paste(folder,ds.importance,sep=""),paste(folder,ds.confusion,sep=""),
                          paste(folder,ds.predictedSources,sep=""),paste(folder,ds.predictedTotals,sep=""))
    }
    #
    if (substr(folder,1,1) == " ") {
      if (predictSources == F)
        out<-list(usage=fcn.date.ver,
                dataUsed=Data.used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                params.numeric=params.numeric,
                params.logical=params.logical,
                formula.rf=formula.rf,
                forest = fit.rf,
                importance = importance.rf,
                confusion = fit.rf$confusion
      )
    }
    if (substr(folder,1,1) != " ") {
      if (predictSources == F)
      out<-list(usage=fcn.date.ver,
                dataUsed=Data.used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                params.numeric=params.numeric,
                params.logical=params.logical,
                formula.rf=formula.rf,
                forest = fit.rf,
                importance = importance.rf,
                confusion = fit.rf$confusion,
                files = fileNames
      )
    }
    #
    if (substr(folder,1,1) == " ") {
      if (predictSources == T)
        out<-list(usage=fcn.date.ver,
                  dataUsed=Data.used,
                  analyticVars=AnalyticVars,
                  params.grouping=params.grouping,
                  params.numeric=params.numeric,
                  params.logical=params.logical,
                  formula.rf=formula.rf,
                  forest = fit.rf,
                  importance = importance.rf,
                  confusion = fit.rf$confusion,
                  predictedSources = predictions,
                  predictedTotals = predictedTotals
        )
    }
    if (substr(folder,1,1) != " ") {
      if (predictSources == T)
        out<-list(usage=fcn.date.ver,
                  dataUsed=Data.used,
                  analyticVars=AnalyticVars,
                  params.grouping=params.grouping,
                  params.numeric=params.numeric,
                  params.logical=params.logical,
                  formula.rf=formula.rf,
                  forest = fit.rf,
                  importance = importance.rf,
                  confusion = fit.rf$confusion,
                  predictedSources = predictions,
                  predictedTotals = predictedTotals,
                  files = fileNames
        )
    }
    out
  }
