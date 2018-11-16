#' fn.randomForest
#'
#'   Random forest analysis
#'
#' @param doc Documentation for the function use, default value is the function name
#' @param data:  Data frame with the data to be analyzed
#' @param predictData:  Data frame with data on unknowns, NA if predictions not made (default value)
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
#' @param plotSourceProbs: Logical, if T (the default) and predictSources=T, show box plots of source
#'    probabilities
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @return The function implements a random forest analysis.  For each analysis, the variables used in the
#'           analysis are considered in the order in which they appear in AnalyticVars (from left to right);
#'           See the vignette for more details.
#'           The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{predictData:}{  The data frame in the argument predictData}
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
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
#'   ID="labID", AnalyticVars=analyticVars, NvarUsed=3)
#' #
#' # predicted sources for artifacts
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- fn.randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
#' AnalyticVars=analyticVars, ID="ID", NvarUsed=3, plotErrorRate=F, plotImportance=F,
#' predictSources=T, predictData=ObsidianArtifacts, plotSourceProbs=T)
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
           predictData = NA,
           plotSourceProbs=T,
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
    if (plotImportance)  {
      varImpPlot(fit.rf, main = "Variable importance")
      browser()
    }
#
    if (predictSources == F) {
      predictedTotals <- NA
      predictions <- NA
    }   # dummy values
    #
    if (predictSources == T) {
      response <- predict(object=fit.rf, newdata=predictData, type="response")
      probMatrix <- predict(object=fit.rf, newdata=predictData, type="prob")
      pred.source <- table(response)
      pred.probs <- apply(probMatrix,2,sum)
      predictedTotals <- rbind(pred.source, pred.probs)
      rownames(predictedTotals) <- c("source", "sum probabilities")
      if (ID == " ")
        predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                  predictData[,AnalyticVars])
      if (ID != " ")
        predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                predictData[,c(ID,AnalyticVars)])
      #
      #  box plots of source probabilities
      # probFrame <- data.frame(source=response, probMatrix)
      probSource <- data.frame(source=response,SourceProbability=rep(NA,length(response)))
         # data frame with assignment probabilities for the assigned source for each unknown
      for (i in 1:length(response)) {
        for (j in 1:ncol(probMatrix))
          if (response[i]==colnames(probMatrix)[j])  probSource[i,2] <- probMatrix[i,j]
        }
      if (plotSourceProbs) {
        fn.BoxPlots(data = probSource, GroupVar="source", Groups="All",
                  AnalyticVars="SourceProbability", Nrow=1, Ncol=1)
        browser()
      }
      #
      # box plots of probabilities for sources not assigned to each unknown
      notSource<- rep("x", 4*length(response))
      prob <- rep(NA, 4*length(response))
      i.row <- 0
      colNames <- as.character(colnames(probMatrix), mode="character")
      for (i in 1:length(response)) {
        for (j in 1:ncol(probMatrix)) {
          if (response[i]!=colnames(probMatrix)[j]) {
            i.row <- i.row + 1
            notSource[i.row] <- as.character(colNames[j])
            prob[i.row] <- probMatrix[i,j]
          }
        } # end of loop on j
      } # end of loop on i
      probNotSource <- data.frame(source=notSource, sourceProbability=prob)
      if (plotSourceProbs)
        fn.BoxPlots(data = probNotSource, GroupVar="source", Groups="All",
                  AnalyticVars="sourceProbability", Nrow=1, Ncol=1)
    } # end of code for predictSources == T
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.numeric<-c(NvarUsed, Ntrees, Seed, digitsImportance)
    names(params.numeric)<-c("NvarUsed", "Ntrees", "Seed", "digitsImportance")
    params.logical<-c(plotErrorRate,plotImportance,predictSources,plotSourceProbs)
    names(params.logical) <- c("plotErrorRate","plotImportance","plotSourceProbs")
    importance.rf <- round(importance.rf, dig=digitsImportance)
    #
    out<-list(usage=fcn.date.ver,
                  dataUsed=Data.used,
                  predictData=predictData,
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
                  location = folder)
      out
  }
