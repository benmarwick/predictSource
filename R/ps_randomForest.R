#' ps_randomForest
#'
#'   Implements a random forest analysis of source data, and predicts sources of unknowns if requested
#'
#' @param doc Documentation for the function use added to model usage, default value is the function name
#' @param data  A data frame with the data used to grow trees (source data if predictions are made)
#' @param GroupVar The name of variable defining groups, grouping is required
#' @param Groups A vector of codes for groups to be used, 'All' if use all groups
#' @param sourceID If not " " (the default), the name of the variable with sample ID for source data
#' @param AnalyticVars A vector with names (character-valued) of the analytic variables
#' @param Ntrees The number of trees grown, default value of 500 is that for the randomForest function
#' @param NvarUsed If not NA (the default), the number of variables to use in each random forest call
#'  to rpart; if NA, rpart uses the default value for randomForest()
#'   (the square root of the number of candidate variables)
#' @param Seed If not NA, a random number generator seed to produce reproducible results;
#' default value is 11111
#' @param digitsImportance  The number of significant digits for the importance measure, default is 1
#' @param plotErrorRate Logical, whether to show the error rate plot, default is T
#' @param plotImportance Logical, whether to show the plot of variable importance, default is T
#' @param predictSources Logical; if T, predict sources for the data in predictData; default is F
#' @param predictData A data frame or matrix with data used to predict sources for observations,
#'    must contain all variables in AnalyticVars.
#' @param artifactID if not " " (the default), the name of the variable with the sample ID for
#'  artifact data
#' @param plotSourceProbs Logical, if T (the default) and predictSources=T, show box plots of source
#'    probabilities
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @details The function implements a random forest analysis using the R function randomForest().
#' If predictSources = T and plotSourceProbs is T, the function creates two box plots.
#'   The first plot shows, for each source,  the set of probabilities of assignment to that source
#'    for the observations assigned to that source (all of these probabilities should be large).
#'    The second plot shows, for each source, the set of probabilities of assignment to that source
#'     for the observations not assigned to that source (for each source,
#'     there is one such probability for observation); these probabilities should be relatively small,
#'     and some should be zero.  See the vignette for more details and examples of these plots.
#'
#' @return The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{sourcesNA:}{  A data frame with data from the data frame data with missing values,
#'    NÁ if no missing values}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params:}{ A list with the values of the grouping, logical, and numeric arguments}
#'   \item{formula.rf:}  {The formula used in the analysis (the variables specified in the argument AnalyticVars
#'                        separated by + signs)}
#'   \item{forest:}{  A summary of the random forest call, estimated error rate, and
#'   confusion matrix}
#'   \item{importance:}{  A data frame with information on the importance of each variable
#'    in AnalyticVars}
#'   \item{confusion:}{  A data frame with the estimate of the confusion matrix}
#'   \item{predictedData:} {  A data frame with the artifact data used for predictions; if there
#'   is missing data, after imputation of the missing data}
#'   \item{predictedNA:}{ A data frame with the observations for which missing data were imputed;
#'   NA if there are no missing data}
#'   \item{predictedSources:}{  A data frame with prediction information, sample ID (if requested),
#'      and values of AnalyticVars}
#'   \item{predictedTotals:}{  A vector with the predicted totals for each group (source)}
#'   \item{impError:}{ The estimated OOB (out of bag) error for imputed predictor data;
#'   NA if no imputed data}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- ps_randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
#'   sourceID="ID", AnalyticVars=analyticVars, NvarUsed=3, plotSourceProbs=F)
#' #
#' # predicted sources for artifacts
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.randomForest <- ps_randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
#' AnalyticVars=analyticVars, sourceID="ID", NvarUsed=3, plotErrorRate=F, plotImportance=F,
#' predictSources=T, predictData=ObsidianArtifacts, artifactID="ID", plotSourceProbs=T)
#'
#' @import  MASS randomForest  missForest rpart
#'
#' @export

ps_randomForest <-
  function(doc = "ps_randomForest",
           data,
           GroupVar,
           Groups = "All",
           AnalyticVars,
           sourceID = " ",
           Ntrees = 500,
           NvarUsed = NA,
           Seed = 11111,
           digitsImportance = 1,
           plotErrorRate = T,
           plotImportance = T,
           predictSources = F,
           predictData = NA,
           artifactID = " ",
           plotSourceProbs=T,
           folder = " "
           )
{
    # create dataset Data.used based on grouping: restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      if (sourceID != " ") {
        Data.used <- data[Use.rows, c(GroupVar, AnalyticVars)]
        dataID <- data[Use.rows, sourceID]
      }
      } else
      Data.used <- data[, c(GroupVar, AnalyticVars)]
    #
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
    # matrix to contain indices for observations with no missing values
    #
    dataKeep <- rep(T, nrow(Data.used))
    for (i in 1:length(AnalyticVars))
      dataKeep[is.na(Data.used[,AnalyticVars[i]])] <- F
    #
    #  redefine Data.used if some analysis variables are missing by imputing missing values
    #
    if (sum(dataKeep) < nrow(Data.used)) {
      temp<-rfImpute(Data.used[,GroupVar] ~ ., Data.used)
      Data.used <- temp[,c(GroupVar,AnalyticVars)]
      sourcesNA <- data[!dataKeep,]
    }
      else sourcesNA <- NA
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
    #
    #  specify one plot per page
    #
    par(mfrow=c(1,1))
    #
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
      predictNA <- NA
      impError <- NA
    }   # dummy values
    #
    if (predictSources == T)  {
      #
      #  vector with F if row contains missing analytic variable
      #
      predictKeep <- rep(T, nrow(predictData))
      for (i in 1:length(AnalyticVars))
        predictKeep[is.na(predictData[,AnalyticVars[i]])] <- F
      #
      #  redefine predictData if some analysis variables are missing by imputing missing values
      #
      if (sum(predictKeep) < nrow(predictData)) {
        predictNA <- predictData[!predictKeep,]
        temp<-missForest(xmis=predictData[,AnalyticVars],variablewise=F)
        impError <- temp$OOBerror
        if (artifactID == " ") predictData <- data.frame(predictData[,GroupVar],temp$ximp)
        else  predictData <- data.frame(predictData[,c(GroupVar, artifactID)],temp$ximp)
        }
      else {
        predictNA <- NA
        impError <- NA
      }
      #
      response <- predict(object=fit.rf, newdata=predictData, type="response")
      probMatrix <- predict(object=fit.rf, newdata=predictData, type="prob")
      pred.source <- table(response)
      pred.probs <- apply(probMatrix,2,sum)
      predictedTotals <- rbind(pred.source, pred.probs)
      rownames(predictedTotals) <- c("source", "sum probabilities")
      if (artifactID == " ")
        predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                  predictData[,AnalyticVars])
      if (artifactID != " ")
         predictions <- data.frame(source=as.character(response), as.matrix(probMatrix),
                                predictData[,c(artifactID,AnalyticVars)])
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
    importance.rf <- round(importance.rf, dig=digitsImportance)
    #
    if ((artifactID != " ") & (predictSources == T))
      predictions <- predictions[order(predictions[,artifactID]),]
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    #
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.numeric<-c(NvarUsed, Ntrees, Seed, digitsImportance)
    names(params.numeric)<-c("NvarUsed", "Ntrees", "Seed", "digitsImportance")
    params.logical<-c(plotErrorRate,plotImportance,predictSources,plotSourceProbs)
    names(params.logical) <- c("plotErrorRate","plotImportance","predictSources","plotSourceProbs")
    params<-list(grouping=params.grouping,logical=params.logical,numeric=params.numeric)
    #
    out<-list(usage=fcn.date.ver,
                  dataUsed=Data.used,
                  sourcesNA=sourcesNA,
                  predictData=predictData,
                  analyticVars=AnalyticVars,
                  params=params,
                  formula.rf=formula.rf,
                  forest = fit.rf,
                  importance = importance.rf,
                  confusion = fit.rf$confusion,
                  predictedData = predictData,
                  predictedNA = predictNA,
                  predictedSources = predictions,
                  predictedTotals = predictedTotals,
                  impError = impError,
                  location = folder)
      out
  }
