#' ps_tree
#'
#' Fit a recursive partitioning model (classification tree) to data from sources
#'
#' @param doc A string with documentation added to defintion of usage,
#' default is ps_tree (the function name)
#' @param data  A data frame with the data to be analyzed
#' @param GroupVar  The name of the variable defining groups, grouping is required
#' @param Groups  A vector of codes for groups to be used, 'All' (the default)
#'  if use all groups
#' @param AnalyticVars  A vector with the names (character values) of the analytic variables
#' @param wts Option to weight the observations, if used, vector with length nrow(data);
#'  if NA (the default), assume equal weights
#' @param Seed A positive integer, to produce a reproducible analysis
#' @param CpDigits  The number of significant digits to display in the Cp table, default value is 3
#' @param plotTree Logical.  If TRUE (the default), plot the recursive partitioning tree
#' @param plotCp  Logical.  If TRUE (the default), plot the Cp table values
#' @param Model  A character string containing the names of the variables (characters) considered
#'  separated by + signs
#' @param ModelTitle  The parameter Model as a single character value
#' @param minSplit  The minimum size of a group for splitting, default is 20
#' (the default in rpart())
#' @param cP  The required improvement in Cp for a group to be split,
#' default is .01 (the default in rpart())
#' @param predictSources  Logical: if TRUE, use the tree to predict sources for the source data;
#'  default is TRUE
#' @param predictUnknowns  Logical: if TRUE, use the tree to predict sources for observations
#'  in unknownData; default is FALSE
#' @param unknownData  Data frame with data used to predict sources, must contain all variables
#'  in AnalyticVars
#' @param ID  If not " " (the default), the name of a variable identifying a sample in unknownData
#' @param folder  The path to the folder in which data frames will be saved; default is " "
#'
#' @details The function fits a classification tree model us the R function rpart().
#'   The variables in AnalyticVars are considered in the order in which they appear in the
#'    Model argument (from left to right).  See the vignette for more details.
#'
#' @return The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{params_grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params:}{ A list with the values of the grouping, logical, and splitting parameters}
#'   \item{Seed:}{ A positive integer to set the random number generator}
#'   \item{model:}{ A character string with the value of the argument ModelTitle}
#'   \item{treeFit:}{ A list with details of the tree construction_}
#'   \item{classification:}  {A data frame showing the crossclassification of sources and predicted sources}
#'   \item{CpTable:}{  A data frame showing the decrease in Cp with increasing numbers of splits}
#'   \item{predictedSource:}{  If predictSources = TRUE, a data frame with the predicted source for each source
#'   sample}
#'   \item{predictedProbs:}{  If predictSources = TRUE, a data frame with the set of prediction probabilities
#'   for each source sample}
#'   \item{errorRate:}{  If predictSources = TRUE, the proportion of misassigned source samples}
#'   \item{errorCount:}{ If predictSources = TRUE, a vector with the number of misassigned sources and
#'   total number of sources}
#'   \item{predictedTotals:}{  If predictUnknowns = TRUE, a vector with the number of objects predicted to be from each source}
#'   \item{location:}{ The value of the argument folder}
#'  }
#'
#' @examples
#' # Analyze the obsidian source data with variables in the model statement in order of
#' # importance from a random forest analysis
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save_tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All",
#' AnalyticVars=analyticVars, Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb",
#'  ModelTitle = "Sr + Nb + Rb + Y + Zr")
#'
#'
#' @import rpart partykit Formula graphics stats assertthat
#'
#' @export

ps_tree <-
  function(doc = "ps_tree",
           data,
           GroupVar,
           Groups = "All",
           AnalyticVars ,
           wts = NA,
           Seed = 11111,
           CpDigits = 3,
           plotTree = TRUE,
           plotCp = TRUE,
           Model,
           ModelTitle,
           minSplit = 20,
           cP = 0.01,
           predictSources = TRUE,
           predictUnknowns = FALSE,
           unknownData,
           ID = " ",
           folder = " ")
{
    #
    #  check for valid parameters
    #
    assert_that(is.data.frame(data), msg="parameter data not a data frame")
    assert_that(is.character(GroupVar), msg="parameter GroupVar not character")
    assert_that(is.character(Groups), msg="parameter Groups not character")
    assert_that(is.vector(AnalyticVars)&is.character(AnalyticVars),
                msg="parameter AnalyticVars not a character vector")
    assert_that(is.character(ID), msg="parameter ID not character valued")
    assert_that(is.na(wts) | is.vector(wts), msg="parameter wts must be NA or a vector")
    if (!is.na(wts)) assert_that(is.numeric(wts) & length(wts)==nrow(data),
                          msg="parameter wts as a vector must be numeric and have length nrow(data)")
    assert_that(is.numeric(CpDigits) | is.na(CpDigits), msg="parameter CpDigits not numeric and not NA")
    if (is.numeric(CpDigits))  assert_that((round(CpDigits,0)==CpDigits)&(CpDigits > 0),
                                 msg="parameter CpDigits not a positive integer")
    assert_that(is.numeric(Seed) | is.na(Seed), msg="parameter Seed not numeric and not NA")
    if (is.numeric(Seed))  assert_that((round(Seed,0)==Seed)&(Seed > 0),
                                           msg="parameter Seed not a positive integer")
    assert_that(is.numeric(minSplit) | is.na(minSplit), msg="parameter minSplit not numeric and not NA")
    if (minSplit > 0)  assert_that((round(minSplit,0)==minSplit)&(minSplit > 0),
                                   msg="parameter minSplit not a positive integer")
    assert_that(is.numeric(cP) & (cP > 0) & (cP < 1), msg="parameter cP not numeric and positive and < 1")
    assert_that(is.logical(predictUnknowns), msg="type of parameter predictUnknowns not logical")
    assert_that(is.character(ModelTitle), msg="parameter ModelTitle not character valued")
    #
    if (!is.na(Seed))
      set.seed(Seed)  # create reproducible analysis
    #
    # create dataset dataUsed based on grouping restrict to desired set of groups
    if (Groups[1] != "All") {
      Use_rows <- (data[, GroupVar] %in% Groups)
      dataUsed <- data[Use_rows, ]
    } else
      dataUsed <- data[, ]
    #
    #  sort source data on GroupVar
    #
    rowsSort <- order(dataUsed[,GroupVar])
    dataUsed <- dataUsed[rowsSort,]
    #
    #  if predictions to be made and ID used, sort on ID
    #
    if ((predictUnknowns == TRUE) & (ID[1] != " "))
      unknownData <- unknownData[order(unknownData[,"ID"]),]
      #
    # define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <-
        as.character(unique(dataUsed[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    if (is.na(wts))
      Weights = rep(1, nrow(dataUsed))
    else
      Weights = wts
    #
    Sources <- factor(dataUsed[, GroupVar])
    formula_rhs <-
      paste(AnalyticVars, collapse = "+")  # right hand side of formula
    formula_tree <-
      as.formula(paste("Sources", formula_rhs, sep = " ~ "))
    treeFit <-
      rpart(formula_tree,
            data = dataUsed,
            weights = Weights,
            method = "class",
            minsplit=minSplit,
            cp=cP)
    if (plotTree) {  #  plotTree is TRUE
      plot(as.party(treeFit), tp_args = list(id = FALSE), main=paste("model:",ModelTitle))
    }
    # classification: accuracy of predicting sources
    predicteds<-rep(" ",length(treeFit$y))
    for (i in 1:length(predicteds))  predicteds[i] <- groups[treeFit$y[i]]
    #
    classification <- table(predicteds, Sources)
    # evaluate tree size
    CpTable <- treeFit$cptable
    #
    if (plotCp) {
      plot(
      x = CpTable[, "nsplit"],
      y = CpTable[, "xerror"],
      ylim = c(0, 1),
      xlab = "number of splits",
      ylab = "rpart cross-validation error estimate",
      pch = 1
             )
      lines(x = CpTable[, "nsplit"], y = CpTable[, "xerror"], lty = 1)
      legend(x = "bottomleft", legend = formula_rhs, bty = "n")
     } # end of code for plotCp
    # optimal number of splits
    nsplitopt <- vector(mode = "integer", length = 25)
    for (i in 1:length(nsplitopt)) {
      cp <- treeFit$cptable
      nsplitopt[i] <- cp[which.min(cp[, "xerror"]), "nsplit"]
    }
    nsplitopt <- cbind(Model = rep(0, 25), Splits = nsplitopt)
    Nsplitopt <-
      table(Model = nsplitopt[, "Model"], Splits = nsplitopt[, "Splits"])
    #
    if (predictSources == TRUE) {
      predictedProbs <- predict(object = treeFit, newdata = dataUsed)
        #  matrix of probabilities: i,j is probability of source j for sample i
      # set up matrix to store indicator values: 1 when sample predicted to be from source
      predictedSource <- matrix(0, nrow(predictedProbs), ncol(predictedProbs))
      rownames(predictedSource) <- dataUsed[,GroupVar]
      colnames(predictedSource) <- groups
      #
      #  define predicted source for each sample
      for (i in 1:nrow(predictedSource)) {
         index_i <- 1 # column index with maximum probability
         for (j in 1:ncol(predictedProbs))
           if(predictedProbs[i,j] > predictedProbs[i,index_i])  index_i <- j
                predictedSource[i,index_i] <- 1
      }
      #  add GroupVar, ID (if given) to predictedProbs
      if (ID == " ")  predictedProbs <- data.frame(source=dataUsed[,GroupVar], predictedProbs)
      if (ID != " ") {
        predictedProbs <- data.frame(dataUsed[,c(GroupVar,ID)], predictedProbs)
        colnames(predictedProbs)<-c("source",ID,groups)
      }
      rownames(predictedProbs) <- 1:nrow(predictedProbs)
      #
      #  add GroupVar, ID (if given) to predictedSource
      if (ID == " ")  predictedSource <- data.frame(source=dataUsed[,GroupVar], predictedSource)
      if (ID != " ") {
        predictedSource  <- data.frame(dataUsed[,c(GroupVar,ID)], predictedSource)
        colnames(predictedSource) <- c("source",ID,groups)
      }
      rownames(predictedSource) <- 1:nrow(predictedSource)
     #
     #  create matrix with number assigned to each group, by group
     #
     classMatrix <- matrix(0,nrow=length(groups),ncol=length(groups))
     dimnames(classMatrix) <- list(groups,groups)
     for (i in 1:length(groups)) {
        data_i <- predictedSource[predictedSource[,"source"]==groups[i],groups]
        classMatrix[i,] <- apply(data_i,2,sum)
     }
     #
     #  error rate
     #
     nCorrect <- sum(diag(classMatrix))
     nTotal <- sum(classMatrix)
     errorRate <- 1 - round(nCorrect/nTotal, dig=3)
     errorCount <- round(c(nTotal-nCorrect, nTotal), dig = 0)
     names(errorCount) <- c("incorrect", "n total")
     #
#     if (substr(ID,1,1) == " ")  predictedResults<-data.frame(unknownData[,AnalyticVars])
#     if (substr(ID,1,1) != " ")  predictedResults<-data.frame(unknownData[,c(ID, AnalyticVars)])
      #
      } # end of code for predictSources == TRUE
    #
    nsplit <- CpTable[,"nsplit"]
    Cp <- round(CpTable[,-2],digits = CpDigits)
    CpTable <- cbind(nsplit,Cp)
    #
    fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
    #
    params_grouping<-list(GroupVar,Groups)
    names(params_grouping)<-c("GroupVar","Groups")
    params_logical<-c(plotTree, plotCp, predictSources, predictUnknowns)
    names(params_logical)<-c("plotTree", "plotCp", "predictSources", "predictUnknowns")
    params_splitting <- c(minSplit, cP, CpDigits)
    names(params_splitting) <- c("minSplit","cP", "CpDigits")
    params<-list(grouping=params_grouping,logical=params_logical,splitting=params_splitting)
    #
    nsplit <- CpTable[,"nsplit"]
    Cp <- round(CpTable[,-2],digits = CpDigits)
    CpTable <- cbind(nsplit,Cp)
    #
    if (!predictUnknowns)
      out<-list(usage=fcnDateVersion,
                dataUsed=dataUsed,
                analyticVars=AnalyticVars,
                params=params,
                model=ModelTitle,
                treeFit = treeFit,
                classification = classMatrix,
                errorRate = errorRate,
                CpTable = CpTable,
                location=folder)
    #
    if (predictSources)
        out<-list(usage=fcnDateVersion,
                  dataUsed=dataUsed,
                  analyticVars=AnalyticVars,
                  params=params,
                  Seed=Seed,
                  model=ModelTitle,
                  treeFit = treeFit,
                  predictedProbs = predictedProbs,
                  predictedSource = predictedSource,
                  classification = classMatrix,
                  errorRate = errorRate,
                  errorCount = errorCount,
                  CpTable = CpTable,
#                  predictedSources = predictedResults,
#                  predictedTotals = data.frame(t(predictedTotals)),
                  location=folder)
   out
  }
