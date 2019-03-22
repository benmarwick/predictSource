#' ps_tree
#'
#' Fit a recursive partitioning model (classification tree) to data from sources
#'
#' @param doc A string with documentation added to defintion of usage, default is ps_tree (the function name)
#' @param data  A data frame with the data to be analyzed
#' @param GroupVar  The name of the variable defining groups, grouping is required
#' @param Groups  A vector of codes for groups to be used, 'All' (the default) if use all groups
#' @param AnalyticVars  A vector with the names (character values) of the analytic variables
#' @param wts Option to weight the observations, if used, vector with length nrow(data); if NA (the default), assume equal weights
#' @param CpDigits  The number of significant digits to display in the Cp table, default value is 3
#' @param plotTree Logical.  If T (true, the default), plot the recursive partitioning tree
#' @param plotCp  Logical.  If T (tree, the default), plot the Cp table values
#' @param Model  A character string containing the names of the variables (characters) considered
#'  separated by + signs
#' @param ModelTitle  The parameter Model as a single character value
#' @param minSplit  The minimum size of a group for splitting, default is 20 (the default in rpart())
#' @param cP  The required improvement in Cp for a group to be split, default is .01 (the default in rpart())
#' @param predictSources  Logical: if T, use the tree to predict sources for observations
#'  in predictData; default is F
#' @param predictData  Data frame with data used to predict sources, must contain all variables
#'  in AnalyticVars
#' @param ID  If not " " (the default), the name of a variable identifying a sample in predictData
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
#'   \item{params.grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params:}{ A list with the values of the grouping, logical, and splitting parameters}
#'   \item{model:}{ A character string with the value of the argument ModelTitle}
#'   \item{Tree:}{ A list with details of the tree construction.}
#'   \item{classification:}  {A data frame showing the crossclassification of sources and predicted sources}
#'   \item{CpTable:}{  A data frame showing the decrease in Cp with increasing numbers of splits}
#'   \item{predictedSources:}{  If predictSources = T, a data frame with the predicted sources}
#'   \item{predictedTotals:}{  If predictedSources = T, a vector with the number of objects predicted to be from each source}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#' @examples
#' # Analyze the obsidian source data with variables in the model statement in order of
#'  importance from a random forst analysis
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
#'   Model = "Rb"+"Sr"+"Y"+"Zr"+"Nb", ModelTitle = "Sr + Nb + Rb + Y + Zr")
#'
#'  #  Predict the sources of artifacts
#' data(ObsidianSources)
#' data(ObsidianArtifacts)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' save.tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
#'      Model = "Sr"+ "Nb" + "Rb" + "Y"+"Zr", ModelTitle = "Sr + Nb + Rb + Y + Zr",
#'      predictSources=T, predictData=ObsidianArtifacts, ID="ID",
#'      plotTree=F, plotCp=F)
#'
#' @import rpart partykit Formula
#'
#' @export

ps_tree <-
  function(doc = "ps_tree",
           data,
           GroupVar,
           Groups = "All",
           AnalyticVars ,
           wts = NA,
           CpDigits = 3,
           plotTree = T,
           plotCp = T,
           Model,
           ModelTitle,
           minSplit = 20,
           cP = 0.01,
           predictSources = F,
           predictData,
           ID = " ",
           folder = " ")
{
    # create dataset Data.used based on grouping restrict to desired set of groups
    if (Groups[1] != "All") {
      Use.rows <- (data[, GroupVar] %in% Groups)
      Data.used <- data[Use.rows, ]
    } else
      Data.used <- data[, ]
    #
    #  sort source data on GroupVar
    #
    rowsSort <- order(Data.used[,GroupVar])
    Data.used <- Data.used[rowsSort,]
    #
    #  if predictions to be made and ID used, sort on ID
    #
    if ((predictSources == T) & (ID[1] != " "))
      predictData <- predictData[order(predictData[,"ID"]),]
      #
    # define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") & (Groups[1] == "All"))
      groups <-
        as.character(unique(Data.used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    if (is.na(wts))
      Weights = rep(1, nrow(Data.used))
    else
      Weights = wts
    #
    Sources <- factor(Data.used[, GroupVar])
    formula.rhs <-
      paste(AnalyticVars, collapse = "+")  # right hand side of formula
    formula.tree <-
      as.formula(paste("Sources", formula.rhs, sep = " ~ "))
    Tree <-
      rpart(formula.tree,
            data = Data.used,
            weights = Weights,
            method = "class",
            minsplit=minSplit,
            cp=cP)
    if (plotTree == T) {
      plot(as.party(Tree), tp_args = list(id = FALSE), main=paste("model:",ModelTitle))
      browser()
      }
    # classification
    classification <- table(Tree$y, Sources)
    # evaluate tree size
    CpTable <- Tree$cptable
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
      legend(x = "bottomleft", legend = formula.rhs, bty = "n")
     } # end of code for plotCp
    # optimal number of splits
    nsplitopt <- vector(mode = "integer", length = 25)
    for (i in 1:length(nsplitopt)) {
      cp <- Tree$cptable
      nsplitopt[i] <- cp[which.min(cp[, "xerror"]), "nsplit"]
    }
    nsplitopt <- cbind(Model = rep(0, 25), Splits = nsplitopt)
    Nsplitopt <-
      table(Model = nsplitopt[, "Model"], Splits = nsplitopt[, "Splits"])
    #
    if (predictSources == T) {
      predictedSources <- predict(object = Tree, newdata = predictData)
      predictedTotals <- apply(predictedSources,2,sum)
      #
      #  create vector with predicted source for each observation
      source <- rep(" ",nrow(predictedSources))
      for (i in 1:ncol(predictedSources))
        source[predictedSources[,i]==1] <- colnames(predictedSources)[i]
      #
      if (substr(ID,1,1) == " ")  predictedResults<-data.frame(source,predictData[,AnalyticVars])
      if (substr(ID,1,1) != " ")  predictedResults<-data.frame(source, predictData[,c(ID, AnalyticVars)])
      #
      } # end of code for predictSources == T
    #
    nsplit <- CpTable[,"nsplit"]
    Cp <- round(CpTable[,-2],dig = CpDigits)
    CpTable <- cbind(nsplit,Cp)
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    #
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(plotTree, plotCp, predictSources)
    names(params.logical)<-c("plotTree", "plotCp", "predictSources")
    params.splitting <- c(minSplit, cP, CpDigits)
    names(params.splitting) <- c("minSplit","cP", "CpDigits")
    params<-list(grouping=params.grouping,logical=params.logical,splitting=params.splitting)
    #
    nsplit <- CpTable[,"nsplit"]
    Cp <- round(CpTable[,-2],dig = CpDigits)
    CpTable <- cbind(nsplit,Cp)
    #
    if (!predictSources)
      out<-list(usage=fcn.date.ver,
                dataUsed=Data.used,
                analyticVars=AnalyticVars,
                params=params,
                model=ModelTitle,
                Tree = Tree,
                classification = classification,
                CpTable = CpTable,
                location=folder)
    #
    if (predictSources)
        out<-list(usage=fcn.date.ver,
                  dataUsed=Data.used,
                  analyticVars=AnalyticVars,
                  params=params,
                  model=ModelTitle,
                  Tree = Tree,
                  classification = classification,
                  CpTable = CpTable,
                  predictedSources = predictedResults,
                  predictedTotals = data.frame(t(predictedTotals)),
                  location=folder)
   out
  }
