#' fn.tree
#'
#' Fit a recursive partitioning model
#'
#' @param doc Documentation, default is fn.tree (the function name)
#' @param data  Data frame with the data to be analyzed
#' @param GroupVar  Name of the variable defining groups, grouping is required
#' @param Groups  Vector of codes for groups to be used, 'All' (the default) if use all groups
#' @param AnalyticVars  Names of analytic variables
#' @param wts Option to weight the observations, if used, vector with length nrow(data); if NA (the default), assume equal weights
#' @param plotTree If T (true, the default), plot the recursive partitioning tree
#' @param plotCp  If T (tree, the default), plot the Cp table
#' @param Model  A character string containing the variables considered separated by + signs
#' @param folder  If " ", no files are written; otherwise, the path to the folder containing the excel files,
#'                must end with two forward slashes
#' @param ds.Classify  Name of the excel file containing the results of classifying the data, must end with .csv
#' @param ds.CpTable Name of the excel file containing the values of Cp at successive splits, must end with .csv
#'
#' @return The function fits a classification tree model.  The variables in AnalyticVars are considered in the order
#'        they appear in the Model argument (from left to right).  See the vignette for more details.
#'        The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage:}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{params.grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params.logical:}{ The value of plotTree}
#'   \item{model:}{ A character string with the value of the argument Model}
#'   \item{classification:}  {A data frame showing the crossclassification of sources and predicted sources}
#'   \item{CpTable:}{  A data frame showing the decrease in Cp with increasing numbers of splits}
#'   \item{files:}{ If folder != " ", a character string with the path to the file containing the excel files
#'    defined is ds.Classify and ds.CpTable.}
#'  }
#'
#' @examples
#' data(ObsidianSources)
#' analyticVars<-c("Rb","Sr","Y","Zr","Nb")
#' tree <- fn.tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
#'                 Model="Sr"+"Nb"+"Rb"+"Y"+"Zr")
#'  # variables in the model statement in order of importance from a random forst analysis
#'
#' @import rpart partykit Formula
#'
#' @export

fn.tree <-
  function(doc = "fn.tree",
           data,
           GroupVar,
           Groups = "All",
           AnalyticVars ,
           wts = NA,
           plotTree = T,
           plotCp = T,
           Model,
           folder = " ",
           ds.Classify,
           ds.CpTable,
           ds.CVtable,
           ds.OptSplit) {


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
            method = "class")
    if (plotTree == T)
      plot(as.party(Tree), tp_args = list(id = FALSE))
    browser()
    # classification
    classification <- table(Tree$y, Sources)
    if (folder != " ") write.csv(t(classification), paste(folder, ds.Classify, sep = ""))
    # evaluate tree size
    CpTable <- Tree$cptable
    if (folder != " ")  write.csv(CpTable, paste(folder, ds.CpTable, sep = ""))
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
    Nsplitopt <-
      table(Model = nsplitopt[, "Model"], Splits = nsplitopt[, "Splits"])
    if (folder != " ")  write.csv(table(Nsplitopt), paste(folder, ds.OptSplit, sep = ""))
    #
    fcn.date.ver<-paste(doc,date(),R.Version()$version.string)
    params.grouping<-list(GroupVar,Groups)
    names(params.grouping)<-c("GroupVar","Groups")
    params.logical<-c(plotTree, plotCp)
    names(params.logical)<-c("plotTree", "plotCp")
    if (folder != " ")
      fileNames <- list(paste(folder,ds.Classify,sep=""),paste(folder,ds.CpTable,sep=""),
                        paste(folder,ds.CVtable,sep=""),paste(folder,ds.OptSplit,sep=""))
    #
    if (substr(folder,1,1) == " ")
      out<-list(usage=fcn.date.ver,
                dataUsed=Data.used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                params.logical=params.logical,
                Tree = Tree,
                classification = classification,
                CpTable = CpTable
                )
    if (substr(folder,1,1) != " ")
      out<-list(usage=fcn.date.ver,
                dataUsed=Data.used,
                analyticVars=AnalyticVars,
                params.grouping=params.grouping,
                params.logical=params.logical,
                Tree = Tree,
                classification = classification,
                CpTable = CpTable,
                files = fileNames
      )
    out
  }
