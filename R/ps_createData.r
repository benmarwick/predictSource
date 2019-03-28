#' ps_createData
#'
#'  Create data frame with data to be analyzed, columns in specified order
#'
#' @param doc Documentation, default is the function name
#' @param data Data frame containing the data for one group
#' @param Group Character: user-specified grouping code for these data, added to the data set
#' @param Subset Character: name of variable with codes for subsets of samples, " " if no subsets
#' @param ID Character: name of IDs (typically lab IDs) for samples
#' @param AnalyticVars Character vector with names for elements used from the data
#'
#' @return A list with the elements:
#'  \itemize{
#' \item{usage:}{  Documentatation, including function name, version of R used, date run)}
#' \item{dataUsed:}{  Data used, restricted to the specified group}
#' \item{analyticVars:}{  The value of AnalyticVars}
#' \item{dataOut:}{  The data frame created by the function, with Group, Subset (if specified), ID, and analytic variables}
#'}
#' @section Details:
#' This function creates a data frame with columns in a specified order_  It is useful
#' for combining multiple data frames into one analysis object using rbind()_  The name
#' for a specified variable must be the same in each file used in the argument data; a
#' space after a variable name in an excel file imported into R yields the variable name
#' followed by a period_
#'
#' @examples
#' # create example data set by restricting to one group, removing column with group code,
#' # then adding that code back to the data set
#' data(ObsidianSources)
#' ps_createData(data = ObsidianSources[(ObsidianSources[,"Code"] == "A"),][,-1],
#'           Group = "A",
#'           Subset = " ",
#'           ID = "ID",
#'           AnalyticVars = c("Rb","Sr","Y","Zr","Nb"))
#'
#' @export

ps_createData <- function(doc="ps_createData", data, Group,Subset,ID,AnalyticVars){
  dataCode <- rep(Group, nrow(data))  # create a vector with the specified code
  #
  if (length(Subset) == nrow(data))  dataSubset <- Subset  # use specified subsets
  if (length(Subset) == 1)  dataSubset<-rep(Subset, nrow(data))  # create subset variable with same value for all obserations
  #
  dataID <- data[, ID]  # create a vector with the lab IDs
  #
  #sourceElements <-
  #  dimnames(data)[[2]][-1]  #  elements in data matrix
  #ElementsUsed <-
  #  Elements %in% sourceElements  # logical vector of length(Elements)
  # for whether ith value is in data matrix
  variables <- data[,AnalyticVars]
  dimnames(variables)[[2]] <- AnalyticVars
  #
  if (Subset[1] != " ")  dataOut<-data.frame(Group=Group, Subset=Subset, ID=dataID, variables)
    else  dataOut<-data_frame(Group=Group, ID=dataID, variables)
  fcnDateVersion<-paste(doc,date(),R.Version()$version.string)
  #
  list(usage=fcnDateVersion,
              analyticVars=AnalyticVars,
              dataOut=dataOut)
}
