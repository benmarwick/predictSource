#' fn.data
#'
#' @param fn.data create data frame with data to be analyzed, columns in specified order
#' @param data data frame containing the data for one group
#' @param Group character: code for the group, added to the data set
#' @param Subset character: name of variable with codes for subsets of samples, " " if no subsets
#' @param ID character: name of IDs (typically lab IDs) for samples
#' @param Elements character vector: names for elements used from the data
#'
#' @return A data frame with columns
#'           Group: group code (specified in Group)
#'           Subset: subset code (specified in Subset)
#'           ID: ID value
#'           elements: one column for each element specified in Elements
#'
#' @section Details:
#' This function creates a data frame with columns in a specified order.  It is useful
#' for combining multiple data frames into one analysis object using rbind().  The name
#' for a specified variable must be the same in each file used in the argument data; a
#' space after a variable name in an excel file imported into R yields the variable name
#' followed by a period.
#'
#' @examples
#' load(ObsidianData)
#' # create example data set by restricting to one group, removing column with group code,
#' # then adding that code back to the data set
#' fn.data(data = ObsidianData[ObsidianData[,"Code"] == "AW"][,-1]
#'           Group = "AW",
#'           Subset = " ",
#'           ID = "ID"
#'           Elements = c("Rb","Sr","Y","Zr","Nb"))
#'
#' @export

function(data,Group,Subset,ID,Elements){
  dataCode <- rep(Group, nrow(data))
  dataSubset <- rep(Subset, nrow(data))
  dataID <- data[, ID]
  #
  sourceElements <-
    dimnames(data)[[2]][-1]  #  elements in data matrix
  ElementsUsed <-
    Elements %in% sourceElements  # logical vector of length(Elements)
  # for whether ith value is in data matrix
  outElements <-
    matrix(nrow = nrow(data), ncol = length(Elements), NA)
  dimnames(outElements)[[2]] <- Elements
  for (i in 1:length(Elements))
    if (ElementsUsed[i] == T)
      elements[, i] <- data[, Elements[i]]
  dataout <-
    data.frame(Group = dataCode,
               Subset = dataSubset,
               ID = dataID,
               elements)
  dataout
}
