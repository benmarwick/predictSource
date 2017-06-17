#' fn.data

#' create data frame of form to be used for all sources
#' @param path to the folder
#' containing the data
#' @param data = excel file containing the data for the group
#' @param Group = character code for the group
#' @param Subset = codes for subsets of samples, ' ' if no
#' subsets
#' @param ID = character IDs for samples
#' @param Elements = character names for elements used, the names in the excel file
#'
#' @export

fn.data <- function(path, data, Group, Subset, ID, Elements) {

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
      outElements[, i] <- data[, Elements[i]]
  dataout <-
    data.frame(Group = dataCode,
               Subset = dataSubset,
               ID = dataID,
               outElements)
  dataout
}
