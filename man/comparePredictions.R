#' comparePredicitons
#'
#'   Compare the predicted source assignmentss from two analysis methods
#'   E.g.  classification trees and random forests
#'
#' @param data1  A data frame containing the predictions from the first analysis
#' @param code1  The code for the predicted assignment in data1
#' @param new_code1  The doce for data1 in the data frame returned
#' @param keep_vars1  The variables in addition to new_code1 to be kept from data1
#' @param data2  A data frame containing the second analysis
#' @param code2  The code for the predicted assignment in data2
#' @param new_code2  The doce for data2 in the data frame returned
#' @param keep_vars2  The variables in addition to new_code1 to be kept from data2
#' @param folder  The path to a folder in which a file with results will be saved
#'                Default value is " " (no file will be saved)
#'
#'@return The function returns a list with the following components:
#'
#' \itemize{
#'   \item{usage}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{data1}{ The value of the parameter data1}
#'   \item{codes1}{ A vector with the values of the parameters code1 and new_code1}
#'   \item{keep_vars1:}{  A vector with the value of the parameters keep_vars1}
#'   \item{data2}{ The value of the parameter data2}
#'   \item{codes2}{ A vector with the values of the parameters code2 and new_code2}
#'   \item{keep_vars2}{  A vector with the value of the parameters keep_vars2}
#'   \item{location}{  The value of the parameter folder}
#'   \item{comparison}{  A data frame with the values of new_code1, new_code2, and the variables kept}
#'
comparePredictions <-function(data1,
                              code1,
                              new_code1,
                              keep_vars1,
                              data2,
                              code2,
                              newCode2,
                              keepVars2,
                              folder = " ") {
  out<-NA
  out
}
