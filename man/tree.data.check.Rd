\name{tree.data.check}
\alias{tree.data.check}
\docType{data}
\title{
tree.data.check
}
\description{
A data frame with points of interest on a principal component plot with
convex hulls of the Jemez source data and predictions from a classification
tree analysis.
}
\usage{data("tree.data.check")}
\format{
  A data frame with 11 observations on the following 9 variables.
  \describe{
    \item{\code{group}}{A character vector with predicted Jemez source codes}
    \item{\code{ID}}{A character vector with artificial lab IDs}
    \item{\code{Rb}}{A numeric vector with rubidium values}
    \item{\code{Sr}}{A numeric vector with strontium values}
    \item{\code{Y}}{A numeric vector with ytterbium values}
    \item{\code{Zr}}{A numeric vector with zirconium values}
    \item{\code{Nb}}{A numeric vector with niobium values}
    \item{\code{pc1}}{A numeric vector with values of the first principal component}
    \item{\code{pc2}}{A numeric vector with values of the second principal component}
  }
}
\source{
Obtained from the function ps_pcaEvaluation() with Identify=TRUE
}
\examples{
data(tree.data.check)
tree.data.check
}
\keyword{datasets}
"tree.data.check"
