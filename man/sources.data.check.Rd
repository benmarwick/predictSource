\name{sources.data.check}
\alias{sources.data.check}
\docType{data}
\title{
sources.data.check
}
\description{
A data frame with points of interest on a plot of zirconium versus rubidium
}
\usage{data("sources.data.check")}
\format{
  A data frame with 5 observations on the following 7 variables.
  \describe{
    \item{\code{Code}}{A character vector with Jemez source codes}
    \item{\code{ID}}{A character vector with artificial lab IDs}
    \item{\code{Rb}}{A numeric vector with rubidium values}
    \item{\code{Sr}}{A numeric vector with strontium values}
    \item{\code{Y}}{A numeric vector with ytterbium values}
    \item{\code{Zr}}{A numeric vector with zirconium values}
    \item{\code{Nb}}{A numeric vector with niobium values}
  }
}
\source{
Obtained from the function ps_2dPlotGauss() with Identify=TRUE
}
\examples{
data(sources.data.check)
sources.data.check
}
\keyword{datasets}
"sources.data.check"
