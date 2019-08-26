#' Chemical composition of obsidian sources
#'
#' A data set containing data on five elements from 113 obsidian samples from five sources in the Jemez
#' caldera in north central New Mexico.  The element presence values (parts per million) were obtained using
#' x-ray fluorescence at a laboratory at the University of California in Berkeley.
#'
#' @format  A data frame with 113 rows and 7 variables:
#' \describe{
#'   \item{Code}{A one-character variable with source code: A, B, C, D, E, F}
#'   \item{ID}{A character variable with an artificial lab ID; the first character
#'   is the value of Code; the remaining characters are 1:n, with n samples from a source}
#'   \item{Rb}{The rubidium value}
#'   \item{Sr}{The strontium value}
#'   \item{Y}{The ytterbium value}
#'   \item{Zr}{The zirconium value}
#'   \item{Nb}{The niobium value}
#'   }
#' @source  Data are from Steve Shackley's website http://www.swxrflab.net/ and from
#'   files provided by Shackley.
#'

"ObsidianSources"
#'
#' Chemical composition of obsidian artifacts
#'
#' A data set containing data on five elements from 91 obsidian artifacts from the Pojoaque Valley
#' in north central New Mexico.  The element presence values (parts per million) were obtained using
#' x-ray fluorescence at Steve Shackley's laboratory in Albuquerque, New Mexico (using a different
#' instrument than the one used to analyze the obsidian source data).
#'
#' @format  A data frame with 91 rows and 7 variables:
#' \describe{
#'   \item{Code}{A one-character variable with a code for the Jemez source identified by Shackely
#'   using a 2- or 3-dimensional scatterplot}
#'   \item{ID}{A character variable with an artificial lab ID; the first character
#'   is A; the remaining characters are 1:91}
#'   \item{Rb}{The rubidium value}
#'   \item{Sr}{The strontium value}
#'   \item{Y}{The ytterbium value}
#'   \item{Zr}{The zirconium value}
#'   \item{Nb}{The niobium value}
#'   }
#' @source  The data are a random sample (30 from each of the three Jemez sources predicted by Shackely, plus
#' the single sample predicted from another source) from a data set on approximately 450 artifacts provided by
#' the state of New Mexico Office of Archaeological Studies (unpublished).
#'

"ObsidianArtifacts"

#'
#' p-values for testing whether the bivariate Rb/Zr distribution is Gaussian
#'  at two Jemez sources.
#'
#' A data set containing p-values from univariate and bivariate tests.  The
#' labels for the rows are the codes for the Jemez sources.
#'
#' @format  A matrix with 2 rows and 6 variables:
#' \describe{
#'   \item{AD.Rb}{A vector with results from the Anderson-Darling test for a
#'   Rb having a univariate Gaussian distribution.}
#'   \item{AD.Zr}{A vector with results from the Anderson-Darling test for a
#'   Zr having a univariate Gaussian distribution.}
#'   \item{SW.Rb}{A vector with results from the Shapiro-Wilk test for a
#'   Rb having a univariate Gaussian distribution.}
#'   \item{SW.Zr}{A vector with results from the Shapiro-Wilk test for a
#'   Zr having a univariate Gaussian distribution.}
#'   \item{Marida.skew}{A vector with results from the Mardia test for a
#'   bivariate Gaussian distribution based on skewness.}
#'   \item{Marida.kurtosis}{A vector with results from the Mardia test for a
#'   bivariate Gaussian distribution based on kurtosis.}
#'   }
#' @source  The matrix was output from the function ps_2dPlotGauss().
#'

"RbZr.pvalues"

#'
#' p-values for testing whether the bivariate Rb/Nb distribution is Gaussian
#'  at two Jemez sources.
#'
#' A data set containing p-values from univariate and bivariate tests.  The
#' labels for the rows are the codes for the Jemez sources.
#'
#' @format  A matrix with 2 rows and 6 variables:
#' \describe{
#'   \item{AD.Rb}{A vector with results from the Anderson-Darling test for a
#'   Rb having a univariate Gaussian distribution.}
#'   \item{AD.Nb}{A vector with results from the Anderson-Darling test for a
#'   Nb having a univariate Gaussian distribution.}
#'   \item{SW.Rb}{A vector with results from the Shapiro-Wilk test for a
#'   Rb having a univariate Gaussian distribution.}
#'   \item{SW.Nb}{A vector with results from the Shapiro-Wilk test for a
#'   Nb having a univariate Gaussian distribution.}
#'   \item{Marida.skew}{A vector with results from the Mardia test for a
#'   bivariate Gaussian distribution based on skewness.}
#'   \item{Marida.kurtosis}{A vector with results from the Mardia test for a
#'   bivariate Gaussian distribution based on kurtosis.}
#'   }
#' @source  The matrix was output from the function ps_2dPlotGauss().
#'

"RbNb.pvalues"

#'
#' Data for artifacts with a predicted Jemez source likely misclassified
#' by a classifiication tree model.
#'
#' A data set containing the predicted source code, lab ID, element values,
#' and first two principal components for these artifacts.
#'
#' @format  A data frame with 11 rows and 9 variables:
#' \describe{
#'   \item{group}{A character with the code for the predicted source.}
#'   \item{ID}{The laboratory ID for the artifact.}
#'   \item{Rb}{A vector with the rubidium values.}
#'   \item{Sr}{A vector with with the strontium values.}
#'   \item{Y}{A vector with the ytterbium values.}
#'   \item{Zr}{A vector with the zirconium values.}
#'   \item{Nb}{A vector with with the niobium values.}
#'   \item{pc1}{A vector with the values of the first principal component
#'   of the principal component plot of the Jemez sources and the artifacts.}
#'   \item{pc2}{A vector with the values of the second principal component
#'   of the principal component plot of the Jemez sources and the artifacts.}
#'   }
#' @source  The data frame was obtained from the function ps_pcaEvaluation()
#' using Identify=TRUE.
#'
"tree.data.check"

#'
#' Data for sources identified as potentially of interest in a plot of
#' zirconium versus rubidium for Jemez source data.
#'
#' @format  A data frame with 5 rows and 7 variables:
#' \describe{
#'   \item{Code}{A character with the code for the source.}
#'   \item{ID}{The laboratory ID for the artifact.}
#'   \item{Rb}{A vector with the rubidium values.}
#'   \item{Sr}{A vector with the strontium values.}
#'   \item{Y}{A vector with the yttrium values.}
#'   \item{Zr}{A vector with the zirconium values.}
#'   \item{Nb}{A vector with with the niobium values.}
#'   }
#' @source  The data frame was obtained from the function ps_2dPlotGauss()
#' using Identify=TRUE.
#'
"sources.data.check"