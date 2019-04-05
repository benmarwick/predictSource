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
