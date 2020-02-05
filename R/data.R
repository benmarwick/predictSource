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

#' A list used to verify that ps_checkData() is valid
#'
#' @format  A list with the following elements
#' \describe{
#' \item{usage:}{  A string with the contents of the argument doc, date run, R version used}
#' \item{dataUsed:}{  The data frame specified by the argument data and GroupVar}
#' \item{params:}{  A character vector with the values of CheckDupVars, GroupVar, and Groups}
#' \item{analyticVars:}{  The vector of names specified by the argument AnalyticVars}
#' \item{Duplicates:}{  A data frame containing the observations with duplicate values}
#' \item{NegativeValues:}{  A data frame containing the observations with at least one negative
#'  value for a variable in AnalyticVars}
#' \item{Nvalues:}{  A data frame contain the number of observations with a value for each analytic variable}
#' \item{statistics:}{  A data frame containing the statistics statistics (by group, if Group is specified)}
#' \item{location:}{  The value of the parameter folder}
#' }
#'
 "testCheckData"

#' A list used to verify that ps_2dPlotGauss() is valid
#'
#' @format  A list with the following elements
#' \describe{
#' \item{usage:}{  String with the contents of the argument doc, the date run, the version of R used}
#' \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#' \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#' \item{analyticVars:}{  A vector with the contents of the argument analyticVars}
#' \item{variablePair:}{  A vector with the contents of the argument variablePair}
#' \item{params:}{  A list with the values of grouping, logical, and numberic arguments}
#' \item{pvalues:}{  A data frame with the p-values for univariate and bivariate tests of normality}
#' \item{dataCheck:}{ If Identify=TRUE, a data frame with the information on user-identified points of interest}
#' \item{location:}{ The value of the parameter folder}
#' }
#'
"test2dPlotGauss"

#' A list used to verify that ps_CV_corr() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{  A list containing the values of the grouping,logical, and numeric parameters}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{CV:}{  A data frame with the coefficients of variation for each analytic variable in each group}
#'   \item{corr:}{  A data frame with the correlations between pairs of variables in each group}
#'   \item{location:}{  The value of the parameter folder}
#'       }
#'
"testCV_corr"

#' A list used to verify that ps_pca() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{  A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{  The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{  A list with the values of the arguments for grouping, logical parameters,
#'   Ellipses, and Colors}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{ellipse_pct:}{  The value of the argument Ellipses}
#'   \item{variances:}{  A data frame including the percent of variation explained by each
#'   principal component and the cumulative percent explained}
#'   \item{weights:}{  A data frame with the principal component weights for each observation}
#'   \item{Predicted:}{  A data frame with the predicted values for each principal component,
#'    plus the value of Groups and an integer GroupIndex (with values 1:number of Groups)}
#'   \item{DataPlusPredicted:}{  A data frame with the data used to compute the principal components,
#'    plus GroupIndex (as defined above) and predicted values for each principal component}
#'   \item{dataCheck:}{  If Identify=TRUE, a data frame with the observations in dataUsed
#'    identified as of interest}
#'   \item{location:}{  The value of the parameter folder}
#'  }
#'
"test_pca"

#' A list used to verify that ps_pcaEvaluation() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{  A vector with the contents of the argument doc, the date run,
#'   the version of R used}
#'   \item{sourceData:}{  The contents of the argument SourceData restricted to knownSources}
#'   \item{sourcesNA:}{ A data frame with source observations with missing data for analytic
#'   variables; NA if no missing data}
#'   \item{unknownData:}{  The contents of the argument unknownData restricted to predictedSources}
#'   \item{unknownsNA:}{ A data frame with unknown observations with missing data for analytic
#'   variables; NA if no missing data}
#'   \item{impError}{  Normalized root mean square error estimate for imputed data;
#'   NA if no missing data}
#'   \item{params:}{  A list with the values of the grouping and source arguments and
#'   values of the logical arguments}
#'   \item{analyticVars:}{  A vector with the value of the argument AnalyticVars}
#'   \item{tableInOut:}{  A data frame with counts of the numbers of unknowns inside and
#'   outside of each predicted source location}
#'   \item{ptsOutside:}{  A data frame with the data for unknown points located outside of the
#'    predicted source}
#'   \item{dataCheck:}{If Identify=TRUE, a data frame with the observations in dataUsed identified
#'    as of interest; value is c(NA,NA) if no points are identified}
#'   \item{location:}{The value of the parameter folder}
#'    }
#'
"test_pcaEval"

#' A list used to verify that ps_pcaGauss() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{ A vector with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{dataNA:}{  A data frame with observations containing a least one missing value
#'   for an analysis variable, NA if no missing values}
#'   \item{params:}{ A list with the values of the arguments grouping and logical arguments}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{pvalues:}{ A data frame with the p-values for the Gaussian assumptions for each
#'    group specified}
#'  \item{data_check:}{  A data frame with data identified as generating points of interest;
#'  value is NA if no points are identified}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
"test_pcaGauss"

#' A list used to verify that ps_randomForest() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{ A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{sourcesNA:}{  A data frame with data from the data frame data with missing values,
#'    NÁ if no missing values}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params:}{ A list with the values of the grouping, logical, and numeric arguments}
#'   \item{formulaRf:}{The formula used in the analysis (the variables specified in the argument AnalyticVars
#'    separated by + signs)}
#'   \item{forest:}{  A summary of the random forest call, estimated error rate, and
#'   confusion matrix}
#'   \item{importance:}{  A data frame with information on the importance of each variable
#'    in AnalyticVars}
#'   \item{confusion:}{A data frame with the estimate of the confusion matrix}
#'   \item{predictedData:}{  A data frame with the artifact data used for predictions; if there
#'   is missing data, after imputation of the missing data}
#'   \item{predictedNA:}{ A data frame with the observations for which missing data were imputed;
#'   NA if there are no missing data}
#'   \item{predictedSources:}{  A data frame with prediction information, sample ID (if requested),
#'    and values of AnalyticVars}
#'   \item{predictedTotals:}{  A vector with the predicted totals for each group (source)}
#'   \item{impError:}{ The estimated OOB (out of bag) error for imputed predictor data;
#'   NA if no imputed data}
#'   \item{location:}{ The value of the parameter folder}
#'  }
#'
#"testRandomForest"
NULL

#' A list used to verify that ps_tree() is valid
#'
#' @format  A list with the following elements
#' \describe{
#'   \item{usage:}{ A string with the contents of the argument doc, the date run, the version of R used}
#'   \item{dataUsed:}{ The contents of the argument data restricted to the groups used}
#'   \item{params_grouping:}{ A list with the values of the arguments GroupVar and Groups}
#'   \item{analyticVars:}{ A vector with the value of the argument AnalyticVars}
#'   \item{params:}{ A list with the values of the grouping, logical, and splitting parameters}
#'   \item{model:}{ A character string with the value of the argument ModelTitle}
#'   \item{Tree:}{ A list with details of the tree construction_}
#'   \item{classification:}{A data frame showing the crossclassification of sources and predicted sources}
#'   \item{CpTable:}{  A data frame showing the decrease in Cp with increasing numbers of splits}
#'   \item{predictedSources:}{  If predictSources = T, a data frame with the predicted sources}
#'   \item{predictedTotals:}{  If predictedSources = T, a vector with the number of objects predicted to be from each source}
#'   \item{location:}{ The value of the parameter folder}
#'  }
"testTree"

#' A dataset...
#'


"ObsidianData"
