
# test validity of ps_pcaEvalulation()

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
data(ObsidianArtifacts)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
sources <- unique(ObsidianSources[,"Code"])
pcaEval <- ps_pcaEvaluation(SourceData=ObsidianSources,
   unknownData=ObsidianArtifacts, SourceGroup= "Code", unknownGroup="Code",
   known_sources=sources, predicted_sources=sources, AnalyticVars=analyticVars, ID="ID",
   plotAllPoints=TRUE, plotHullsOutsidePoints = TRUE, plotOutsidePoints = TRUE)

test_that("ps_pcaEvaluation() output is a list", {
  expect_type(pcaEval, "list")
})

#  use saved valid output from the function
data(test_pcaEval)

test_that("ps_pcaEvaluation pointsOutside is correct, first column is character", {
  expect_equal(pcaEval$pointsOutside[,-1], test_pcaEval$pointsOutside[,-1])
})

test_that("ps_pcaEvaluation tableInOut is correct", {
  expect_equal(pcaEval$tableInOut, test_pcaEval$tableInOut)
})
