
# test validity of ps_2dPlotGauss

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
twoDPlotGauss<-ps_2dPlotGauss(data=ObsidianSources, GroupVar="Code", ID="ID", Groups=c("A","B"),
       AnalyticVars= analyticVars, variablePair=c("Rb","Zr"))

test_that("ps_2dPlotGauss output is a list", {
  expect_type(twoDPlotGauss, "list")
})

#  use saved valid output from the function
data(test2dPlotGauss)

test_that("ps_2dPlotGauss pvalues are correct: first column is character", {
  expect_equal(twoDPlotGauss$pvalues[,-1], test2dPlotGauss$pvalues[,-1])
})


