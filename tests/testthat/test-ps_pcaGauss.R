
# test validity of ps_pcaGauss

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
pcaGauss <- ps_pcaGauss(data=ObsidianSources, GroupVar="Code",Groups=c("A","B"),
              AnalyticVars=analyticVars)

test_that("ps_pcaGauss output is a list", {
  expect_type(pcaGauss, "list")
})

#  use saved valid output from the function
data(test_pcaGauss)

test_that("ps_pcaGauss pvalues are correct: first column is character", {
  expect_equal(pcaGauss$pvalues[,-1], test+pcaGauss$pvalues[,-1])
})


