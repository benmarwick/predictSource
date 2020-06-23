
# test validity of ps_pcaGaussian

data(ObsidianSources)
analVars<-c("Rb","Sr","Y","Zr","Nb")
pcaGauss <- ps_pcaGaussian(data=ObsidianSources, GroupVar="Code",Groups=c("A","B"),
              analyticVars=analVars,qqPlot=FALSE)

test_that("ps_pcaGauss output is a list", {
  expect_type(pcaGauss, "list")
})

#  use saved valid output from the function
data(test_pcaGauss)

test_that("ps_pcaGauss pvalues are correct: first column is character", {
  expect_equal(pcaGauss$p_values[,-1], test_pcaGauss$pvalues[,-1])
})


