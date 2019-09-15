
# test validity of ps_CV_corr

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
CV_corr<-ps_CV_corr(data = ObsidianSources, GroupVar="Code", Groups = "All",
        AnalyticVars=analyticVars, ByGroup=FALSE)

test_that("ps_CV_corr output is a list", {
  expect_type(CV_corr, "list")
})

#  use saved valid output from the function
data(testCV_corr)

test_that("ps_CV_corr correlations are correct", {
  expect_equal(CV_corr$corr, testCV_corr$corr)
})


