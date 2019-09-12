
# test validity of ps_pca()

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
pca <- ps_pca(data=ObsidianSources, ID="ID", GroupVar="Code",
                   + Groups="All", AnalyticVars=analyticVars)

test_that("ps_pca output is a list", {
  expect_type(pca, "list")
})

#  use saved valid output from the function
data(test_pca)

test_that("ps_pca Predicted pvalues are correct: first column is character", {
  expect_equal(pca$Predicted[,-1], test_pca$Predicted[,-1])
})


