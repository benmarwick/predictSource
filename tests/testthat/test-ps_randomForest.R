
# test validity of ps_randomForest()

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
rf <- ps_randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
        sourceID="ID", AnalyticVars=analyticVars, NvarUsed=3, plotSourceProbs=FALSE)


test_that("ps_randomForest output is a list", {
  expect_type(rf, "list")
})

data(testRandomForest)

test_that("ps_randomForest confusion matrix is correct", {
  expect_equal(rf$confusion, testRandomForest$confusion)
})

test_that("ps_randomForest importance estimates are correct", {
  expect_equal(rf$importance, testRandomForest$importance)
})

