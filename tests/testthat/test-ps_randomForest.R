
# test validity of ps_randomForest()

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
data(ObsidianArtifacts)
rf <- ps_randomForest(data=ObsidianSources, GroupVar="Code",Groups="All",
                                       AnalyticVars=analyticVars, sourceID="ID", NvarUsed=3, plotErrorRate=FALSE,
                                       plotImportance=FALSE, predictSources=TRUE, predictData=ObsidianArtifacts, unknownID="ID",
                                       plotSourceProbs=FALSE)

test_that("ps_randomForest output is a list", {
  expect_type(rf, "list")
})

data(test_randomForest)
test_randomForest$predictedSources$source <-
  as.character(test_randomForest$predictedSources$source )

test_that("ps_randomForest$predictedSources is correct", {
   expect_equal(rf$predictedSources,
                test_randomForest$predictedSources)
 })
