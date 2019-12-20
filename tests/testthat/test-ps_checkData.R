
# test validity of ps_checkData

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
checkData<-ps_checkData(data=ObsidianSources,CheckDupVars=analyticVars,GroupVar="Code",Groups="All",
       ID = "ID", AnalyticVars=analyticVars)

test_that("ps_checkData output is a list", {
  expect_type(checkData, "list")
})

#  use saved valid output from the function
 data(testCheckData)

test_that("ps_checkData statistics are correct for one element", {
   expect_equal(checkData$statistics[1:5,-(1:2)], testCheckData$statistics[1:5,-(1:2)])
})


