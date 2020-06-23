
# test validity of ps_tree()

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", ModelTitle="Sr + Nb + Rb + Y + Zr",
                plotTree=FALSE, plotCp=FALSE)

test_that("ps_tree output is a list", {
  expect_type(tree, "list")
})

data(testTree)

test_that("ps_tree Cp table is correct", {
  expect_equal(tree$CpTable, testTree$CpTable)
})

test_that("ps_tree classification is correct", {
  expect_equal(tree$classification, testTree$classification)
})

