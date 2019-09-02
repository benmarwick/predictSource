
# starting with the Vignette, let's look to see if the key functions
# do what they ought to

data(ObsidianSources)
analyticVars<-c("Rb","Sr","Y","Zr","Nb")
tree <- ps_tree(data=ObsidianSources, GroupVar="Code",Groups="All", AnalyticVars=analyticVars,
                Model="Sr"+"Nb"+"Rb"+"Y"+"Zr", ModelTitle="Sr + Nb + Rb + Y + Zr", plotCp=FALSE)

classification_output <- structure(c(15L, 0L, 0L, 0L, 0L, 0L, 16L, 0L, 0L, 0L, 0L, 0L,
                                     28L, 0L, 0L, 0L, 0L, 0L, 15L, 0L, 0L, 0L, 0L, 0L, 39L),
                                   .Dim = c(5L, 5L),
                                   .Dimnames = list(c("1", "2", "3", "4", "5"),
                                                    Sources = c("A", "B", "C", "D", "E")),
                                   class = "table")


test_that("ps_tree output is a list", {
  expect_type(tree, "list")
})

test_that("ps_tree output nrow is correct", {
  expect_equal(nrow(tree$dataUsed), 113)
})

test_that("ps_tree output classification is correct", {
  expect_equal(tree$classification, classification_output)
})

