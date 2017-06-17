
# Notes on preparation

# open the RData file, then create plain text file of the contents:

# load('../functions2017Feb27.Rdata')

# get vector of fn names fns <- ls()

# write each fn out as a .R file
# sapply(fns, function(i) dump(i, file = paste('R/',
# i, '.R')))


# get data in, and deposit in appropriate location
# load('../ObsidianData.Rdata')
# devtools::use_data(ObsidianData)
