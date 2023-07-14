# we had only rda files in the package, but then JK
# wanted to share data with a student, so we load the
# rda files and write to CSV for easy sharing

myfiles <-
list.files("data",
           full.names =TRUE)
myfiles <- myfiles[ !grepl("test", myfiles) ]

sapply(myfiles,
       load,
       envir=globalenv())

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfs)){
  write.csv(dfs[i],
            paste0("data-raw/", names(dfs)[i], ".csv"))
}

