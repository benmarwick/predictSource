


fn.2dPlot.Gauss <- function (doc = "fn.bivariate.Gauss", data=ObsidianSources, GroupVar = "Code",labID=" ", Groups = "All",
          AnalyticVars=Els[1:2], QQtest = T, pvalue.digits=3, Identify=F, folder=Folder, ds.pvalues="bivariatePvalues.csv")
{
  #
  #   doc: character variable returned with p values
  #   data: data frame containing character group variable and analytic variables
  #   GroupVar: name of grouping variable
  #   Groups: define groups to be used, can be a vector of character values
  #   AnalyticVars: names of two analytic variables to be considered
  #   QQtest: if true (default), use bootstrapping to show plots evaluating the qqlines for the variables in each group
  #   pvalue.digits: number of significant digits for p-values to be returned
  #   folder: character value, path to folder containing excel file with p-values, ending in \\
  #     if " ", no file is written
  #   ds.pvalues: character value with name of file, extension .csv
  #
  if (length(AnalyticVars)!=2)  stop("length of AnalyticVars must be 2")
  #
  if (Groups[1] != "All") {
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows, ]
  }
  else data.Used <- data
  #
  if (Groups[1] == "All")
    groups <- as.character(unique(data.Used[, GroupVar]))
  else groups <- as.character(Groups)
  #
  GroupIndex <- rep(NA, nrow(data.Used))
  for (i in 1:nrow(data.Used)) {
    for (j in 1:length(groups)) if (data.Used[i, GroupVar] ==
                                    groups[j])
      GroupIndex[i] <- j
  }
  pvalues <- matrix(NA, length(groups), 6)
  dimnames(pvalues) <- list(groups, c(paste("AD.",AnalyticVars[1],sep=""), paste("AD.",AnalyticVars[2],sep=""),
                                      paste("SW.",AnalyticVars[1],sep=""),paste("SW.",AnalyticVars[2],sep=""), "Mardia.skew", "Mardia.kurtosis"))
  n.pages <- round(0.01+length(groups)/2, dig = 0)  # number of pages (rounds up with an odd number of groups)
  #
  if (Identify) data.check<-data.Used[1,]  # set up data frame to store identified points
  i.group <- 0
  fn.plot <- function() {
    temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
    temp1 <- temp[, AnalyticVars[1]]
    qqnorm.pts<-qqnorm(temp1, main = paste(AnalyticVars[1],"source", groups[i.group]))
    qqline(temp1)
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
    }
    temp2 <- temp[, AnalyticVars[2]]
    qqnorm.pts<-qqnorm(temp2, main = paste(AnalyticVars[2],"source", groups[i.group]))
    qqline(temp2)
    if (Identify) {
      index<-identify(qqnorm.pts)
      data.grp<-data.Used[data.Used[,GroupVar]==groups[i.group],]
      data.check<<-rbind(data.check,data.grp[index,])
    }
    ADp1 <- ad.test(temp1)$p.value
    ADp2 <- ad.test(temp2)$p.value
    SWp1 <- uniNorm(temp1, type = "SW")[[2]]$p.value
    SWp2 <- uniNorm(temp2, type = "SW")[[2]]$p.value
    mardia <- mardiaTest(data = temp)
    if (nrow(temp) >= 20)
      p.kurtosis <- mardia@p.value.kurt
    else p.kurtosis <- mardia@p.value.small
    c(ADp1, ADp2, SWp1, SWp2, mardia@p.value.skew, p.kurtosis)
  }
  for (page in 1:n.pages) {
    plot.new()
    par(mfrow = c(2, 2))
    i.group <- i.group + 1
    pvalues[i.group, ] <- fn.plot()
    i.group <- i.group + 1
    if (i.group <= length(groups))
      pvalues[i.group, ] <- fn.plot()
    browser()
  }
  #    fn.Mardia.plot <- function() {
  #        temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
  #        mardia <- mardiaTest(data = temp)
  #        mvnPlot(mardia, type = "persp")
  #        mvnPlot(mardia, type = "contour")
  #    }
  #    i.group <- 0
  #   for (page in 1:n.pages) {
  #        plot.new()
  #        par(mfrow = c(2, 2))
  #        i.group <- i.group + 1
  #        fn.Mardia.plot()
  #        i.group <- i.group + 1
  #        if (i.group <= length(groups))
  #            fn.Mardia.plot()
  #        browser()
  #    }
  if (QQtest) {
    fn.qqtest <- function() {
      temp <- data.Used[data.Used[, GroupVar] == groups[i.group],AnalyticVars[1:2]]
      temp1 <- temp[,AnalyticVars[1]]
      qqtest(data = temp1, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[1],"source", groups[i.group]))
      temp2 <- temp[,AnalyticVars[2]]
      qqtest(data = temp2, dist = "normal", drawPercentiles = T,
             main = paste(AnalyticVars[2],"source", groups[i.group]))
    }
    i.group <- 0
    for (page in 1:n.pages) {
      plot.new()
      par(mfrow = c(2, 2))
      i.group <- i.group + 1
      fn.qqtest()
      i.group <- i.group + 1
      if (i.group <= length(groups))
        fn.qqtest()
      browser()
    }
  }
  pvalues<-round(pvalues,dig=pvalue.digits)
  #
  if (substr(folder,1,1) != " ")
    if (substr(ds.pvalues,1,1) != " ") write.csv(pvalues, file = paste(folder, ds.pvalues, sep = ""))
  #
  #  remove duplicated observations from data.check
  #
  if (Identify) {
    data.check<-data.check[-1,]  # remove dummy first row
    if (labID != " ") index<-duplicated(data.check[,labID])
    else  index<-duplicated(data.check[,c(GroupVar,AnalyticVars)])
    if (length(index) > 0)  data.check<-data.check[!index,]
    if (labID != " ") {
      index.ID<-order(data.check[,labID])
      data.check<-data.check[index.ID,]
    }
  }
  #
  fcn.date.ver<-c(doc,date(),R.Version())
  parameters<-c(groupVar=GroupVar,groups=Groups,digits.pvalue=pvalue.digits,qqtest=QQtest)
  if ((substr(folder,1,1) == " ") & (!Identify)) out<-list(fcn.date.ver=fcn.date.ver,data.Used=data.Used,analyticVars=AnalyticVars,pvalues=pvalues)
  if ((substr(folder,1,1) == " ") & ( Identify)) out<-list(fcn.date.ver=fcn.date.ver,data.Used=data.Used,analyticVars=AnalyticVars,pvalues=pvalues,data.check=data.check)
  else  {
    file<-list(corr=paste(folder,ds.pvalues,sep=""))
    if (!Identify) out<-list(fcn.date.ver=fcn.date.ver,data.Used=data.Used,analyticVars=AnalyticVars,pvalues=pvalues,file=file)
    if ( Identify) out<-list(fcn.date.ver=fcn.date.ver,data.Used=data.Used,analyticVars=AnalyticVars,pvalues=pvalues,data.check=data.check,file=file)
  }
  out
}
