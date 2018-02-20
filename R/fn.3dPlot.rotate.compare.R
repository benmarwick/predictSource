#' fn.3dPlot.rotate.compare
#'
#' create 3-dimensional data plot(s) comparing groups that
#' can be rotated data: R matrix or data frame containing the data to be analyzed
#'
#'
#' @param GroupVar: name for variable defining grouping, ' ' if no grouping
#' @param Groups: vector of values of group variable for which plots are to be done
#' @param AnalyticVars: vector of names (character values) of analytic results
#' @param Selections: vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param Color: color of plotted points
#'
#' @import rgl MASS
#'
#' @export

fn.3dPlot.rotate.compare <-
  function(data ,
           GroupVar,
           Groups ,
           AnalyticVars,
           Selections,
           Colors) {


    # create dataset data.Used based on grouping
    Use.rows <- (data[, GroupVar] %in% Groups)
    data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    # define variable groups as groups used in analysis
    groups <- as.character(unique(data.Used[, GroupVar]))
    # add index for group number to data.Used
    group.index <- rep(NA, nrow(data.Used))
    for (i in 1:nrow(data.Used)) {
      for (j in 1:length(groups))
        if (data.Used[i, GroupVar] == groups[j])
          group.index[i] <- j
    }
    data.Used <- data.frame(index = group.index, data.Used)
    # define subtitle showing colors
    subtitle <- paste(Groups[1], ": ", Colors[1], sep = "")
    for (i in 2:length(Groups))
      subtitle <- paste(subtitle, "  ", Groups[i], ": ", Colors[i],
                        sep = "")
    # one set of variables
    if (is.vector(Selections)) {
      plot3d(
        data.Used[, Selections[1]],
        data.Used[, Selections[2]],
        data.Used[, Selections[3]],
        xlab = Selections[1],
        ylab = Selections[2],
        zlab = Selections[3],
        col = Colors[group.index],
        pch = 16,
        sub = subtitle
      )
    }
    if (is.matrix(Selections) | is.data.frame(Selections)) {
      # plot for each set of variables and each group
      for (i in 1:nrow(Selections)) {
        win.graph()  # open new graph window
        plot3d(
          data.Used[, Selections[i, 1]],
          data.Used[, Selections[i, 2]],
          data.Used[,
                    Selections[i, 3]],
          xlab = Selections[i, 1],
          ylab = Selections[i, 2],
          zlab = Selections[i, 3],
          col = Colors[group.index],
          pch = 16,
          sub = subtitle
        )
        browser()  # pause to allow saving plot
      }
    }
    invisible()
  }
