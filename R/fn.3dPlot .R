#' fn.3dPlot
#'
#' create 3-dimensional data plot
#' The function stops after producing each plot.  Enter c ("continue") at the prompt to
#'   get the next plot.  If this function is run using Rstudio, each plot appears in a separate window,
#'   not in the Rstudio plot pane.
#'
#' @param data R matrix or data frame containing the data to be analyzed
#' @param GroupVar name for variable defining grouping, " " if no grouping
#' @param Groups vector of values of group variable for which plots are to be done "All" use all groups " " no grouping
#' @param AnalyticVars vector of names (character values) of analytic results
#' @param Selections vector of length 3, or data frame with 3 columns, with combinations to be plotted
#' @param ByGroup if T, show scatterplot for each group for each selection of 3 variables
#' @param Color color of plotted points
#' @param SymbolSize value at most 1, smaller value gives smaller diameter points
#'
#'   DETAILS
#'

#'
#'   @return   A list with the following components:
#'   fcn.date.ver: a vector with the contents of the argument doc, the date run, the version of R used
#'   dataUsed: the contents of the argument data restricted to the groups used
#'   params: a vector with the values of the arguments ByGroup and SymbolSize
#'   groups: a vector (may be of length 1) with the value of the argument Groups
#'   analyticVars: a vector with the value of the argument AnalyticVars
#'   colors: a vector with the value of the argument Colors
#'
#' @examples
#' data(ObsidianData)
#' fn.3dPlot(data = ObsidianData,
#'           Groups = " ",
#'           GroupVar = " ",
#'           ByGroup = FALSE,
#'           Selections = c(4,5,6))
#'
#'
#' @import MASS scatterplot3d
#'
#' @export

fn.3dPlot <-
  function(data,
           GroupVar,
           Groups,
           AnalyticVars,
           Selections,
           ByGroup = FALSE,
           Color = "red",
           SymbolSize = 0.7) {

    #
    #  create dataset data.Used based on grouping
    #
    if ((Groups[1] != " ") &
        (Groups[1] != "All")) {
      # restrict to desired set of groups
      Use.rows <- (data[, GroupVar] %in% Groups)
      data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
    }
    else  if (GroupVar[1] == " ")
      data.Used <- data[, AnalyticVars]
    else
      data.Used <- data[, c(GroupVar, AnalyticVars)]
    #
    #  define variable groups as groups used in analysis
    if ((GroupVar[1] != " ") &
        (Groups[1] == "All"))
      groups <- as.character(unique(data.Used[, GroupVar]))
    else if (GroupVar[1] != " ")
      groups <- as.character(Groups)
    #
    if ((GroupVar[1] == " ") | (ByGroup != T)) {
      # no grouping
      if (is.vector(Selections))
        # single plot
        scatterplot3d(
          data.Used[, Selections[1]],
          data.Used[, Selections[2]],
          data.Used[, Selections[3]],
          xlab = Selections[1],
          ylab = Selections[2],
          zlab = Selections[3],
          color = Color,
          pch = 16,
          cex.symbols = SymbolSize,
          main = paste(Selections[1], ",", Selections[2], ",", Selections[3])
        )
      if (is.matrix(Selections)) {
        # plot for each group of elements
        for (i in 1:nrow(Selections)) {
          win.graph()  # open new graph window
          scatterplot3d(
            data.Used[, Selections[i, 1]],
            data.Used[, Selections[i, 2]],
            data.Used[, Selections[i, 3]],
            xlab = Selections[i, 1],
            ylab = Selections[i, 2],
            zlab = Selections[i, 3],
            color = Color,
            pch = 16,
            cex.symbols = SymbolSize,
            main = paste(Selections[i, 1], ",", Selections[i, 2], ",", Selections[i, 3])
          )
          browser()  # pause to allow saving plot
        }
      }
    }
    #
    if ((GroupVar != " ") & (ByGroup == T)) {
      # show scatterplots by group
      if (Groups[1] != "All") {
        #  restrict to desired set of groups
        Use.rows <- (data[, GroupVar] %in% Groups)
        data.Used <- data[Use.rows, c(GroupVar, AnalyticVars)]
      }
      else
        data.Used <- data[, c(GroupVar, AnalyticVars)]
      #
      groups <-
        unique(data.Used[, GroupVar])  # codes for groups to be used
      if (is.vector(Selections))  {
        #  one set of variables
        for (i in 1:length(groups)) {
          data.i <- data.Used[data.Used[, GroupVar] == groups[i], ]
          scatterplot3d(
            data.i[, Selections[1]],
            data.i[, Selections[2]],
            data.i[, Selections[3]],
            xlab = Selections[1],
            ylab = Selections[2],
            zlab = Selections[3],
            color = Color,
            pch = 16,
            cex.symbols = SymbolSize,
            main = paste(
              groups[i],
              ":",
              Selections[1],
              ",",
              Selections[2],
              ",",
              Selections[3]
            )
          )
          browser()
        }
      }
      if (is.matrix(Selections) |
          is.data.frame(Selections)) {
        # plot for each set of variables and each group
        for (i in 1:nrow(Selections)) {
          for (j in 1:length(groups)) {
            data.j <- data.Used[data.Used[, GroupVar] == groups[j], ]
            win.graph()  # open new graph window
            scatterplot3d(
              data.j[, Selections[i, 1]],
              data.j[, Selections[i, 2]],
              data.j[, Selections[i, 3]],
              xlab = Selections[i, 1],
              ylab = Selections[i, 2],
              zlab = Selections[i, 3],
              color = Color,
              pch = 16,
              cex.symbols = SymbolSize,
              main = paste(
                groups[j],
                ":",
                Selections[i, 1],
                ",",
                Selections[i, 2],
                ",",
                Selections[i, 3]
              )
            )
            browser()  # pause to allow saving plot
          }
        }
      }

    }
    invisible()
  }
