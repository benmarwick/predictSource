#' ps_convexHull
#'
#' Compute the corners of a convex hull and plot the hull: used in ps_pcaEvaluation()
#'
#' @param data  R matrix or data frame containing the data to be analyzed
#' @param groupVar  The name for variable defining grouping; a group variable is required
#' @param hullGroup  A code defining the group for which a hull is to be plotted
#'
#' @section  Details:
#' The function assumes that the data frame data (an argument) contains group codes in
#' GroupVar and two principal components, named pc1 and pc2.
#'
#' @return  The function returns a matrix with the points defining the corners of the
#' convex hull.
#'
#' @import graphics
#'
#' @export
#'

ps_convexHull <-
  function(data,
           groupVar,
           hullGroup)
  {
    locations <- data[data[, groupVar] == hullGroup, c("pc1", "pc2")]
    chull <- chull(x = locations[, "pc1"], y = locations[, "pc2"])
    chull <- c(chull, chull[1])
    hull_pts <-
      locations[chull, c("pc1", "pc2")]  # points in order defining hull
    lines(x = hull_pts[, "pc1"], y = hull_pts[, "pc2"])
    hull_pts
  }
