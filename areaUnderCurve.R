#' @description Calculates the area under the curve of a signal.
#' @import MESS
#' @param xValue: Vector containing the x values of the signal.
#' @param yValue: Vector containing the y values of the signal.
#' @return aucValue: Area under the curve value.
#' @author Heinz Lugo.
library(MESS)
areaUnderCurve <- function(xValue, yValue)
{
  tempDataFrame <- try(cbind.data.frame(xValue, yValue))
  if(class(tempDataFrame) == "try-error")
  {
    stop("All data columns must be the same length.")
  }
  aucValue <- MESS::auc(tempDataFrame$xValue, tempDataFrame$yValue, type = "linear")
  aucValue
}