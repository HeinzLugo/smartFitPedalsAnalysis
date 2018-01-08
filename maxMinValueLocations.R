#' @description Returns the maximum and minimum values and respective locations.
#' for a signal sorted by the Y value.
#' @param signalXValue: Vector with the X values.
#' @param signalYValue: Vector with the Y values.
#' @param numberTerms: Number of local maximum/minimum terms to return. By default the function returns the global
#' maximum and minimum.
#' @return resultsList: List containing the dataframe with the values and locations of the maximum and minimums.
#' @import dplyr
#' @import EMD
#' @author Heinz Lugo.
library(dplyr)
library(EMD)
maxMinValueLocations <- function(signalXValue, signalYValue, numberTerms = 1)
{
  tempDataFrame <- try(cbind.data.frame(signalXValue, signalYValue))
  if(class(tempDataFrame) == "try-error")
  {
    stop("All data columns must be the same length.")
  }
  maxValuesLocationsIndexes <- EMD::extrema(tempDataFrame$signalYValue)$maxindex[,1]
  minValuesLocationsIndexes <- EMD::extrema(tempDataFrame$signalYValue)$minindex[,1]
  maxValues <- tempDataFrame[c(maxValuesLocationsIndexes), ]
  minValues <- tempDataFrame[c(minValuesLocationsIndexes), ]
  maxValues <- dplyr::arrange(maxValues, desc(signalYValue))
  minValues <- dplyr::arrange(minValues, signalYValue)
  maxValues <- maxValues[1:numberTerms, ]
  minValues <- minValues[1:numberTerms, ]
  maxValues <- dplyr::mutate(maxValues, identifier = rep(x = "maximum", times = nrow(maxValues)), numberTerm = seq(from = 1, to = nrow(maxValues)))
  minValues <- dplyr::mutate(minValues, identifier = rep(x = "minimum", times = nrow(minValues)), numberTerm = seq(from = 1, to = nrow(minValues)))
  resultsDataFrame <- rbind.data.frame(maxValues, minValues)
  names(resultsDataFrame)[1:2] <- c("location", "value")
  resultsList <- vector(mode = "list", length = 1)
  resultsList[[1]] <- resultsDataFrame
  resultsList
}