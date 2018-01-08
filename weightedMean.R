#' @description Calculates the weighted mean value.
#' @param meanValues: Column with the sample mean values.
#' @param sampleNumber: Column with the sample number values.
#' @returns weightedMeanValue: Weighted mean value.
#' @author Heinz Lugo.
weightedMean <- function(meanValues, sampleNumber)
{
  if(length(meanValues) != length(sampleNumber))
  {
    stop("Both columns must have the same length.")
  }
  ## Step 1. Calculate the weigthed mean.
  weightedMeanValue <- sum(meanValues * sampleNumber, na.rm = TRUE) / sum(sampleNumber[!is.na(sampleNumber)])
  weightedMeanValue
}