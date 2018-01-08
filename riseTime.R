#' @description Calculates the rise time of a signal.
#' @param signalXValues: Vector with the X values.
#' @param signalYValues: Vector with the Y values.
#' @param signalType: Sets if the signal value is based on time or on radians. When based on radians wrap up of the
#' signal is taken into account.
#' @return riseTimeValue: Rise time value.
#' @import dplyr
#' @warning If the data does not cover the length required to present the rise time magnitude NA is returned.
#' @warning If the signal type is defined as circular X values must be provided in radians.
#' @warning The circular algorithm is based on mirroring the signal. If the rise time is not found on
#' left of the maximum value location then the fall time is used.
#' Other options that could be implemented are:
#' 1. Use a sliding window containing 3 cycles.
#' 2. Reconstruct the signal using FFT coefficients and using the theoretical FWHM value.
#' @author Heinz Lugo.
library(dplyr)
riseTime <- function(signalXValues, signalYValues, signalType = c("time", "circular"))
{
  if((signalType != "time") && (signalType != "circular"))
  {
    stop("Signal type should be time or circular.")
  }
  if(signalType == "circular")
  {
    if(max(signalXValues) > 2*pi)
    {
      stop("For circular signals X values must be provided in radians.")
    }
  }
  tempDataFrame <- try(cbind.data.frame(signalXValues, signalYValues))
  if(class(tempDataFrame) == "try-error")
  {
    stop("All data columns must be the same length.")
  }
  riseTimeValue <- NA
  startLocation <- NA
  finishLocation <- NA
  if(nrow(tempDataFrame) != 0)
  {
    ## Shift the signal to guarantee that all Y values are positive so the threshold inequality is not affected by negative numbers.
    tempDataFrame <- dplyr::mutate(tempDataFrame, signalYValues = signalYValues + (abs(min(tempDataFrame$signalYValues)) + 1))
    yMaxValue <- max(tempDataFrame$signalYValues)
    yMaxValueXLocation <- tempDataFrame$signalXValues[which.max(tempDataFrame$signalYValues)]
    yMinValueXLocation <- tempDataFrame$signalXValues[which.min(tempDataFrame$signalYValues)]
    yLimit10Percent <- yMaxValue * 0.1
    yLimit90Percent <- yMaxValue * 0.9
    if(signalType == "time")
    {
      dataFrameForAnalysis <- dplyr::mutate(tempDataFrame, differencesVector = abs(signalYValues - yLimit90Percent)) %>% dplyr::filter(signalXValues <= yMaxValueXLocation)
      if(nrow(dataFrameForAnalysis != 0))
      {
        dataFrameForAnalysis <- dplyr::distinct(dataFrameForAnalysis, differencesVector, signalXValues) %>% dplyr::arrange(differencesVector)
        finishLocation <- dataFrameForAnalysis$signalXValues[1]
      }
      dataFrameForAnalysis <- dplyr::mutate(tempDataFrame, differencesVector = abs(signalYValues - yLimit10Percent)) %>% dplyr::filter(signalXValues <= yMaxValueXLocation)
      if(nrow(dataFrameForAnalysis != 0))
      {
        dataFrameForAnalysis <- dplyr::distinct(dataFrameForAnalysis, differencesVector, signalXValues) %>% dplyr::arrange(differencesVector)
        startLocation <- dataFrameForAnalysis$signalXValues[1]
      }
      if((!is.na(startLocation)) && (!is.na(finishLocation)))
      {
        riseTimeValue <- abs(finishLocation - startLocation)
      }
    }
    else
    {
      if(yMaxValueXLocation >= yMinValueXLocation)
      {
        tempDataFrame <- dplyr::filter(tempDataFrame, signalXValues >= yMinValueXLocation, signalXValues <= yMaxValueXLocation)
      }
      else
      {
        tempDataFrame <- dplyr::filter(tempDataFrame, signalXValues >= yMaxValueXLocation, signalXValues <= yMinValueXLocation)
      }
      dataFrameForAnalysis <- dplyr::mutate(tempDataFrame, differencesVector = abs(signalYValues - yLimit90Percent))
      if(nrow(dataFrameForAnalysis != 0))
      {
        dataFrameForAnalysis <- dplyr::distinct(dataFrameForAnalysis, differencesVector, signalXValues) %>% dplyr::arrange(differencesVector)
        finishLocation <- dataFrameForAnalysis$signalXValues[1]
      }
      dataFrameForAnalysis <- dplyr::mutate(tempDataFrame, differencesVector = abs(signalYValues - yLimit10Percent))
      if(nrow(dataFrameForAnalysis != 0))
      {
        dataFrameForAnalysis <- dplyr::distinct(dataFrameForAnalysis, differencesVector, signalXValues) %>% dplyr::arrange(differencesVector)
        startLocation <- dataFrameForAnalysis$signalXValues[1]
      }
      if((!is.na(startLocation)) && (!is.na(finishLocation)))
      {
        riseTimeValue <- abs(finishLocation - startLocation)
      }
    }
  }
  riseTimeValue
}