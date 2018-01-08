#' @description Calculates the Full Width At Half Maximum (FWHM) value of a signal.
#' @param signalXValues: Vector with the X values.
#' @param signalYValues: Vector with the Y values.
#' @param signalType: Sets if the signal value is based on time or on radians. When based on radians wrap up of the
#' signal is taken into account.
#' @return fwhmValue: FWHM value.
#' @import dplyr
#' @warning If the data does not cover the length required to present the FWHM magnitude NA is returned.
#' @warning If the signal type is defined as circular X values must be provided in radians.
#' @warning The circular algorithm is based on mirroring the signal. Other options that could be implemented are:
#' 1. Use a sliding window containing 3 cycles.
#' 2. Reconstruct the signal using FFT coefficients and using the theoretical FWHM value.
#' @author Heinz Lugo.
library(dplyr)
fullWidthAtHalfMaximum <- function(signalXValues, signalYValues, signalType = c("time", "circular"))
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
  fwhmValue <- NA
  if(nrow(tempDataFrame) != 0)
  {
    ## Shift the signal to guarantee that all Y values are positive so the threshold inequality is not affected by negative numbers.
    tempDataFrame <- dplyr::mutate(tempDataFrame, signalYValues = signalYValues + (abs(min(tempDataFrame$signalYValues)) + 1))
    yMaxValue <- max(tempDataFrame$signalYValues)
    yMaxValueXLocation <- tempDataFrame$signalXValues[which.max(tempDataFrame$signalYValues)]
    tempDataFrame <- dplyr::mutate(tempDataFrame, differencesVector = abs(signalYValues - yMaxValue / 2))
    tempDataFrame <- dplyr::distinct(tempDataFrame, differencesVector, signalXValues) %>% dplyr::arrange(differencesVector)
    ## Handle signal type.
    if(signalType == "circular")
    {
      if(nrow(tempDataFrame) > 1)
      {
        testValueOne <- yMaxValueXLocation - tempDataFrame$signalXValues[1]
        testValueTwo <- yMaxValueXLocation - tempDataFrame$signalXValues[2]
        if(tempDataFrame$signalXValues[1] != tempDataFrame$signalXValues[2])
        {
          if((testValueOne < 0) && (testValueTwo < 0))
          {
            ##--REMOVE COMMENT IF NO MIRROR ASSUMPTION IS NOT USED--##
            # fwhmValue <- (2 * pi) - max(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2])) + min(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2]))
            ##------------------------------------------------------##
            fwhmValue <- 2 * (min(c(tempDataFrame$signalXValues[1],  tempDataFrame$signalXValues[2])) - yMaxValueXLocation)
          }
          else if((testValueOne > 0) && (testValueTwo > 0))
          {
            ##--REMOVE COMMENT IF NO MIRROR ASSUMPTION IS NOT USED--##
            # fwhmValue <- (2 * pi) - max(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2])) + min(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2]))
            ##------------------------------------------------------##
            fwhmValue <- 2 * (yMaxValueXLocation - max(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2])))
          }
          else
          {
            fwhmValue <- max(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2])) - min(c(tempDataFrame$signalXValues[1], tempDataFrame$signalXValues[2]))
          }
        }
      }
    }
    else
    {
      if(nrow(tempDataFrame) > 1)
      {
        fwhmValue <- abs(tempDataFrame$signalXValues[1] - tempDataFrame$signalXValues[2])
      }
    }
  }
  fwhmValue
}