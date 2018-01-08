#' @description Calculates the normalised power magnitude.
#' Returns the normalised power value.
#' Given the Normalised Power definition only time interval option is possible, if the time interval
#' is smaller than 30 seconds the value returned is NA.
#' @import dplyr
#' @import stats
#' @param powerValues: Vector with power values.
#' @param samplingFrequency: Data sampling frequency.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo.
library(dplyr)
library(stats)
normalisedPower <- function(powerValues, samplingFrequency)
{
  normalisedPowerValue <- normalisedPowerCalculation(powerValues = powerValues, samplingFrequency = samplingFrequency)
  ## Step 1. Result list.
  scriptResultsList <- vector("list", 1) 
  scriptResultsList[[1]] <- normalisedPowerValue
  scriptResultsList
}

#' @description Calculates the Normalised Power based on the formula presented by
#' Cogan and Allen. If there is less than 30 seconds worth of data the normalised
#' power value cannot be calculated and the returned value is NA.
#' @param powerValues: Power values.
#' @param samplingFrequency
#' @return normalisedPowerValue: Normalised power value.
#' @author Heinz Lugo.
normalisedPowerCalculation <- function(powerValues, samplingFrequency)
{
  numberOfSamples <- 30 / (1 / samplingFrequency)
  tempVector <- powerValues
  normalisedPowerValue <- NA
  if(length(powerValues) > numberOfSamples)
  {
    powerValues <- stats::filter(x = powerValues, filter = rep(1 / numberOfSamples, numberOfSamples), sides = 1)
    powerValues <- powerValues[which(!is.na(powerValues))]
    if(length(powerValues) > 1)
    {
      normalisedPowerValue <- (powerValues)^4
      normalisedPowerValue <- mean(normalisedPowerValue)
      normalisedPowerValue <- (normalisedPowerValue)^0.25
    }
  }
  normalisedPowerValue
}