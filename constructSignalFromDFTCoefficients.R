#' @description Reconstructs the signal based on the Discrete Fourier Transform results. The number of terms
#' used is controlled using the parameter provided. The selection is based on the sorted amplitude of the components.
#' The function also returns the residuals and the derivative evaluated at the sampling points.
#' @param samplingFrequency: Sampling frequency value.
#' @param signal: Vector containing the signal value.
#' @param numberOfTermsToInclude: Number of frequency terms to include in the fit. By default it is set to 1. The constant value
#' is always included.
#' @return resultsReconstructDFTList: List of dataframes containing the reconstructed signal, the residual sum of squares result and a
#' dataframe containing the derivative values.
#' @import dplyr
#' @import discreteFourierTransform.R
#' @warning In the case of signals where the sampling rate is known in degrees then it should be transformed to Hz outside
#' this function.
#' @author Heinz Lugo.
library(dplyr)
source("discreteFourierTransform.R")
constructSignalFromDFTCoefficients <- function(samplingFrequency, signal, numberOfTermsToInclude = 1)
{
  ## Step 1. Discrete Fourier transform results.
  dftCoefficients <- discreteFourierTransform(samplingFrequency, signal)
  tempDataFrame <- dplyr::filter(dftCoefficients, freq <= (samplingFrequency / 2)) %>% dplyr::select(cycle:delay)
  ## Step 2. Sort and select the component frequencies.
  constantTerm <- tempDataFrame[1, ]
  tempDataFrame <- tempDataFrame[-1, ]
  tempDataFrame <-  dplyr::mutate(tempDataFrame, strength = strength * 2)
  tempDataFrame <- dplyr::arrange(tempDataFrame, desc(strength))
  if(nrow(tempDataFrame) > (numberOfTermsToInclude))
  {
    tempDataFrame <- tempDataFrame[1:numberOfTermsToInclude,]
  }
  tempDataFrame <- rbind.data.frame(constantTerm, tempDataFrame)
  ## Step 3. Reconstruct the signal with the component frequencies.
  resultsReconstructDFTList <- vector(mode = "list", length = 3)
  ts <- seq(0, 1/samplingFrequency * (length(signal) - 1), 1/samplingFrequency)
  for(i in 1:nrow(tempDataFrame))
  {
    tempValues <- signalValue(ts, tempDataFrame$strength[i], tempDataFrame$freq[i], tempDataFrame$delay[i])
    if (i == 1)
    {
      signalValues <- tempValues
    }
    else
    {
      signalValues <- signalValues + tempValues
    }
  }
  tempResultsDataFrame <- dplyr::tbl_df(cbind.data.frame(time = ts, signal = signalValues))
  resultsReconstructDFTList[[1]] <- tempResultsDataFrame
  ## Step 4. Calculate the residuals.
  tempValues <- signal - tempResultsDataFrame$signal
  tempResultsDataFrame <- dplyr::tbl_df(cbind.data.frame(time = ts, residuals = tempValues))
  resultsReconstructDFTList[[2]] <- tempResultsDataFrame
  ## Step 5. Calculate the derivative.
  for(i in 1:nrow(tempDataFrame))
  {
    tempValues <- derivativeValue(ts, tempDataFrame$strength[i], tempDataFrame$freq[i], tempDataFrame$delay[i])
    if (i == 1)
    {
      signalValues <- tempValues
    }
    else
    {
      signalValues <- signalValues + tempValues
    }
  }
  tempResultsDataFrame <- dplyr::tbl_df(cbind.data.frame(time = ts, derivative = signalValues))
  resultsReconstructDFTList[[3]] <- tempResultsDataFrame
  resultsReconstructDFTList
}

#' @description Calculates the signal values at the sampling points.
#' @param ts: Vector containing the sampling points.
#' @param strength: Vector containing the amplitude values.
#' @param frequency: Vector containg the frequency values.
#' @param delay: Vector containing the delay values.
#' @return evaluatedFunction: Vector containg the evaluated signal values.
#' @author Heinz Lugo.
signalValue <- function(ts, strength, frequency, delay)
{
  evaluatedFunction <- strength * cos(2 * pi * frequency * ts + delay)
}

#' @description Calculates the derivative of the DFT constructed signal at the sampling points.
#' @import numDeriv
#' @param ts: Vector containing the sampling points.
#' @param strength: Vector containing the amplitude values.
#' @param frequency: Vector containg the frequency values.
#' @param delay: Vector containing the delay values.
#' @return evaluatedFunction: Vector containg the evaluated derivative values.
#' @author Heinz Lugo.
library(numDeriv)
derivativeValue <- function(ts, strength, frequency, delay)
{
  func <- function(ts){strength * cos(2 * pi * frequency * ts + delay)}
  evaluatedFunction <- numDeriv::grad(func, ts)
}
