#' @description Returns the Discrete Fourier Transform results.
#' @param samplingFrequency: Sampling frequency value.
#' @param signal: Vector containing the signal value.
#' @return dftDataFrame: Dataframe containing the frequency, strength and delay values.
#' @import dplyr
#' @import stats
#' @warning In the case of signals where the sampling rate is known in degrees then it should be transformed to Hz outside
#' this function.
#' @author Heinz Lugo.
library(dplyr)
library(stats)
discreteFourierTransform <- function(samplingFrequency, signal)
{
  ## Step 1. Normalised Discrete Fourier Transform values.
  fftValues <- stats::fft(signal)
  fftNormalisedValues <- fftValues / length(signal)
  ## Step 2. Calculate the frequency, strength and delay values.
  strengthFunction <- function(c)signif(Mod(c), 4)
  angleFunction <- function(c)signif(Arg(c), 3)
  ## Step 3. Construct the resulting dataframe.
  dftDataFrame <- data.frame(fftOriginalValues = fftValues,
                             fftNormalisedValues,
                             cycle = 0:(length(fftNormalisedValues) - 1),
                             freq = 0:(length(fftNormalisedValues) - 1) * samplingFrequency / length(signal),
                             strength = sapply(fftNormalisedValues, strengthFunction),
                             delay = sapply(fftNormalisedValues, angleFunction))
  dftDataFrame <- dplyr::tbl_df(dftDataFrame)
  dftDataFrame <- dplyr::mutate(dftDataFrame, delayDeg = 180 * delay / pi)
  dftDataFrame
}