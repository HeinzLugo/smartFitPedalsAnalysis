#' @description Calculates the rate of application of a signal.
#' @import constructSignalFromDFTCoefficients.R
#' @param samplingFrequency: Sampling frequency value.
#' @param signal: Vector containing the signal value.
#' @param numberOfTermsToInclude: Number of terms to include from .
#' @return rateOfApplicationValue: Rate of application values.
#' @author Heinz Lugo.
source("constructSignalFromDFTCoefficients.R")
rateOfApplication <- function(samplingFrequency, signal, numberOfTermsToInclude)
{
  rateOfApplicationValue <- constructSignalFromDFTCoefficients(samplingFrequency = samplingFrequency, signal = signal, numberOfTermsToInclude = numberOfTermsToInclude)
  rateOfApplicationValue <- rateOfApplicationValue[[3]]
  rateOfApplicationValue
}