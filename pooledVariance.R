#' @description Calculates the pooled variance value.
#' @param varianceValues: Column with the sample variance values.
#' @param sampleNumber: Column with the sample number values.
#' @returns pooledVariance: Pooled variance value.
#' @author Heinz Lugo.
pooledVariance <- function(varianceValues, sampleNumber)
{
  if(length(varianceValues) != length(sampleNumber))
  {
    stop("Both columns must have the same length.")
  }
  ## Step 1. Calculate the pooled variance mean.
  pooledVarianceValue <- sum(varianceValues * (sampleNumber - 1), na.rm = TRUE) / sum(sampleNumber[!is.na(sampleNumber)] - 1)
  pooledVarianceValue
}