#' @description Calculates the cadence distribution per revolution and the
#' overall cadence distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import valueSummaryPerRevolution.R
#' @import weightedMean.R
#' @import pooledVariance.R
#' @param cadenceValues: Vector with cadence values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @param cadenceApproximation: Boolean index if cadence approximation is used. By default True is used.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo
library(dplyr)
source("valueSummaryPerRevolution.R")
source("weightedMean.R")
source("pooledVariance.R")
cadenceDistribution <- function(cadenceValues, angleValues, angleType = c("degrees", "radians"),  analysisType = c("angle", "time"), cadenceApproximation = TRUE)
{
  if((angleType != "degrees") & (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((analysisType != "angle") && (analysisType != "time"))
  {
    stop("Analysis type should be angle or time.")
  }
  degPerSecToRevPerMin <- 60 / 360
  degPerSecToRadPerPerSec <- pi / 180
  ## Step 1. Check the length of the columns.
  if(length(cadenceValues) != length(angleValues))
  {
    stop("The length of the cadence and angle values columns must be the same.")
  }
  ## Step 2. Cadence per revolution.
  cadencePerRevolution <- valueSummaryPerRevolution(valueColumn = cadenceValues, angleValues = angleValues, angleType = angleType)
  cadencePerRevolution <- cadencePerRevolution[[1]]
  # Step 3. Overall cadence.
  if(cadenceApproximation == FALSE)
  {
    cadencePerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      meanValue = weightedMean(meanValues = cadencePerRevolution$meanValue, sampleNumber = cadencePerRevolution$sampleNumber),
      sdValue = sqrt(pooledVariance(varianceValues = cadencePerRevolution$sdValue**2, sampleNumber = cadencePerRevolution$sampleNumber)),
      sampleNumber = sum(!is.na(cadencePerRevolution$meanValue))
    ))
  }
  else
  {
    cadencePerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      meanValue = weightedMean(meanValues = cadencePerRevolution$meanValue, sampleNumber = cadencePerRevolution$sampleNumber),
      sdValue = sd(cadencePerRevolution$meanValue, na.rm = TRUE),
      sampleNumber = sum(!is.na(cadencePerRevolution$meanValue))
    ))
  }
  ## Step 4. Result list.
  if(analysisType == "angle"){
    scriptResultsList <- vector("list", 2)
    scriptResultsList[[1]] <- cadencePerRevolution
    scriptResultsList[[2]] <- cadencePerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- cadencePerRevolution
    tempResultsList[[2]] <- cadencePerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}