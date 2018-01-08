#' @description Calculates the pedal smoothness magnitude per revolution and the
#' overall pedal smoothness distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @param powerValues: Vector with power values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo
library(dplyr)
source("splitByRevolution.R")
pedalSmoothness <- function(powerValues, angleValues, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((analysisType != "angle") && (analysisType != "time"))
  {
    stop("Analysis type should be angle or time.")
  }
  degToRad <- pi / 180
  ## Step 1. Check the length of the columns.
  if(length(powerValues) != length(angleValues))
  {
    stop("The length of the power and angle values columns must be the same.")
  }
  ## Step 2. Split angle by revolution.
  if(angleType == "degrees")
  {
    angleValues <- angleValues * degToRad
  }
  dataFrameForAnalysis <- splitByRevolution(angleValues = angleValues, angleType = "radians")
  dataFrameForAnalysis <- dplyr::select(dataFrameForAnalysis, angleValues = originalAngleValues, revolutionFactorColumn)
  dataFrameForAnalysis <- cbind.data.frame(powerValues, dataFrameForAnalysis)
  ## Step 3. Pedal smoothness calculation per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  pedalSmoothnessPerRevolution <- dplyr::summarise(dataFrameForAnalysis, 
                                                       pedalSmoothness = pedalSmoothnessCalculation(signalYValues = powerValues))
  ## Step 4. Overall torque effectiveness.
  pedalSmoothnessPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
    meanValue = mean(pedalSmoothnessPerRevolution$pedalSmoothness, na.rm = TRUE),
    sdValue = sd(pedalSmoothnessPerRevolution$pedalSmoothness, na.rm = TRUE),
    sampleNumber = sum(!is.na(pedalSmoothnessPerRevolution$pedalSmoothness))
  ))
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- pedalSmoothnessPerRevolution
    scriptResultsList[[2]] <- pedalSmoothnessPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- pedalSmoothnessPerRevolution
    tempResultsList[[2]] <- pedalSmoothnessPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Actual pedal smoothness calculation.
#' @param signalYValues: Vector with signal Y values.
#' @return pedalSmoothness: Pedal smoothness value.
#' @author Heinz Lugo.
pedalSmoothnessCalculation <- function(signalYValues)
{
  meanSignalValue <- mean(signalYValues, na.rm = TRUE)
  maxSignalValue <- max(signalYValues, na.rm = TRUE)
  if((maxSignalValue == 0) | is.na(maxSignalValue))
  {
    pedalSmoothness <- 0
  }
  else
  {
    pedalSmoothness <- meanSignalValue / maxSignalValue
  }
  if(pedalSmoothness > 1)
  {
    pedalSmoothness <- 1
  }
  if(pedalSmoothness < 0)
  {
    pedalSmoothness <- 0
  }
  pedalSmoothness
}