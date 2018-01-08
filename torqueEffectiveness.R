#' @description Calculates the torque effectiveness magnitude per revolution and the
#' overall torque effectiveness distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import MESS
#' @import splitByRevolution.R
#' @param powerValues: Vector with power values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo.
library(dplyr)
library(MESS)
source("splitByRevolution.R")
torqueEffectiveness <- function(powerValues, angleValues, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
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
  ## Step 3. Torque effectiveness calculation per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  torqueEffectivenessPerRevolution <- dplyr::summarise(dataFrameForAnalysis, 
                                                       torqueEffectiveness = torqueEffectivenessCalculation(signalXValues = angleValues, signalYValues = powerValues))
  ## Step 4. Overall torque effectiveness.
  torqueEffectivenessPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
    meanValue = mean(torqueEffectivenessPerRevolution$torqueEffectiveness, na.rm = TRUE),
    sdValue = sd(torqueEffectivenessPerRevolution$torqueEffectiveness, na.rm = TRUE),
    sampleNumber = sum(!is.na(torqueEffectivenessPerRevolution$torqueEffectiveness))
  ))
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- torqueEffectivenessPerRevolution
    scriptResultsList[[2]] <- torqueEffectivenessPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- torqueEffectivenessPerRevolution
    tempResultsList[[2]] <- torqueEffectivenessPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Actual torque effectiveness calculation.
#' @import MESS
#' @param signalXValues: Vector with signal X values.
#' @param signalYValues: Vector with signal Y values.
#' @return torqueEffectiveness: Torque effectiveness value.
#' @author Heinz Lugo.
torqueEffectivenessCalculation <- function(signalXValues, signalYValues)
{
  ## Step 1. Index for positive Y signal values.
  idx <- which(signalYValues >= 0)
  ## Step 2. AUC for positive values.
  aucPositiveValues <- MESS::auc(x = signalXValues[idx], y = signalYValues[idx])
  if((aucPositiveValues == 0) | is.na(aucPositiveValues))
  {
    torqueEffectiveness <- 0
  }
  else
  {
    torqueEffectiveness <- MESS::auc(x = signalXValues, y = signalYValues) / aucPositiveValues
  }
  if(torqueEffectiveness > 1)
  {
    torqueEffectiveness <- 1
  }
  if(torqueEffectiveness < 0)
  {
    torqueEffectiveness <- 0
  }
  torqueEffectiveness
}