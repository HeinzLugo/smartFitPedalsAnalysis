#' @description Calculates the rise time distribution per revolution and 
#' the overall rise time distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import circular
#' @import splitByRevolution.R
#' @import riseTime.R
#' @param powerValues: Vector with power values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param powerType: Sets if the power values are coming from a single leg or as the net contribution of both legs.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo.
library(dplyr)
library(circular)
source("splitByRevolution.R")
source("riseTime.R")
riseTimePower <- function(powerValues, angleValues, angleType = c("degrees", "radians"), powerType = c("individual", "net"), analysisType = c("angle", "time"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((powerType != "individual") && (powerType != "net"))
  {
    stop("Power type should be individual or net.")
  }
  if((analysisType != "angle") && (analysisType != "time"))
  {
    stop("Analysis type should be angle or time.")
  }
  degToRad <- pi / 180
  rightLegAngleLimit <- 210 * degToRad
  leftLegAngleLimit <- 150 * degToRad
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
  ## Step 3. Full width at half maximum calculation per revolution.
  if(powerType == "individual")
  {
    tempDataFrame <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
    riseTimePowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                               riseTime = riseTime(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
  }
  else
  {
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues <= rightLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    rightRiseTimePowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                    rightRiseTime = riseTime(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues >= leftLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    leftRiseTimePowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                    leftRiseTime = riseTime(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
    if(nrow(rightRiseTimePowerPerRevolution) <= nrow(leftRiseTimePowerPerRevolution))
    {
      riseTimePowerPerRevolution <- merge(x = leftRiseTimePowerPerRevolution, y = rightRiseTimePowerPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE)
    }
    else
    {
      riseTimePowerPerRevolution <- merge(x = rightRiseTimePowerPerRevolution, y = leftRiseTimePowerPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE)
    }
  }
  ## Step 4. Rise time.
  if(powerType == "individual")
  {
    riseTimePowerPerRevolution$riseTime <- circular::circular(x = riseTimePowerPerRevolution$riseTime,
                                                              type = "angles", units = "radians", template = "none",
                                                              modulo = "2pi", zero = pi/2, rotation = "clock")
    riseTimePowerPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      meanValue = circular::mean.circular(x = riseTimePowerPerRevolution$riseTime, na.rm = TRUE),
      sdValue = circular::sd.circular(riseTimePowerPerRevolution$riseTime, na.rm = TRUE),
      sampleNumber = sum(!is.na(riseTimePowerPerRevolution$riseTime))
    ))
    if(angleType == "degrees")
    {
      riseTimePowerPerRevolution$riseTime <- circular::deg(riseTimePowerPerRevolution$riseTime)
      riseTimePowerPerRevolutionSummary$meanValue <- circular::deg(riseTimePowerPerRevolutionSummary$meanValue)
      riseTimePowerPerRevolutionSummary$sdValue <- circular::deg(riseTimePowerPerRevolutionSummary$sdValue)
    }
  }
  else
  {
    riseTimePowerPerRevolution$rightRiseTime <- circular::circular(x = riseTimePowerPerRevolution$rightRiseTime,
                                                           type = "angles", units = "radians", template = "none",
                                                           modulo = "2pi", zero = pi/2, rotation = "clock")
    riseTimePowerPerRevolution$leftRiseTime <- circular::circular(x = riseTimePowerPerRevolution$leftRiseTime,
                                                          type = "angles", units = "radians", template = "none",
                                                          modulo = "2pi", zero = pi/2, rotation = "clock")
    riseTimePowerPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      rightMeanValue = circular::mean.circular(riseTimePowerPerRevolution$rightRiseTime, na.rm = TRUE),
      rightSDValue = circular::sd.circular(riseTimePowerPerRevolution$rightRiseTime, na.rm = TRUE),
      rightSampleNumber = sum(!is.na(riseTimePowerPerRevolution$rightRiseTime)),
      leftMeanValue = circular::mean.circular(riseTimePowerPerRevolution$leftRiseTime, na.rm = TRUE),
      leftSDValue = circular::sd.circular(riseTimePowerPerRevolution$leftRiseTime, na.rm = TRUE),
      leftSampleNumber = sum(!is.na(riseTimePowerPerRevolution$leftRiseTime))
    ))
    if(angleType == "degrees")
    {
      riseTimePowerPerRevolution$rightRiseTime <- circular::deg(riseTimePowerPerRevolution$rightRiseTime)
      riseTimePowerPerRevolution$leftRiseTime <- circular::deg(riseTimePowerPerRevolution$leftRiseTime)
      riseTimePowerPerRevolutionSummary$rightMeanValue <- circular::deg(riseTimePowerPerRevolutionSummary$rightMeanValue)
      riseTimePowerPerRevolutionSummary$rightSDValue <- circular::deg(riseTimePowerPerRevolutionSummary$rightSDValue)
      riseTimePowerPerRevolutionSummary$leftMeanValue <- circular::deg(riseTimePowerPerRevolutionSummary$leftMeanValue)
      riseTimePowerPerRevolutionSummary$leftSDValue <- circular::deg(riseTimePowerPerRevolutionSummary$leftSDValue)
    }
  }
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- riseTimePowerPerRevolution
    scriptResultsList[[2]] <- riseTimePowerPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- riseTimePowerPerRevolution
    tempResultsList[[2]] <- riseTimePowerPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}