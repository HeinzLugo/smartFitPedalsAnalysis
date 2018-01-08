#' @description Calculates the power balance per revolution and the
#' overall power balance distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @import weightedMean.R
#' @import pooledVariance.R
#' @param leftPowerValues: Vector with left power values.
#' @param rightPowerValues: Vector with right power values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo
library(dplyr)
source("splitByRevolution.R")
source("weightedMean.R")
source("pooledVariance.R")
powerBalance <- function(leftPowerValues, rightPowerValues, angleValues, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
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
  if((length(leftPowerValues) != length(rightPowerValues)) | (length(leftPowerValues) != length(angleValues)))
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
  dataFrameForAnalysis <- cbind.data.frame(leftPowerValues, rightPowerValues, dataFrameForAnalysis)
  ## Step 3. Pedal smoothness calculation per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  powerBalancePerRevolution <- dplyr::summarise(dataFrameForAnalysis,
                                                powerBalance = powerBalanceCalculation(leftPowerValues, rightPowerValues))
  tempDataFrame <- do.call(what = rbind.data.frame, args = powerBalancePerRevolution$powerBalance)
  powerBalancePerRevolution <- cbind.data.frame(revolutionFactorColumn = powerBalancePerRevolution$revolutionFactorColumn,
                                                tempDataFrame)
  ## Step 4. Overall power balance.
  powerBalancePerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
    rightMeanValue = weightedMean(meanValues = powerBalancePerRevolution$rightPowerBalanceMean, sampleNumber = powerBalancePerRevolution$rightSampleNumber),
    rightSDValue = sqrt(pooledVariance(varianceValues = powerBalancePerRevolution$rightPowerBalanceSD ** 2, sampleNumber = powerBalancePerRevolution$rightSampleNumber)),
    leftMeanValue = weightedMean(meanValues = powerBalancePerRevolution$leftPowerBalanceMean, sampleNumber = powerBalancePerRevolution$leftSampleNumber),
    leftSDValue = sqrt(pooledVariance(varianceValues = powerBalancePerRevolution$leftPowerBalanceSD ** 2, sampleNumber = powerBalancePerRevolution$leftSampleNumber)),
    rightSampleNumber = sum(!is.na(powerBalancePerRevolution$rightPowerBalanceMean)),
    leftSampleNumber = sum(!is.na(powerBalancePerRevolution$leftPowerBalanceMean))
  ))
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- powerBalancePerRevolution
    scriptResultsList[[2]] <- powerBalancePerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- powerBalancePerRevolution
    tempResultsList[[2]] <- powerBalancePerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Actual power balance calculation.
#' @param leftPowerValues: Vector with signal Y values.
#' @param rightPowerValues: Vector with right power values.
#' @return powerBalance: Power balance values.
#' @author Heinz Lugo.
powerBalanceCalculation <- function(leftPowerValues, rightPowerValues)
{
  ## Step 1. Normalise the scale.
  leftPowerValues <- (leftPowerValues - mean(leftPowerValues, na.rm = TRUE)) / sd(leftPowerValues, na.rm = TRUE)
  rightPowerValues <- (rightPowerValues - mean(rightPowerValues, na.rm = TRUE)) / sd(rightPowerValues, na.rm = TRUE)
  ## Step 2. Power balance calculation.
  leftPowerBalanceValues <- abs(leftPowerValues) / (abs(leftPowerValues) + abs(rightPowerValues))
  rightPowerBalanceValues <- 1 - leftPowerBalanceValues
  leftPowerBalance <- cbind.data.frame(leftPowerBalanceMean = mean(leftPowerBalanceValues, na.rm = TRUE), leftPowerBalanceSD = sd(leftPowerBalanceValues, na.rm = TRUE))
  rightPowerBalance <- cbind.data.frame(rightPowerBalanceMean = mean(rightPowerBalanceValues, na.rm = TRUE), rightPowerBalanceSD = sd(rightPowerBalanceValues, na.rm = TRUE))
  powerBalance <- dplyr::tbl_df(cbind.data.frame(leftPowerBalance, rightPowerBalance, leftSampleNumber = sum(!is.na(leftPowerBalanceValues)), rightSampleNumber = sum(!is.na(rightPowerBalanceValues))))
  powerBalance <- list(powerBalance)
  powerBalance
}