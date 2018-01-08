#' @description Calculates the Full Width At Half Magnitude distribution per revolution and the
#' overall FWHM distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @import fullWidthAtHalfMaximum.R
#' @import circular
#' @param powerValues: Vector with power values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param powerType: Sets if the power values are coming from a single leg or as the net contribution of both legs.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @warning If the data does not cover the length required to present the FWHM magnitude NA is returned.
#' @warning The results returned are of type circular. The dimensional unit for the angle values are the same as
#' the dimensional unit set in the function call.
#' @author Heinz Lugo.
library(dplyr)
source("splitByRevolution.R")
source("fullWidthAtHalfMaximum.R")
library(circular)
fwhmPower <- function(powerValues, angleValues, angleType = c("degrees", "radians"), powerType = c("individual", "net"), analysisType = c("angle", "time"))
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
    fwhmPowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                         fwhm = fullWidthAtHalfMaximum(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
  }
  else
  {
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues <= rightLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    rightFWHMPowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                               rightFWHM = fullWidthAtHalfMaximum(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues >= leftLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    leftFWHMPowerPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                    leftFWHM = fullWidthAtHalfMaximum(signalXValues = angleValues, signalYValues = powerValues, signalType = "circular"))
    if(nrow(rightFWHMPowerPerRevolution) <= nrow(leftFWHMPowerPerRevolution))
    {
      fwhmPowerPerRevolution <- merge(x = leftFWHMPowerPerRevolution, y = rightFWHMPowerPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE)
    }
    else
    {
      fwhmPowerPerRevolution <- merge(x = rightFWHMPowerPerRevolution, y = leftFWHMPowerPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE)
    }
  }
  ## Step 4. Overall full width at half maximum.
  if(powerType == "individual")
  {
    fwhmPowerPerRevolution$fwhm <- circular::circular(x = fwhmPowerPerRevolution$fwhm,
                                                      type = "angles", units = "radians", template = "none",
                                                      modulo = "2pi", zero = pi/2, rotation = "clock")
    fwhmPowerPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      meanValue = circular::mean.circular(x = fwhmPowerPerRevolution$fwhm, na.rm = TRUE),
      sdValue = circular::sd.circular(fwhmPowerPerRevolution$fwhm, na.rm = TRUE),
      sampleNumber = sum(!is.na(fwhmPowerPerRevolution$fwhm))
    ))
    if(angleType == "degrees")
    {
      fwhmPowerPerRevolution$fwhm <- circular::deg(fwhmPowerPerRevolution$fwhm)
      fwhmPowerPerRevolutionSummary$meanValue <- circular::deg(fwhmPowerPerRevolutionSummary$meanValue)
      fwhmPowerPerRevolutionSummary$sdValue <- circular::deg(fwhmPowerPerRevolutionSummary$sdValue)
    }
  }
  else
  {
    fwhmPowerPerRevolution$rightFWHM <- circular::circular(x = fwhmPowerPerRevolution$rightFWHM,
                                                      type = "angles", units = "radians", template = "none",
                                                      modulo = "2pi", zero = pi/2, rotation = "clock")
    fwhmPowerPerRevolution$leftFWHM <- circular::circular(x = fwhmPowerPerRevolution$leftFWHM,
                                                           type = "angles", units = "radians", template = "none",
                                                           modulo = "2pi", zero = pi/2, rotation = "clock")
    fwhmPowerPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      rightMeanValue = circular::mean.circular(fwhmPowerPerRevolution$rightFWHM, na.rm = TRUE),
      rightSDValue = circular::sd.circular(fwhmPowerPerRevolution$rightFWHM, na.rm = TRUE),
      rightSampleNumber = sum(!is.na(fwhmPowerPerRevolution$rightFWHM)),
      leftMeanValue = circular::mean.circular(fwhmPowerPerRevolution$leftFWHM, na.rm = TRUE),
      leftSDValue = circular::sd.circular(fwhmPowerPerRevolution$leftFWHM, na.rm = TRUE),
      leftSampleNumber = sum(!is.na(fwhmPowerPerRevolution$leftFWHM))
    ))
    if(angleType == "degrees")
    {
      fwhmPowerPerRevolution$rightFWHM <- circular::deg(fwhmPowerPerRevolution$rightFWHM)
      fwhmPowerPerRevolution$leftFWHM <- circular::deg(fwhmPowerPerRevolution$leftFWHM)
      fwhmPowerPerRevolutionSummary$rightMeanValue <- circular::deg(fwhmPowerPerRevolutionSummary$rightMeanValue)
      fwhmPowerPerRevolutionSummary$rightSDValue <- circular::deg(fwhmPowerPerRevolutionSummary$rightSDValue)
      fwhmPowerPerRevolutionSummary$leftMeanValue <- circular::deg(fwhmPowerPerRevolutionSummary$leftMeanValue)
      fwhmPowerPerRevolutionSummary$leftSDValue <- circular::deg(fwhmPowerPerRevolutionSummary$leftSDValue)
    }
  }
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- fwhmPowerPerRevolution
    scriptResultsList[[2]] <- fwhmPowerPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- fwhmPowerPerRevolution
    tempResultsList[[2]] <- fwhmPowerPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}