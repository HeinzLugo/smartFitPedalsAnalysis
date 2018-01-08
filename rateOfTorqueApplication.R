#' @description Ccalculates the rate of torque application magnitude per revolution and the
#' overall rate of torque application distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @param torqueValues: Vector with torque values.
#' @param angleValues: Vector with angle values.
#' @param angularDivisionValue: Angle sector in which the column should be split, specified in degrees (e.g. 180, 45, 360).
#' @param angleType: Type of the of the angle values.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo.
source("splitByRevolution.R")
rateOfTorqueApplication <- function(torqueValues, angleValues, angularDivisionValue, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
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
  samplingFrequency <- 180 / (angularDivisionValue * pi)
  angularDivisionValue <- angularDivisionValue * degToRad
  ## Step 1. Check the length of the columns.
  if(length(torqueValues) != length(angleValues))
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
  dataFrameForAnalysis <- cbind.data.frame(torqueValues, dataFrameForAnalysis)
  ## Step 3. Rate of torque per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  rateOfTorqueApplicationPerRevolution <- dplyr::summarise(dataFrameForAnalysis, 
                                                   rateOfTorqueApplication = rateOfTorqueApplicationCalculation(
                                                     samplingFrequency = samplingFrequency,
                                                     torqueValues = torqueValues,
                                                     angleValues = angleValues,
                                                     angularDivisionValue = angularDivisionValue,
                                                     angleType = angleType, 
                                                     analysisType = analysisType))
  ## Step 4. Overall rate of torque application.
  for(i in 1:nrow(rateOfTorqueApplicationPerRevolution))
  {
    tempDataFrame <- rateOfTorqueApplicationPerRevolution$rateOfTorqueApplication[i][[1]]
    if(i == 1)
    {
      rateOfTorqueApplicationPerRevolutionSummary <- tempDataFrame
    }
    else
    {
      rateOfTorqueApplicationPerRevolutionSummary <- rbind.data.frame(rateOfTorqueApplicationPerRevolutionSummary, tempDataFrame)
    }
  }
  rateOfTorqueApplicationPerRevolutionSummary <- dplyr::tbl_df(rateOfTorqueApplicationPerRevolutionSummary)
  rateOfTorqueApplicationPerRevolutionSummary <- dplyr::group_by(rateOfTorqueApplicationPerRevolutionSummary, angleSectorFactorColumn)
  rateOfTorqueApplicationPerRevolutionSummary <- dplyr::summarise(rateOfTorqueApplicationPerRevolutionSummary,
                                                                  meanValue = mean(rateOfTorqueApplication, na.rm = TRUE),
                                                                  sdValue = sd(rateOfTorqueApplication, na.rm = TRUE),
                                                                  sampleNumber = sum(!is.na(rateOfTorqueApplication)))

  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- rateOfTorqueApplicationPerRevolution
    scriptResultsList[[2]] <- rateOfTorqueApplicationPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- rateOfTorqueApplicationPerRevolution
    tempResultsList[[2]] <- rateOfTorqueApplicationPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Actual rate of torque application calculation.
#' @import splitByAngularRevolution.R
#' @param samplingFrequency: Sampling frequency value.
#' @param torqueValues: Vector with torque values.
#' @param angleValues: Vector with angle values.
#' @param angularDivisionValue: Angle sector in which the column should be split.
#' @param angleType: Type of the of the angle values.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return rateOfTorque: Rate of torque application value.
#' @author Heinz Lugo.
source("valueSummaryPerAngleSector.R")
source("rateOfApplication.R")
rateOfTorqueApplicationCalculation <- function(samplingFrequency, torqueValues, angleValues, angularDivisionValue, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
{
  radToDeg <- 180 / pi
  ## Step 1. Mean torque value per angle sector.
  dataFrameForAnalysis <- valueSummaryPerAngleSector(valueColumn = torqueValues, angleValues = angleValues, angularDivisionValue = angularDivisionValue, angleType = angleType)
  dataFrameForAnalysis <- dataFrameForAnalysis[[1]]
  ## Step 2. Rate of torque application.
  numberOfTermsToInclude <- 2
  tempDataFrame <- rateOfApplication(samplingFrequency = samplingFrequency, signal = dataFrameForAnalysis$meanValue, numberOfTermsToInclude = numberOfTermsToInclude) 
  names(tempDataFrame) <- c("angleSectorFactorColumn", "rateOfTorqueApplication")
  if(angleType == "degrees")
  {
    tempDataFrame$angleSectorFactorColumn <- tempDataFrame$angleSectorFactorColumn * radToDeg
  }
  rateOfTorque <- vector(mode = "list", length = 1)
  rateOfTorque[[1]] <- tempDataFrame
  rateOfTorque
}