#' @description Calculates the mechanical work per revolution and the
#' overall mechanical work distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @import areaUnderCurve.R
#' @param torqueValues: Vector with torque values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param cadenceValues: Vector with cadence values.
#' @param cadenceUnits: Dimensional unit for the cadence value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo
library(dplyr)
source("splitByRevolution.R")
source("areaUnderCurve.R")
mechanicalWork <- function(torqueValues, angleValues, angleType = c("degrees", "radians"), cadenceValues, cadenceUnits = c("rpm", "rad/s", "deg/s"),  analysisType = c("angle", "time"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((cadenceUnits != "rpm") && (cadenceUnits != "rad/s") && (cadenceUnits != "deg/s"))
  {
    stop("Cadence units should be rpm, rad/s or deg/s.")
  }
  if((analysisType != "angle") && (analysisType != "time"))
  {
    stop("Analysis type should be angle or time.")
  }
  rpmToRadPerSec <- 2 * pi / 60
  degPerSecToRadPerSec <- pi / 180
  millimetresToMetres <- 1 / 1000
  degToRad <- pi / 180
  ## Step 1. Cadence values dimensional check.
  if(cadenceUnits == "rpm")
  {
    cadenceValues <- cadenceValues * rpmToRadPerSec
  }
  else if(cadenceUnits == "deg/s")
  {
    cadenceValues <- cadenceValues * degPerSecToRadPerSec
  }
  ## Step 2. Check the length of the columns.
  if((length(torqueValues) != length(angleValues)) | (length(torqueValues) != length(cadenceValues)))
  {
    stop("The length of the power and angle values columns must be the same.")
  }
  ## Step 3. Split angle by revolution.
  if(angleType == "degrees")
  {
    angleValues <- angleValues * degToRad
  }
  dataFrameForAnalysis <- splitByRevolution(angleValues = angleValues, angleType = "radians")
  dataFrameForAnalysis <- dplyr::select(dataFrameForAnalysis, angleValues = originalAngleValues, revolutionFactorColumn)
  dataFrameForAnalysis <- cbind.data.frame(torqueValues, cadenceValues, dataFrameForAnalysis)
  dataFrameForAnalysis
  ## Step 4. Mechanical work calculation per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  mechanicalWorkPerRevolution <- dplyr::summarise(dataFrameForAnalysis,
                                                   mechanicalWork = areaUnderCurve(xValue = angleValues, yValue = torqueValues))
  ## Step 5. Overall mechanical work.
  mechanicalWorkPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
    meanValue = mean(mechanicalWorkPerRevolution$mechanicalWork, na.rm = TRUE),
    sdValue = sd(mechanicalWorkPerRevolution$mechanicalWork, na.rm = TRUE),
    sampleNumber = sum(!is.na(mechanicalWorkPerRevolution$mechanicalWork))
  ))
  ## Step 6. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- mechanicalWorkPerRevolution
    scriptResultsList[[2]] <- mechanicalWorkPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- mechanicalWorkPerRevolution
    tempResultsList[[2]] <- mechanicalWorkPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}