#' @description Calculates the pedalling efficiency magnitude per revolution and the
#' overall pedalling efficiency distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import splitByRevolution.R
#' @param tangentialForceValues: Vector with tangential force values.
#' @param radialForceValues: Vector with radial force values.
#' @param angleValues: Vector with angle values.
#' @param angleType: Dimensional unit for the angle value.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo
pedallingEfficiency <- function(tangentialForceValues, radialForceValues,  angleValues, angleType = c("degrees", "radians"), analysisType = c("angle", "time"))
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
  if(length(tangentialForceValues) != length(radialForceValues))
  {
    stop("The length of the tangential and radial force values columns must be the same.")
  }
  if(length(tangentialForceValues) != length(angleValues))
  {
    stop("The length of the force values and angle values columns must be the same.")
  }
  ## Step 2. Split angle by revolution.
  if(angleType == "degrees")
  {
    angleValues <- angleValues * degToRad
  }
  dataFrameForAnalysis <- splitByRevolution(angleValues = angleValues, angleType = "radians")
  dataFrameForAnalysis <- dplyr::select(dataFrameForAnalysis, angleValues = originalAngleValues, revolutionFactorColumn)
  dataFrameForAnalysis <- cbind.data.frame(radialForceValues, tangentialForceValues, dataFrameForAnalysis)
  ## Step 3. Pedalling efficiency calculation per revolution.
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  pedallingEfficiencyPerRevolution <- dplyr::summarise(dataFrameForAnalysis, 
                                                   pedallingEfficiency = pedallingEfficiencyCalculation(signalRadialValues = radialForceValues,
                                                                                                        signalTangentialValues = tangentialForceValues))
  ## Step 4. Overall pedealling efficiency .
  pedallingEfficiencyPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
    meanValue = mean(pedallingEfficiencyPerRevolution$pedallingEfficiency, na.rm = TRUE),
    sdValue = sd(pedallingEfficiencyPerRevolution$pedallingEfficiency, na.rm = TRUE),
    sampleNumber = sum(!is.na(pedallingEfficiencyPerRevolution$pedallingEfficiency))
  ))
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- pedallingEfficiencyPerRevolution
    scriptResultsList[[2]] <- pedallingEfficiencyPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- pedallingEfficiencyPerRevolution
    tempResultsList[[2]] <- pedallingEfficiencyPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Actual pedalling efficiency calculation.
#' @param signalRadialValues: Vector with signal radial values.
#' @param signalTangentialValues: Vector with signal tangential values.
#' @return pedallingEfficiency: Pedalling efficiency value.
#' @author Heinz Lugo.
pedallingEfficiencyCalculation <- function(signalRadialValues, signalTangentialValues)
{
  sumForceMagnitude <- sum(sqrt((signalRadialValues)^2 + (signalTangentialValues)^2))
  sumForceTangential <- sum(signalTangentialValues)
  pedallingEfficiency <- sumForceTangential / sumForceMagnitude
  if(pedallingEfficiency > 1)
  {
    pedallingEfficiency <- 1
  }
  if(pedallingEfficiency < 0)
  {
    pedallingEfficiency <- 0
  }
  pedallingEfficiency
}