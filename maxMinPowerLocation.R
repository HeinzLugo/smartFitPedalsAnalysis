#' @description Returns the maximum and minimum power magnitude and location distribution per revolution and 
#' the overall values distribution. Results are returned as a list of dataframes the first one containing
#' the results per revolution and the second one containing the overall results.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @import dplyr
#' @import maxMinValueLocations.R
#' @import circular
#' @param powerValues: Power values.
#' @param angleValues: Angle values.
#' @param angleType: Type of the of the angle values.
#' @param powerType: Sets if the power values are coming from a single leg or as the net contribution of both legs.
#' @param analysisType: Type of analysis to be conducted either angle or time. This parameter only affects the format
#' in which the results are returned the actual grouping must be performed before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @author Heinz Lugo.
library(dplyr)
library(circular)
source("splitByRevolution.R")
source("maxMinValueLocations.R")
maxMinPowerLocation <- function(powerValues, angleValues, angleType = c("degrees", "radians"), powerType = c("individual", "net"), analysisType = c("angle", "time"))
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
  ## Step 3. Maximum power and location per revolution.
  if(powerType == "individual")
  {
    tempDataFrame <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
    maxMinPowerLocationPerRevolution <- dplyr::summarise(tempDataFrame,
                                                         maxMinValueLocationList = maxMinValueLocations(signalXValue = angleValues, signalYValue = powerValues, numberTerms = 1))
    maxMinPowerLocationPerRevolution <- dplyr::mutate(maxMinPowerLocationPerRevolution,
                                                      maxValue = sapply(X = maxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "maximum"),
                                                      maxValueLocation = sapply(X = maxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "maximum"),
                                                      minValue = sapply(X = maxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "minimum"),
                                                      minValueLocation = sapply(X = maxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "minimum")
                                                      )
    maxMinPowerLocationPerRevolution <- dplyr::select(maxMinPowerLocationPerRevolution, -maxMinValueLocationList)
    maxMinPowerLocationPerRevolution$maxValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$maxValueLocation,
                                                              type = "angles", units = "radians", template = "none",
                                                              modulo = "2pi", zero = pi/2, rotation = "clock")
    maxMinPowerLocationPerRevolution$minValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$minValueLocation,
                                                                            type = "angles", units = "radians", template = "none",
                                                                            modulo = "2pi", zero = pi/2, rotation = "clock")
    maxMinPowerLocationPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      meanMaxValue =  mean(maxMinPowerLocationPerRevolution$maxValue, na.rm = TRUE),
      sdMaxValue = sd(maxMinPowerLocationPerRevolution$maxValue, na.rm = TRUE),
      sampleNumberMaxValue = sum(!is.na(maxMinPowerLocationPerRevolution$maxValue)),
      meanMaxValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$maxValueLocation, na.rm = TRUE),
      sdMaxValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$maxValueLocation, na.rm = TRUE),
      sampleNumberMaxValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$maxValueLocation)),
      meanMinValue =  mean(maxMinPowerLocationPerRevolution$minValue, na.rm = TRUE),
      sdMinValue = sd(maxMinPowerLocationPerRevolution$minValue, na.rm = TRUE),
      sampleNumberMinValue = sum(!is.na(maxMinPowerLocationPerRevolution$minValue)),
      meanMinValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$minValueLocation, na.rm = TRUE),
      sdMinValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$minValueLocation, na.rm = TRUE),
      sampleNumberMinValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$minValueLocation))
    ))
    if(angleType == "degrees")
    {
      maxMinPowerLocationPerRevolution$maxValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$maxValueLocation)
      maxMinPowerLocationPerRevolution$minValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$minValueLocation)
      maxMinPowerLocationPerRevolutionSummary$meanMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$meanMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$sdMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$sdMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$meanMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$meanMinValueLocation)
      maxMinPowerLocationPerRevolutionSummary$sdMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$sdMinValueLocation)
    }
  }
  else
  {
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues <= rightLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    rightMaxMinPowerLocationPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                              rightMaxMinValueLocationList = maxMinValueLocations(signalXValue = angleValues, signalYValue = powerValues, numberTerms = 1))
    rightMaxMinPowerLocationPerRevolution <- dplyr::mutate(rightMaxMinPowerLocationPerRevolution,
                                                      rightMaxValue = sapply(X = rightMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "maximum"),
                                                      rightMaxValueLocation = sapply(X = rightMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "maximum"),
                                                      rightMinValue = sapply(X = rightMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "minimum"),
                                                      rightMinValueLocation = sapply(X = rightMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "minimum"))
    tempDataFrame <- dplyr::filter(dataFrameForAnalysis, angleValues >= leftLegAngleLimit)
    tempDataFrame <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
    leftMaxMinPowerLocationPerRevolution <- dplyr::summarise(tempDataFrame, 
                                                       leftMaxMinValueLocationList = maxMinValueLocations(signalXValue = angleValues, signalYValue = powerValues, numberTerms = 1))
    leftMaxMinPowerLocationPerRevolution <- dplyr::mutate(leftMaxMinPowerLocationPerRevolution,
                                                          leftMaxValue = sapply(X = leftMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "maximum"),
                                                          leftMaxValueLocation = sapply(X = leftMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "maximum"),
                                                          leftMinValue = sapply(X = leftMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "value", identifierMagnitude = "minimum"),
                                                          leftMinValueLocation = sapply(X = leftMaxMinValueLocationList, FUN = decodeMaxMinPowerLocationPerRevolutionList, typeMagnitude = "location", identifierMagnitude = "minimum"))
    if(nrow(rightMaxMinPowerLocationPerRevolution) <= nrow(leftMaxMinPowerLocationPerRevolution))
    {
      maxMinPowerLocationPerRevolution <- merge(x = leftMaxMinPowerLocationPerRevolution, y = rightMaxMinPowerLocationPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE) %>%
        dplyr::select(-c(leftMaxMinValueLocationList, rightMaxMinValueLocationList))
    }
    else
    {
      maxMinPowerLocationPerRevolution <- merge(x = rightMaxMinPowerLocationPerRevolution, y = leftMaxMinPowerLocationPerRevolution, by = 'revolutionFactorColumn', all.x = TRUE) %>%
        dplyr::select(-c(leftMaxMinValueLocationList, rightMaxMinValueLocationList))
    }
    maxMinPowerLocationPerRevolution$rightMaxValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$rightMaxValueLocation,
                                                                            type = "angles", units = "radians", template = "none",
                                                                            modulo = "2pi", zero = pi/2, rotation = "clock")
    maxMinPowerLocationPerRevolution$rightMinValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$rightMinValueLocation,
                                                                            type = "angles", units = "radians", template = "none",
                                                                            modulo = "2pi", zero = pi/2, rotation = "clock")
    maxMinPowerLocationPerRevolution$leftMaxValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$leftMaxValueLocation,
                                                                                 type = "angles", units = "radians", template = "none",
                                                                                 modulo = "2pi", zero = pi/2, rotation = "clock")
    maxMinPowerLocationPerRevolution$leftMinValueLocation <- circular::circular(x = maxMinPowerLocationPerRevolution$leftMinValueLocation,
                                                                                 type = "angles", units = "radians", template = "none",
                                                                                 modulo = "2pi", zero = pi/2, rotation = "clock")
    
    maxMinPowerLocationPerRevolutionSummary <- dplyr::tbl_df(cbind.data.frame(
      rightMeanMaxValue =  mean(maxMinPowerLocationPerRevolution$rightMaxValue, na.rm = TRUE),
      rightSDMaxValue = sd(maxMinPowerLocationPerRevolution$rightMaxValue, na.rm = TRUE),
      rightSampleNumberMaxValue = sum(!is.na(maxMinPowerLocationPerRevolution$rightMaxValue)),
      rightMeanMaxValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$rightMaxValueLocation, na.rm = TRUE),
      rightSDMaxValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$rightMaxValueLocation, na.rm = TRUE),
      rightSampleNumberMaxValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$rightMaxValueLocation)),
      rightMeanMinValue =  mean(maxMinPowerLocationPerRevolution$rightMinValue, na.rm = TRUE),
      rightSDMinValue = sd(maxMinPowerLocationPerRevolution$rightMinValue, na.rm = TRUE),
      rightSampleNumberMinValue = sum(!is.na(maxMinPowerLocationPerRevolution$rightMinValue)),
      rightMeanMinValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$rightMinValueLocation, na.rm = TRUE),
      rightSDMinValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$rightMinValueLocation, na.rm = TRUE),
      rightSampleNumberMinValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$rightMinValueLocation)),
      ## Left data.
      leftMeanMaxValue =  mean(maxMinPowerLocationPerRevolution$leftMaxValue, na.rm = TRUE),
      leftSDMaxValue = sd(maxMinPowerLocationPerRevolution$leftMaxValue, na.rm = TRUE),
      leftSampleNumberMaxValue = sum(!is.na(maxMinPowerLocationPerRevolution$leftMaxValue)),
      leftMeanMaxValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$leftMaxValueLocation, na.rm = TRUE),
      leftSDMaxValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$leftMaxValueLocation, na.rm = TRUE),
      leftSampleNumberMaxValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$leftMaxValueLocation)),
      leftMeanMinValue =  mean(maxMinPowerLocationPerRevolution$leftMinValue, na.rm = TRUE),
      leftSDMinValue = sd(maxMinPowerLocationPerRevolution$leftMinValue, na.rm = TRUE),
      leftSampleNumberMinValue = sum(!is.na(maxMinPowerLocationPerRevolution$leftMinValue)),
      leftMeanMinValueLocation = circular::mean.circular(x = maxMinPowerLocationPerRevolution$leftMinValueLocation, na.rm = TRUE),
      leftSDMinValueLocation = circular::sd.circular(x = maxMinPowerLocationPerRevolution$leftMinValueLocation, na.rm = TRUE),
      leftSampleNumberMinValueLocation = sum(!is.na(maxMinPowerLocationPerRevolution$leftMinValueLocation))
    ))
    if(angleType == "degrees")
    {
      maxMinPowerLocationPerRevolution$rightMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$rightMaxValueLocation)
      maxMinPowerLocationPerRevolution$rightMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$rightMinValueLocation)
      maxMinPowerLocationPerRevolutionSummary$rightMeanMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$rightMeanMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$rightSDMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$rightSDMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$rightMeanMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$rightMeanMinValueLocation)
      maxMinPowerLocationPerRevolutionSummary$rightSDMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$rightSDMinValueLocation)
      ## Left leg.
      maxMinPowerLocationPerRevolution$leftMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$leftMaxValueLocation)
      maxMinPowerLocationPerRevolution$leftMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolution$leftMinValueLocation)
      maxMinPowerLocationPerRevolutionSummary$leftMeanMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$leftMeanMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$leftSDMaxValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$leftSDMaxValueLocation)
      maxMinPowerLocationPerRevolutionSummary$leftMeanMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$leftMeanMinValueLocation)
      maxMinPowerLocationPerRevolutionSummary$leftSDMinValueLocation <- circular::deg(maxMinPowerLocationPerRevolutionSummary$leftSDMinValueLocation)
    }
  }
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 2) 
    scriptResultsList[[1]] <- maxMinPowerLocationPerRevolution
    scriptResultsList[[2]] <- maxMinPowerLocationPerRevolutionSummary
  }
  else
  {
    tempResultsList <- vector(mode = "list", 2)
    tempResultsList[[1]] <- maxMinPowerLocationPerRevolution
    tempResultsList[[2]] <- maxMinPowerLocationPerRevolutionSummary
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}

#' @description Decodes the maximum and minimum power location and value.
#' @import dplyr
#' @param maxMinValueLocationList: List to decode the values from.
#' @param typeMagnitude: Identifier to choose which value return.
#' @param identifierMagnitude: Identifier to choose if maximum or minimum values are returned.
#' @return decodedParameter: Parameter retrieved from the list.
#' @author Heinz Lugo.
decodeMaxMinPowerLocationPerRevolutionList <- function(maxMinValueLocationList, typeMagnitude = c("value", "location"), identifierMagnitude = c("maximum", "minimum"))
{
  tempDataFrame <- maxMinValueLocationList
  if(identifierMagnitude == "maximum")
  {
    tempDataFrame <- dplyr::filter(tempDataFrame, identifier == "maximum")
  }
  else if(identifierMagnitude == "minimum")
  {
    tempDataFrame <- dplyr::filter(tempDataFrame, identifier == "minimum")
  }
  if(typeMagnitude == "value")
  {
    decodedParameter <- tempDataFrame$value[1]
  }
  else
  {
    decodedParameter <- tempDataFrame$location[1]
  }
  decodedParameter
}