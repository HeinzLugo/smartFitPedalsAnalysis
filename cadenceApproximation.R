#' @description Calculates the cadence per revolution based on the displacement covered within each revolution and the 
#' time it has taken to complete the revolution.
#' @import dplyr
#' @import splitByRevolution.R
#' @param angleValues: Vector with angle values to be split.
#' @param angleType: Dimensional unit for the angle value.
#' @param samplingFrequency: Sampling frequency. By default the sampling frequency is set to 500 Hz.
#' @param maximumAllowableCadence: Maximum allowable cadence for cadence evaluation. The dimensional units
#' of the cadence should be the same as the cadence units defined.
#' @param cadenceUnits: Dimensional unit for the cadence value.
#' @return cadenceValues: Cadence values dataframe.
#' @author Heinz Lugo.
library(dplyr)
source("splitByRevolution.R")
cadenceApproximation <- function(angleValues, angleType = c("degrees", "radians"), samplingFrequency = 500, maximumAllowableCadence, cadenceUnits = c("rpm", "rad/s", "deg/s"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((cadenceUnits != "rpm") && (cadenceUnits != "rad/s") && (cadenceUnits != "deg/s"))
  {
    stop("Cadence units should be rpm, rad/s or deg/s.")
  }
  deltaTime <- 1 / samplingFrequency
  degPerSecToRevPerMin <- 60 / 360
  degPerSecToRadPerSec <- pi / 180
  maxCorrectionAttempts <- 3
  correctionAttempts <- 1
  correctionIndex <- FALSE
  ## Step 1. Split the angle values by revolution.
  tempDataFrame <- splitByRevolution(angleValues = angleValues, angleType = angleType)
  ## Step 2. Calculate the cadence per revolution.
  cadenceValues <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
  cadenceValues <- dplyr::summarise(cadenceValues, 
                                    cadence = (max(angleValues) - min(angleValues)) / (length(revolutionFactorColumn) * deltaTime))
  if(cadenceUnits == "rpm")
  {
    cadenceValues$cadence <- cadenceValues$cadence *  degPerSecToRevPerMin
  }
  else if(cadenceUnits == "rad/s")
  {
    cadenceValues$cadence <- cadenceValues$cadence *  degPerSecToRadPerSec
  }
  cadenceValues <- merge(x = tempDataFrame, y = cadenceValues, by = "revolutionFactorColumn")
  ## Step 3. Cadence value correction.
  while(max(cadenceValues$cadence) > maximumAllowableCadence)
  {
    if(correctionAttempts > maxCorrectionAttempts)
    {
      correctionIndex <- TRUE
      warning("Cadence correction failure. The cadence for the previous compliant revolution is used.")
      indexes <- which(cadenceValues$cadence >= maximumAllowableCadence)
      if(length(indexes) != 0)
      {
        for(i in 1:length(indexes))
        {
          for(j in indexes[i] - 1:1)
          {
            if(cadenceValues$cadence[j] < maximumAllowableCadence)
            {
              cadenceValues$cadence[indexes[i]] <- cadenceValues$cadence[j]
              break
            }
          }
        }
      }
    }
    if(correctionIndex == FALSE)
    {
      cadenceValues <- cadenceFaultsCorrection(angleValues = cadenceValues$angleValues,
                                               angleType = angleType, samplingFrequency = samplingFrequency,
                                               cadenceValues = cadenceValues$cadence,
                                               maximumAllowableCadence = maximumAllowableCadence,
                                               cadenceUnits = cadenceUnits)
    }
    correctionAttempts <- correctionAttempts + 1  
  }
  cadenceValues
}

#' @description Corrects the cadence values based on the previous allowed cadence and its angle.
#' @import splitByRevolution.R
#' @param angleValues: Vector with angle values to be evaluated for correction.
#' @param angleType: Dimensional unit for the angle value.
#' @param samplingFrequency: Sampling frequency.
#' @param cadenceValues: Cadence values to be evaluate for correction.
#' @param maximumAllowableCadence: Maximum allowable cadence for cadence evaluation. The dimensional units
#' of the cadence should be the same as the cadence units defined.
#' @param cadenceUnits: Dimensional unit for the cadence value.
#' @return cadenceValuesCorrected: Corrected cadence values dataframe.
#' @author Heinz Lugo.
library(circular)
cadenceFaultsCorrection <- function(angleValues, angleType = c("degrees", "radians"), samplingFrequency, cadenceValues, maximumAllowableCadence, cadenceUnits = c("rpm", "rad/s", "deg/s"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((cadenceUnits != "rpm") && (cadenceUnits != "rad/s") && (cadenceUnits != "deg/s"))
  {
    stop("Cadence units should be rpm, rad/s or deg/s.")
  }
  deltaTime <- 1 / samplingFrequency
  degreesToRad <- pi / 180
  degPerSecToRevPerMin <- 60 / 360
  degPerSecToRadPerSec <- pi / 180
  revPerMinToDegPerSec <- 360 / 60
  radPerSecToDegPerSec <- 180 / pi
  ## Step 1. Check the length of the columns.
  if(length(angleValues) != length(cadenceValues))
  {
    stop("The length of the power and angle values columns must be the same.")
  }
  if(angleType == "degrees")
  {
    angleValues <- angleValues * degreesToRad
  }
  if(cadenceUnits == "rpm")
  {
    cadenceValues <- cadenceValues * revPerMinToDegPerSec * degPerSecToRadPerSec
    maximumAllowableCadence <- maximumAllowableCadence *  revPerMinToDegPerSec * degPerSecToRadPerSec
  }
  else if(cadenceUnits == "deg/s")
  {
    cadenceValues <- cadenceValues * degPerSecToRadPerSec
    maximumAllowableCadence <- maximumAllowableCadence *  degPerSecToRadPerSec
  }
  dataFrameForAnalysis <- cbind.data.frame(angleValues, cadenceValues)
  ## Step 2. Check which values cadence values are bigger than the allowed cadence.
  indexes <- which(dataFrameForAnalysis$cadenceValues >= maximumAllowableCadence)
  ## Step 3. Correct the angle values which do not comply with the criteria.
  if(length(indexes) != 0)
  {
    for(i in 1:length(indexes))
    {
      for(j in indexes[i] - 1:1)
      {
        if(dataFrameForAnalysis$cadenceValues[j] < maximumAllowableCadence)
        {
          correctionCadence <- dataFrameForAnalysis$cadenceValues[j]
          initialAngle <- circular::circular(x = dataFrameForAnalysis$angleValues[j], type = "angles",units = "radians",modulo = "2pi", rotation = "clock")
          timeDifference <- (indexes[i] - j) * deltaTime
          deltaAngle <- circular::circular(x = correctionCadence * timeDifference, type = "angles",units = "radians",modulo = "2pi", rotation = "clock")
          dataFrameForAnalysis$angleValues[indexes[i]] <- (initialAngle + deltaAngle)[[1]]
          break
        }
      }
    }
  }
  ## Step 4. Split the angle values by revolution.
  tempDataFrame <- splitByRevolution(angleValues = dataFrameForAnalysis$angleValues, angleType = "radians")
  if(angleType == "degrees")
  {
    tempDataFrame <- dplyr::select(tempDataFrame, -originalAngleValues)
  }
  ## Step 5. Calculate the cadence per revolution.
  cadenceValues <- dplyr::group_by(tempDataFrame, revolutionFactorColumn)
  cadenceValues <- dplyr::summarise(cadenceValues, 
                                    cadence = (max(angleValues) - min(angleValues)) / (length(revolutionFactorColumn) * deltaTime))
  if(cadenceUnits == "rpm")
  {
    cadenceValues$cadence <- cadenceValues$cadence *  degPerSecToRevPerMin
  }
  else if(cadenceUnits == "rad/s")
  {
    cadenceValues$cadence <- cadenceValues$cadence *  degPerSecToRadPerSec
  }
  cadenceValues <- merge(x = tempDataFrame, y = cadenceValues, by = "revolutionFactorColumn")
  cadenceValues
}