#' @description This script analyses the raw data captured by the Smartfit system.
#' If new functionality or data analysis steps are to be performed functions or scripts needed should
#' be called from this script.
#' @reminder Change the load and store folder paths to where the data and the results will be stored.
#' @author Heinz Lugo.
## Step 1. Set the raw data load and results storage folder paths.
loadFolderPath <- "CHANGE TO LOCATION OF RAW DATA"
storeFolderPath <- "CHANGE TO LOCATION TO STORE DATA"
##-------CHANGE PARAMETERS AS REQUIRED------- ##
## Parameter definitions. ##
samplingFrequency <- 500
columnNames <- c("digitalTrigger", "leftTangentialForce", "leftRadialForce", "rightTangentialForce", "rightRadialForce", "position")
numRowsExcluded <- 8
crankLength <- 170
angleType <- "degrees"
cadenceUnits <- "rpm"
angularDivisionValue <- 5
millimetresToMetres <- 1 / 1000
powerType <- "net"
analysisType <- "time"
timeDivisionValueInMinutes <- 1
timeValueType <- "seconds"
maximumAllowableCadence <- 300
##------------------------------------------ ##
## Step 2. Preprocess the raw torque data.
source("torqueFilePreProcessing.R")
source("cadenceApproximation.R")
source("powerCalculation.R")
source("forceVectorCalculation.R")
fileNamePath <- paste(loadFolderPath, "Right_suspended.xls", sep = "/")
## Step 2.1. Torque file preprocessing.
rawTable <- torqueFilePreProcessing(filePath = fileNamePath,
                                         columnNames = columnNames,
                                         samplingFrequency = samplingFrequency,
                                         numRowsExcluded = numRowsExcluded)
## Step 2.2. Cadence value approximation.
rawTable <- dplyr::mutate(rawTable, cadence = cadenceApproximation(angleValues = position,
                                                         angleType = angleType,
                                                         samplingFrequency = samplingFrequency,
                                                         cadenceUnits = cadenceUnits,
                                                         maximumAllowableCadence = maximumAllowableCadence)$cadence)
## Step 2.3. Power calculation.
rawTable <- dplyr::mutate(rawTable, leftPower = powerCalculation(forceValues = leftTangentialForce,
                                                                 crankLength = crankLength,
                                                                 cadenceValues = cadence,
                                                                 cadenceUnits = cadenceUnits),
                          rightPower = powerCalculation(forceValues = rightTangentialForce,
                                                       crankLength = crankLength,
                                                       cadenceValues = cadence,
                                                       cadenceUnits = cadenceUnits),
                          netPower = leftPower + rightPower)
## Step 2.4. Force vector calculation.
temp <- forceVectorCalculation(tangentialForceValues = rawTable$leftRadialForce, radialForceValues = rawTable$leftTangentialForce, angleType = angleType)
rawTable <- dplyr::mutate(rawTable, leftForceVectorMagnitude = temp[[1]], leftForceVectorAngle = temp[[2]])
temp <- forceVectorCalculation(tangentialForceValues = rawTable$rightRadialForce, radialForceValues = rawTable$rightTangentialForce, angleType = angleType)
rawTable <- dplyr::mutate(rawTable, rightForceVectorMagnitude = temp[[1]], rightForceVectorAngle = temp[[2]])
## Step 3. Pedalling characteristics analysis.
source("torqueEffectiveness.R")
source("pedalSmoothness.R")
source("powerBalance.R")
source("valueSummaryPerAngleSector.R")
source("valueSummaryPerRevolution.R")
source("normalisedPower.R")
source("cadenceDistribution.R")
source("rateOfTorqueApplication.R")
source("mechanicalWork.R")
source("fwhmPower.R")
source("riseTimePower.R")
source("maxMinPowerLocation.R")
source("pedallingEfficiency.R")
source("valueSummaryPerAngleSectorPerRevolution.R")
## Step 3.1. Split the data by time interval if required.
if(analysisType == "time")
{
  source("splitByTime.R")
  rawTable <- dplyr::mutate(rawTable, 
                            timeFactorColumn = splitByTime(timeValues = timeStamp,
                                                           timeDivisionValueInMinutes = timeDivisionValueInMinutes,
                                                           timeValueType = timeValueType)$timeFactorColumn)
  rawTable <- dplyr::group_by(rawTable, timeFactorColumn)
  ## Step 3.1.1. Torque effectiveness.
  leftTorqueEffectiveness <- dplyr::summarise(rawTable, leftTorqueEffectiveness = torqueEffectiveness(powerValues = leftPower, angleValues = position, angleType = angleType, analysisType = analysisType))
  rightTorqueEffectiveness <- dplyr::summarise(rawTable, rightTorqueEffectiveness = torqueEffectiveness(powerValues = rightPower, angleValues = position, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.2 Pedal smoothness.
  leftPedalSmoothness <- dplyr::summarise(rawTable, leftPedalSmoothness = pedalSmoothness(powerValues = leftPower, angleValues = position, angleType = angleType, analysisType = analysisType))
  rightPedalSmoothness <- dplyr::summarise(rawTable, rightPedalSmoothness = pedalSmoothness(powerValues = rightPower, angleValues = position, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.3. Power balance.
  powerBalance <- dplyr::summarise(rawTable, powerBalance = powerBalance(leftPowerValues = leftPower, rightPowerValues = rightPower, angleValues = position, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.4. Net power per angle sector.
  powerAngleSector <- dplyr::summarise(rawTable, powerAngleSector = valueSummaryPerAngleSector(valueColumn = netPower, angleValues = position, angularDivisionValue = angularDivisionValue, angleType = angleType))
  ## Step 3.1.5. Net power per revolution.
  powerRevolution <- dplyr::summarise(rawTable, powerRevolution = valueSummaryPerRevolution(valueColumn = netPower, angleValues = position, angleType = angleType))
  ## Step 3.1.6. Net normalised power.
  normalisedPower <- dplyr::summarise(rawTable, normalisedPower = normalisedPower(powerValues = netPower, samplingFrequency = samplingFrequency))
  ## Step 3.1.7. Cadence distribution.
  cadenceDistribution <- dplyr::summarise(rawTable, cadenceDistribution = cadenceDistribution(cadenceValues = cadence, angleValues = position, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.8. Rate of torque application.
  rateOfTorqueApplication <- dplyr::summarise(rawTable, rateOfTorqueApplication = rateOfTorqueApplication(torqueValues = (leftTangentialForce + rightTangentialForce) * crankLength * millimetresToMetres,
                                              angleValues = position, angularDivisionValue = angularDivisionValue, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.9 Mechanical work.
  mechanicalWork <- dplyr::summarise(rawTable, mechanicalWork = mechanicalWork(torqueValues = (leftTangentialForce + rightTangentialForce) * crankLength * millimetresToMetres,
                                                                               angleValues = position, angleType = angleType, cadenceValues = cadence,
                                                                               cadenceUnits = cadenceUnits, analysisType = analysisType))
  ## Step 3.1.10. FWHM power.
  fwhmPower <- dplyr::summarise(rawTable, fwhmPower = fwhmPower(powerValues = netPower, angleValues = position, angleType = angleType, powerType = powerType, analysisType = analysisType))
  ## Step 3.1.11. Rise time power.
  riseTimePower <- dplyr::summarise(rawTable, riseTimePower = riseTimePower(powerValues = netPower, angleValues = position, angleType = angleType, powerType = powerType, analysisType = analysisType))
  ## Step 3.1.12. Maximum and minimum power magnitude and location.
  maxMinPowerLocation <- dplyr::summarise(rawTable, maxMinPowerLocation = maxMinPowerLocation(powerValues = netPower, angleValues = position, angleType = angleType, powerType = powerType, analysisType = analysisType))
  ## Step 3.1.13. Pedalling efficiency.
  pedallingEfficiency <- dplyr::summarise(rawTable, pedallingEfficiency = pedallingEfficiency(tangentialForceValues = rightTangentialForce + leftTangentialForce, radialForceValues = rightRadialForce + leftRadialForce, angleValues = position, angleType = angleType, analysisType = analysisType))
  ## Step 3.1.14. Power per angle sector per revolution.
  powerPerAngleSectorPerRevolution <- dplyr::summarise(rawTable, powerPerAngleSectorPerRevolution = valueSummaryPerAngleSectorPerRevolution(valueColumn = netPower, angleValues = position, angularDivisionValue = angularDivisionValue, angleType = angleType, analysisType = analysisType))
}else
{
  ## Step 3.2.1. Torque effectiveness.
  leftTorqueEffectiveness <- torqueEffectiveness(powerValues = rawTable$leftPower, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  rightTorqueEffectiveness <- torqueEffectiveness(powerValues = rawTable$rightPower, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.2 Pedal smoothness.
  leftPedalSmoothness <- pedalSmoothness(powerValues = rawTable$leftPower, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  rightPedalSmoothness <- pedalSmoothness(powerValues = rawTable$rightPower, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.3. Power balance.
  powerBalance <- powerBalance(leftPowerValues = rawTable$leftPower, rightPowerValues = rawTable$rightPower, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.4. Net power per angle sector.
  powerAngleSector <- valueSummaryPerAngleSector(valueColumn = rawTable$netPower, angleValues = rawTable$position, angularDivisionValue = angularDivisionValue, angleType = angleType)
  ## Step 3.2.5. Net power per revolution.
  powerRevolution <- valueSummaryPerRevolution(valueColumn = rawTable$netPower, angleValues = rawTable$position, angleType = angleType)
  ## Step 3.2.6. Net normalised power.
  tempDataFrame <- dplyr::select(rawTable, timeStamp, netPower) %>%
    dplyr::mutate(timeFactorColumn = splitByTime(timeValues = timeStamp,
                                                 timeDivisionValueInMinutes = timeDivisionValueInMinutes,
                                                 timeValueType = timeValueType)$timeFactorColumn) %>%
    dplyr::group_by(timeFactorColumn)
  normalisedPower <- dplyr::summarise(tempDataFrame, normalisedPower = normalisedPower(powerValues = netPower, samplingFrequency = samplingFrequency))
  ## Step 3.2.7. Cadence distribution.
  cadenceDistribution <- cadenceDistribution(cadenceValues = rawTable$cadence, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.8. Rate of torque application.
  rateOfTorqueApplication <- rateOfTorqueApplication(torqueValues = (rawTable$leftTangentialForce + rawTable$rightTangentialForce) * crankLength * millimetresToMetres,
                                                     angleValues = rawTable$position, angularDivisionValue = angularDivisionValue, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.9 Mechanical work.
  mechanicalWork <- mechanicalWork(torqueValues = (rawTable$leftTangentialForce + rawTable$rightTangentialForce) * crankLength * millimetresToMetres,
                                   angleValues = rawTable$position, angleType = angleType, cadenceValues = rawTable$cadence, cadenceUnits = cadenceUnits,
                                   analysisType = analysisType)
  ## Step 3.2.10. FWHM power.
  fwhmPower <- fwhmPower(powerValues = rawTable$netPower, angleValues = rawTable$position, angleType = angleType, powerType = powerType, analysisType = analysisType)
  ## Step 3.2.11. Rise time power.
  riseTimePower <- riseTimePower(powerValues = rawTable$netPower, angleValues = rawTable$position, angleType = angleType, powerType = powerType, analysisType = analysisType)
  ## Step 3.2.12. Maximum and minimum power magnitude and location.
  maxMinPowerLocation <- maxMinPowerLocation(powerValues = rawTable$netPower, angleValues = rawTable$position, angleType = angleType, powerType = powerType, analysisType = analysisType)
  ## Step 3.2.13. Pedalling efficiency.
  pedallingEfficiency <- pedallingEfficiency(tangentialForceValues = rawTable$rightTangentialForce + rawTable$leftTangentialForce, radialForceValues = rawTable$rightRadialForce + rawTable$leftRadialForce, angleValues = rawTable$position, angleType = angleType, analysisType = analysisType)
  ## Step 3.2.14. Power per angle sector per revolution.
  powerPerAngleSectorPerRevolution <- valueSummaryPerAngleSectorPerRevolution(valueColumn = rawTable$netPower, angleValues = rawTable$position, angularDivisionValue = angularDivisionValue, angleType = angleType, analysisType = analysisType)
}

## SAVING OF RDS ##
saveRDS(object = leftTorqueEffectiveness, file = paste(storeFolderPath, "leftTorqueEffectiveness.Rds", sep = "/"))
saveRDS(object = rightTorqueEffectiveness, file = paste(storeFolderPath, "rightTorqueEffectiveness.Rds", sep = "/"))
saveRDS(object = leftPedalSmoothness, file = paste(storeFolderPath, "leftPedalSmoothness.Rds", sep = "/"))
saveRDS(object = rightPedalSmoothness, file = paste(storeFolderPath, "rightPedalSmoothness.Rds", sep = "/"))
saveRDS(object = powerBalance, file = paste(storeFolderPath, "powerBalance.Rds", sep = "/"))
saveRDS(object = powerAngleSector, file = paste(storeFolderPath, "powerAngleSector.Rds", sep = "/"))
saveRDS(object = powerRevolution, file = paste(storeFolderPath, "powerRevolution.Rds", sep = "/"))
saveRDS(object = normalisedPower, file = paste(storeFolderPath, "normalisedPower.Rds", sep = "/"))
saveRDS(object = cadenceDistribution, file = paste(storeFolderPath, "cadenceDistribution.Rds", sep = "/"))
saveRDS(object = rateOfTorqueApplication, file = paste(storeFolderPath, "rateOfTorqueApplication.Rds", sep = "/"))
saveRDS(object = mechanicalWork, file = paste(storeFolderPath, "mechanicalWork.Rds", sep = "/"))
saveRDS(object = fwhmPower, file = paste(storeFolderPath, "fwhmPower.Rds", sep = "/"))
saveRDS(object = riseTimePower, file = paste(storeFolderPath, "riseTimePower.Rds", sep = "/"))
saveRDS(object = maxMinPowerLocation, file = paste(storeFolderPath, "maxMinPowerLocation.Rds", sep = "/"))
saveRDS(object = pedallingEfficiency, file = paste(storeFolderPath, "pedallingEfficiency.Rds", sep = "/"))
saveRDS(object = powerPerAngleSectorPerRevolution, file = paste(storeFolderPath, "powerPerAngleSectorPerRevolution.Rds", sep = "/"))