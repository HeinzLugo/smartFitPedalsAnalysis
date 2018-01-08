#' @description Calculates the power value.
#' @import dplyr
#' @param forceValues: Vector with force values.
#' @param crankLength: Crank length in millimetres. By default the value used is 170mm.
#' @param cadenceValues: Vector with cadence values.
#' @param cadenceUnits: Dimensional unit for the cadence value.
#' @return  powerValues: Vector with power values.
#' @author Heinz Lugo.
library(dplyr)
powerCalculation <- function(forceValues, crankLength = 170, cadenceValues, cadenceUnits = c("rpm", "rad/s", "deg/s"))
{
  if((cadenceUnits != "rpm") && (cadenceUnits != "rad/s") && (cadenceUnits != "deg/s"))
  {
    stop("Cadence units should be rpm, rad/s or deg/s.")
  }
  rpmToRadPerSec <- 2 * pi / 60
  degPerSecToRadPerSec <- pi / 180
  millimetresToMetres <- 1 / 1000
  ## Step 1. Cadence values dimensional check.
  if(cadenceUnits == "rpm")
  {
    cadenceValues <- cadenceValues * rpmToRadPerSec
  }
  else if(cadenceUnits == "deg/s")
  {
    cadenceValues <- cadenceValues * degPerSecToRadPerSec
  }
  ## Step 2. Torque calculation.
  torqueValues <- forceValues * crankLength * millimetresToMetres
  ## Step 3. Power calculation.
  powerValues <- torqueValues * cadenceValues
  powerValues
}