#' @description Calculates the force vector magnitude and its angle 
#' with respect with radial/tangential reference frame.
#' A List containing both results is returned.
#' @import dplyr
#' @import circular
#' @param tangentialForceValues: Vector with tangential force values.
#' @param radialForceValues: Vector with radial angle values.
#' @param angleType: Dimensional unit for the returned angle value.
#' @return scriptResultsList: List with resulting force vector magnitude and angle.
#' The first item is the force and the second is the angle.
#' @warning For ploting and further analysis correction using the global reference frame
#' is required based on the location.
#' @author Heinz Lugo.
library(dplyr)
library(circular)
forceVectorCalculation <- function(tangentialForceValues, radialForceValues, angleType = c("degrees", "radians"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  ## Step 1. Vectorial addition.
  forceMagnitude <- sqrt((tangentialForceValues)^2 + (radialForceValues)^2)
  ## Step 2. Angle calculation.
  forceAngle <- atan(radialForceValues / tangentialForceValues)
  if(angleType == "degrees")
  {
    forceAngle <- circular::deg(forceAngle)
  }
  scriptResultsList <- list(forceMagnitude, forceAngle)
  scriptResultsList
}