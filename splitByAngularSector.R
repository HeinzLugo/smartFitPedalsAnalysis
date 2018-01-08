#' @description Groups the angle values in segments of equal length to the angular
#' division value.
#' @import plyr
#' @import dplyr
#' @import R.utils
#' @import splitByRevolution.R
#' @param angleValues: Vector with angle values to split.
#' @param angularDivisionValue: Angle sector in which the column should be split.
#' @param angleType: Dimensional unit for the angle value.
#' @return dataFrameOrganisedByAngleSector: Processed data frame split by angular sector.
#' @author Heinz Lugo.
library(R.utils)
## Safe load of plyr package.
if(isPackageLoaded("plyr") == FALSE)
{
  if(isPackageLoaded("dplyr") == TRUE)
  {
    detach("package:dplyr", unload=TRUE)
  }
  library(plyr)
  library(dplyr)
}
splitByAngularSector <- function(angleValues, angularDivisionValue, angleType = c("degrees", "radians"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  ## Step 1. Checks for the angular division value.
  if(angleType == "degrees")
  {
    if(angularDivisionValue > 360)
    {
      stop("The largest angular sector allowed is 360º.")
    }
  }
  else
  {
    radToDegFactor <- 180 / pi
    if(angularDivisionValue > 2 * pi)
    {
      stop("The maximum angular division allowed is 2pi.")
    }
    originalAngleValues <- angleValues
    angleValues <- angleValues * radToDegFactor
    angularDivisionValue <- angularDivisionValue * radToDegFactor
  }
  ## Step 2. Separate the angles by angular division value.
  angleSectorFactorColumn <- sapply(angleValues, function(x) ceiling(x / angularDivisionValue))
  if(angleType == "degrees")
  {
    dataFrameOrganisedByAngleSector <- dplyr::tbl_df(cbind.data.frame(angleValues, angleSectorFactorColumn))
  }
  else
  {
    dataFrameOrganisedByAngleSector <- dplyr::tbl_df(cbind.data.frame(originalAngleValues, angleValues, angleSectorFactorColumn))
    angularDivisionValue <- angularDivisionValue / radToDegFactor
  }
  dataFrameOrganisedByAngleSector$angleSectorFactorColumn <- angleSectorFactorColumn * angularDivisionValue
  ## Correct for angles which value is exactly 0º.
  dataFrameOrganisedByAngleSector$angleSectorFactorColumn[which(dataFrameOrganisedByAngleSector$angleValues == 0)] <- angularDivisionValue
  dataFrameOrganisedByAngleSector
}