#' @description Returns which revolution an angle value belongs to.
#' @import plyr
#' @import dplyr
#' @import R.utils
#' @param angleValues: Vector with angle values to split.
#' @param angleType: Dimensional unit for the angle value.
#' @return dataFrameOrganisedByRevolution: Processed data frame split by revolution is returned.
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
splitByRevolution <- function(angleValues, angleType = c("degrees", "radians"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if(angleType == "radians")
  {
    radToDegFactor <- 180 / pi
    originalAngleValues <- angleValues
    angleValues <- angleValues * radToDegFactor
  }
  ## Step 1. Determine which revolution the angle belongs to.
  revolutionFactorColumn <- vector("numeric", length = length(angleValues))
  approxValues <- plyr::round_any(angleValues, 5, f = ceiling)
  revolution <- 1
  for(i in 1:length(approxValues))
  {
    if(i != 1)
    {
      difference = approxValues[i] - approxValues[i -1]
      if(difference < 0 && abs(difference) > 270)
      {
        revolution <- revolution + 1
      }
    }
    revolutionFactorColumn[i]  <- revolution
  }
  if(angleType == "degrees")
  {
    dataFrameOrganisedByRevolution <- dplyr::tbl_df(cbind.data.frame(angleValues, revolutionFactorColumn))
  }
  else if(angleType == "radians")
  {
    dataFrameOrganisedByRevolution <- dplyr::tbl_df(cbind.data.frame(originalAngleValues, angleValues, revolutionFactorColumn))
  }
  dataFrameOrganisedByRevolution
}
