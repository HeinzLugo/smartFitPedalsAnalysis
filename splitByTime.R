#' @description Groups the time stamps in segments of equal length to the 
#' time division value provided. 
#' @import plyr
#' @import dplyr
#' @import R.utils
#' @param timeValues: Vector with time values to split.
#' @param timeDivisionValueInMinutes: Time division value in which the column should be split. The value is specified in
#' minutes (e.g. 1, 1.5, 0.5)
#' @param timeValueType: Dimensional unit for the time value.
#' @return dataFrameOrganisedByTimeDivision: Processed data frame split by time division.
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
splitByTime <- function(timeValues, timeDivisionValueInMinutes, timeValueType = c("milliseconds","seconds", "minutes"))
{
  minutesToSecondsFactor <- 60 
  if((timeValueType != "milliseconds") && (timeValueType != "seconds") && (timeValueType != "minutes"))
  {
    stop("Time value should be milliseconds, seconds or minutes.")
  }
  if(timeValueType == "milliseconds")
  {
    millisecondsToSecondsFactor <- 1 / 1000
    originalTimeValues <- timeValues
    timeValues <- timeValues * millisecondsToSecondsFactor
  }
  if(timeValueType == "minutes")
  {
    originalTimeValues <- timeValues
    timeValues <- timeValues * minutesToSecondsFactor
  }
  ## Step 1. Split the data based on the time interval.
  timeFactorColumn <- ceiling(timeValues / (timeDivisionValueInMinutes * minutesToSecondsFactor))
  if(timeValueType == "seconds")
  {
    dataFrameOrganisedByTimeDivision <- dplyr::tbl_df(cbind.data.frame(timeValues, timeFactorColumn))
  }
  else
  {
    dataFrameOrganisedByTimeDivision <- dplyr::tbl_df(cbind.data.frame(originalTimeValues, timeValues, timeFactorColumn))
  }
dataFrameOrganisedByTimeDivision
}