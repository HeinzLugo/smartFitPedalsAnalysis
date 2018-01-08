#' @description Returns the distribution for the magnitude of the values provided
#' organised per angle division.
#' @import dplyr
#' @import splitByAngularSector.R
#' @param valueColumn: Vector containing the value which distribution will be calculated.
#' @param angleValues: Vector with angle values.
#' @param angularDivisionValue: Angle sector in which the column should be split.
#' @param angleType: Dimensional unit for the angle value.
#' @return scriptResultsList: List with resulting dataframes.
#' @reminder The scripts return mean values, standard deviation and the number of samples. Upper and lower limits can be found
#' using these values.
#' @reminder The largest angular sector possible is 360º.
#' @author Heinz Lugo.
library(dplyr)
source("splitByAngularSector.R")
valueSummaryPerAngleSector <- function(valueColumn, angleValues, angularDivisionValue, angleType = c("degrees", "radians"))
{
  if((angleType != "degrees") & (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  ## Step 1. Create the analysis dataframe.
  if(length(valueColumn) != length(angleValues))
  {
    stop("The length of the angle and value columns must be the same.")
  }
  if(angleType == "degrees")
  {
    if(angularDivisionValue > 360)
    {
      stop("The largest angular sector allowed is 360º.")
    }
  }
  else
  {
    if(angularDivisionValue > 2 * pi)
    {
      stop("The maximum angular division allowed is 2pi.")
    }
  }
  ## Step 2. Split by angular sector.
  dataFrameForAnalysis <- splitByAngularSector(angleValues = angleValues, angularDivisionValue = angularDivisionValue, angleType = angleType) %>%
    dplyr::select(angleSectorFactorColumn)
  ## Step 3. Calculate the value distribution.
  dataFrameForAnalysis <- cbind.data.frame(valueColumn, dataFrameForAnalysis) %>%
    dplyr::group_by(angleSectorFactorColumn)
  resultsDataFrame <- dplyr::summarise(dataFrameForAnalysis, meanValue = mean(valueColumn, na.rm = TRUE), 
                                       sdValue = sd(valueColumn, na.rm = TRUE),
                                       sampleNumber = sum(!is.na(valueColumn)))
  ## Step 4. Result list.
  scriptResultsList <- vector("list", 1) 
  scriptResultsList[[1]] <- resultsDataFrame
  scriptResultsList
}