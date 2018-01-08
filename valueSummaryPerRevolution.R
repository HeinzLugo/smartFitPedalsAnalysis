#' @description Returns the distribution for the magnitude of the values provided
#' organised per revolution.
#' @import dplyr
#' @import splitByRevolution.R
#' @param valueColumn: Vector containing the value which distribution will be calculated.
#' @param angleValues: Vector with angle values.
#' @param angularDivisionValue: Angle sector in which the column should be split.
#' @param angleType: Dimensional unit for the angle value.
#' @return scriptResultsList: List with resulting dataframes.
#' @reminder The scripts return mean values, standard deviation and the number of samples. Upper and lower limits can be found
#' using these values.
#' @author Heinz Lugo.
library(dplyr)
source("splitByRevolution.R")
valueSummaryPerRevolution <- function(valueColumn, angleValues, angleType = c("degrees", "radians"))
{
  if((angleType != "degrees") && (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  ## Step 1. Create the analysis dataframe.
  if(length(valueColumn) != length(angleValues))
  {
    stop("The length of the angle and value columns must be the same.")
  }
  dataFrameForAnalysis <- splitByRevolution(angleValues, angleType)
  dataFrameForAnalysis <- cbind.data.frame(valueColumn, dataFrameForAnalysis)
  ## Step 2. Calculate the value distribution.
  dataFrameForAnalysisGrouped <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  resultsDataFrame <- dplyr::summarise(dataFrameForAnalysisGrouped, meanValue = mean(valueColumn, na.rm = TRUE),
                                       sdValue = sd(valueColumn, na.rm = TRUE),
                                       sampleNumber = sum(!is.na(valueColumn)))
  ## Step 3. Result list.
  scriptResultsList <- vector("list", 1) 
  scriptResultsList[[1]] <- resultsDataFrame
  scriptResultsList
}