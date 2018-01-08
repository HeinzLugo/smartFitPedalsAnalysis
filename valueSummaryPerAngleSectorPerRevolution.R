#' @description Returns the distribution for the magnitude of the values provided
#' organised per angle division within each revolution.
#' @import dplyr
#' @import valueSummaryPerRevolution.R
#' @param valueColumn: Vector containing the value which distribution will be calculated.
#' @param angleValues: Vector with angle values.
#' @param angularDivisionValue: Angle sector in which the column should be split.
#' @param angleType: Dimensional unit for the angle value.
#' If type analysis is set to time then the structure of results returned changes however the grouping by time
#' has to be done before calling the function.
#' @return scriptResultsList: List with resulting dataframes.
#' @reminder The scripts return mean values, standard deviation and the number of samples. Upper and lower limits can be found
#' using these values.
#' @reminder The largest angular sector possible is 360º.
#' @author Heinz Lugo.
valueSummaryPerAngleSectorPerRevolution <- function(valueColumn, angleValues, angularDivisionValue, angleType, analysisType)
{
  if((angleType != "degrees") & (angleType != "radians"))
  {
    stop("Angle type should be degrees or radians.")
  }
  if((analysisType != "angle") && (analysisType != "time"))
  {
    stop("Analysis type should be angle or time.")
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
  ## Step 2. Split by revolution.
  dataFrameForAnalysis <- splitByRevolution(angleValues = angleValues, angleType = angleType)
  dataFrameForAnalysis <- dplyr::tbl_df(cbind.data.frame(valueColumn, dataFrameForAnalysis))
  dataFrameForAnalysis <- dplyr::group_by(dataFrameForAnalysis, revolutionFactorColumn)
  valueSummaryPerAngleSectorPerRevolutionResults <- dplyr::summarise(dataFrameForAnalysis,valueSummaryPerAngleSectorPerRevolutionResults = 
                                                                       valueSummaryPerAngleSector(valueColumn = valueColumn, angleValues = angleValues,
                                                                                                  angularDivisionValue = angularDivisionValue,
                                                                                                  angleType = angleType))
  ## Step 5. Result list.
  if(analysisType == "angle")
  {
    scriptResultsList <- vector("list", 1) 
    scriptResultsList[[1]] <- valueSummaryPerAngleSectorPerRevolutionResults
  }
  else
  {
    tempResultsList <- vector(mode = "list", 1)
    tempResultsList[[1]] <- valueSummaryPerAngleSectorPerRevolutionResults
    scriptResultsList <- vector("list", 1)
    scriptResultsList[[1]] <- tempResultsList
  }
  scriptResultsList
}