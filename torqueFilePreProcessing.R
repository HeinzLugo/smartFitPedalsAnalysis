#' @description Formats the raw tab separated data file for post processing. Outliers are corrected using standard deviation criteria.
#' @import dplyr
#' @import filterByStandardDeviationCriteria.R
#' @import cadenceFormatCorrection.R
#' @import positionValueCorrection.R
#' @param filePath: Path to the file to preprocess.
#' @param columNames: Column names for the dataframe. If no column names are provided the header names will be used.
#' @param samplingFrequency: Data sampling frequency, by default the sampling frequency is set to 500 Hz.
#' @param numRowsExcluded: Number of rows to exclude from the raw file. The rows are excluded from the top down. By
#' default the number of rows excluded is set to 8.
#' @return processedDataFrame: Formatted table data frame.
#' @warning This function is specific to the raw file provided by the Smartfit system.
#' @author Heinz Lugo.
library(dplyr)
torqueFilePreProcessing <- function(filePath, columnNames = "", samplingFrequency = 500, numRowsExcluded = 8)
{
  ## Step 1. Reading and formatting of the file.
  rawTable <- dplyr::tbl_df(read.table(file = filePath, sep = '\t', skip = numRowsExcluded, header = TRUE, stringsAsFactors = FALSE))
  rawTable <- dplyr::select(rawTable, -Channel.name.)
  ## Step 2. Rename the columns if required.
  if ((columnNames != "") && (length(columnNames) == ncol(rawTable)))
  {
    names(rawTable) <- columnNames
  }else
  {
    stop("The length of the columns in the dataframe and the column names are different.")
  }
  ## Step 3. Create the time data.
  deltaTime <- 1 / samplingFrequency
  timeStamp <- seq(from = deltaTime, to = deltaTime * nrow(rawTable), by = deltaTime)
  rawTable <- cbind.data.frame(timeStamp, rawTable)
  rawTable
}