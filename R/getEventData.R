#' Get Event Data Table
#'
#' @param data Data frame object of the following columns: Data Table Name, Data File Path, Event Column, Expanded View Flag, Event Name, Event Start Column, Event Stop Column, Treatment Flag, Response Flag. More information in details.
#' @param list A list object of data frames annotated in the data object. The names of each data frame in the list should match the first column of the data object.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#'
getEventData <- function(data = NULL,list = NULL) {
  if (is.null(data)) stop("Must provide parameter file data.")
  if (is.null(list)) stop("Must provide list of data frames.")
  if (ncol(data) != 9) stop("Please check data format has required columns.")

  data[,7] <- ifelse(is.na(data[,7]),data[,6],data[,7])
  eventData <- data[which(!is.na(data[,3])),]
  eventDataColumns <- c("SampleName","Event","EventType","EventTab","AgeAtEventStart","AgeAtEventStop")
  Patient_Event_Data <- data.frame(matrix(nrow = 0, ncol = length(eventDataColumns)))
  colnames(Patient_Event_Data) <- eventDataColumns
  for (row in seq(nrow(eventData))) {
    tabName <- eventData[row,1]
    df <- list[[tabName]]
    SampleName <- df[,1]
    EventType <- eventData[row,5]
    EventTab <- tabName
    if (eventData[row,4]) {
      Event <- paste0(EventType,": ",df[,eventData[row,3]])
    } else {
      Event <- eventData[row,3]
    }
    AgeAtEventStart <- suppressWarnings(as.numeric(df[,eventData[row,6]]))
    AgeAtEventStop <- suppressWarnings(as.numeric(df[,eventData[row,7]]))
    Patient_Event_Data <- rbind(Patient_Event_Data,
                                data.frame(SampleName,
                                           Event,
                                           EventType,
                                           EventTab,
                                           AgeAtEventStart,
                                           AgeAtEventStop))
    Patient_Event_Data <- unique(Patient_Event_Data)
  }
  Patient_Event_Data <- Patient_Event_Data[order(Patient_Event_Data[,1]),]
  return(Patient_Event_Data)
}

