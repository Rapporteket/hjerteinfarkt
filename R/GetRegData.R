#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getRegData <- function(reshID) {

  # nocov start
  query <- paste("SELECT * FROM data WHERE PrimaerSykehus=",reshID,";")


  rapbase::loadRegData("data", query)
  # nocov end

}
