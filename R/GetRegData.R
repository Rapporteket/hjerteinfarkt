#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getRegData <- function(reshID) {

  # nocov start
  query <- paste("SELECT * FROM hovedskjema_1 WHERE UnitId=", reshID, ";")


  rapbase::loadRegData("NorskHjerteinfarktregister", query)
  # nocov end

}
