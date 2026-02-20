#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getRegData <- function(reshID) {

  # nocov start
<<<<<<< HEAD
  query <- paste("SELECT * FROM data WHERE PrimaerSykehus=",reshID,";")
=======
  query <- paste("SELECT * FROM data WHERE PrimaerSykehus=", reshID, ";")
>>>>>>> e4d2a8169f915a7a5beb30f32548d70e0b744ac5


  rapbase::loadRegData("data", query)
  # nocov end

}
