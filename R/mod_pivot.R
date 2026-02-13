#' UI for the pivot module
#'
#' @param id Character string module namespace
#'
#' @return An shiny app ui object
#' @export
pivot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        rpivotTable::rpivotTableOutput(ns("pivotSurvey"))
      )
    )
  )
}


#' Server logic for the pivot module
#'
#' @param id Character string module namespace
#' @param user reactiveValues object containing user information
#'
#' @return Server logic for the pivot module
#' @export
pivot_server <- function(id, user) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      regData <- shiny::reactiveVal(data.frame())

      shiny::observeEvent(c(user$role(),user$org()), {
        message("user role changed. It's now: ", user$role())
        if ((user$role() %in% c("LC", "SC"))) {
          regData(getRegData(user$org()))
        } else {
          regData(data.frame())
        }
      })

      output$pivotSurvey <- rpivotTable::renderRpivotTable({
        shiny::req(c(user$role(),user$org()))
        rpivotTable::rpivotTable(regData())
      })
    }
  )
}
