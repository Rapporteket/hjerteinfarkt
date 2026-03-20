#' Client (ui) for the hjerteinfarkt app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  regTitle <- "Norsk hjerteinfarktregister"

  shiny::tagList(
    shiny::navbarPage(
      title = rapbase::title(regTitle),
      windowTitle = regTitle,
      theme = rapbase::theme(),
      id = "tabs",
      shiny::tabPanel(
        "Informasjon",
        info_ui("info"),
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
      ),
       shiny::tabPanel(
         "Kvalitetsindikatorer",
         plots_ui("plots")
       ),
      # shiny::tabPanel(
      #   "Over tid",
      #   mod_over_tid_ui("over_tid")
      # ),
      shiny::tabPanel(
        "Samlerapport",
        samlerapport_ui("samlerapport")
      ),
      shiny::tabPanel(
        "Tertialrapport",
        tertialrapport_ui("tertialrapport")
      ),
      shiny::tabPanel(
        "Pivot-tabell",
        pivot_ui("pivot")
      ),
      shiny::tabPanel(
        shiny::span(
          "Abonnement",
          title = "Bestill tilsending av rapporter p\u00e5 e-post"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("subscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("subscription")
          )
        )
      )
    ) # navbarPage
  ) # tagList
}
