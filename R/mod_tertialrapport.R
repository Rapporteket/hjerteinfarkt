#' Shiny module providing GUI and server logic for the report tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

tertialrapport_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Fordeling av mpg",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          inputId = ns("varS"),
          label = "Variabel:",
          c("mpg", "disp", "hp", "drat", "wt", "qsec")
        ),
        uiOutput(ns("enhetSelect")),
        shiny::sliderInput(
          inputId = ns("binsS"),
          label = "Antall grupper:",
          min = 1,
          max = 10,
          value = 5
        ),
        shiny::selectInput(
          inputId = ns("formatS"),
          label = "Velg format for nedlasting:",
          choices = list(PDF = "pdf", HTML = "html")
        ),
        shiny::downloadButton(
          outputId = ns("downloadSamlerapport"),
          label = "Last ned!"
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("tertialrapport"))
      )
    )
  )
}

#' Server logic for samlerapport
#' @return A Shiny app server object
#' @export


tertialrapport_server <- function(id, enhetsvalg = c("Ahus","Arendal","Drammen","Elverum","Haukeland","Rikshospitalet","St. Olav")) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # --- Enhet-velger (kan gjøres dynamisk) ---
      output$enhetSelect <- shiny::renderUI({
        shiny::selectizeInput(
          inputId = ns("enhet"),
          label  = "Velg enhet:",
          choices = enhetsvalg,
          selected = if (length(enhetsvalg)) enhetsvalg[[1]] else NULL,
          options  = list(placeholder = "Velg enhet...")
        )
      })

      # # --- Hovedvisning: render Rmd fragment reaktivt på input ---
             output$tertialrapport <- shiny::renderUI({
               shiny::req(input$enhet, input$varS, input$binsS)
               rapbase::renderRmd(
                 system.file("tertialrapport.Rmd", package = "hjerteinfarkt"),
                 #outputType = "html_fragment",
                 outputType = "pdf",
                 params = list(type = "pdf",
                 #params = list(type = "html",
                               var = input$varS,
                               bins = input$binsS,enhet=input$enhet)
               )
             })

      # --- Nedlasting: bruker samme parametre ---
      output$downloadSamlerapport <- shiny::downloadHandler(
        filename = function() {
          basename(tempfile(
            pattern = "rapRegTemplateSamlerapport",
            fileext = paste0(".", input$formatS)
          ))
        },
        content = function(file) {
          srcFile <- normalizePath(system.file("tertialrapport.Rmd", package = "hjerteinfarkt"))
          fn <- rapbase::renderRmd(
            file = srcFile,
            outputType = input$formatS,
            params = list(
              type = input$formatS,
              var  = input$varS,
              bins = input$binsS,
              enhet = input$enhet
            )
          )
          file.rename(fn, file)
        }
      )
    }
  )
}
