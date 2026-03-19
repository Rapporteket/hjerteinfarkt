#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace

plots_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::selectInput(
            inputId = ns("var"),
            label = "Kvalitetsindikator:",
            choices = c("KiC","KiD","KiF","KiG","KiH","KiI")
          ),

          shiny::selectInput(
            inputId = ns("RHF"),
            label = "Regionalt helseforetak:",
            choices = c("Alle", "Helse Midt-Norge", "Helse Vest", "Helse Sør-Øst", "Helse Nord")
          ),
          # shiny::selectInput(
          #   inputId = ns("HF"),
          #   label = "Helseforetak:",
          #   choices = c("Alle", "Akershus universitetssykehus HF","St. Olavs hospital HF")
          # ),
          shiny::selectInput(
            inputId = ns("erMann"),
            label = "Kjønn",
            choices = c("Begge" = 0, "Menn" = 1, "Kvinner" = 2)
          ),
          # shiny::sliderInput(
          #   inputId = "alder",
          #   label = "Alder",
          #   min = 0,
          #   max = 110,
          #   value = c(0, 110)
          # ),
          shiny::dateRangeInput(
            inputId = ns("datovalgGjsn"),
            start = as.Date(
              paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01')
            ),
            end = Sys.Date(),
            label = "Tidsperiode", separator="t.o.m.", language="nb"
          )


        )




      ),
      shiny::column(
        width = 9,
        height=11,
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Figur",

            shiny::uiOutput(ns("tekst_over")),

            shiny::plotOutput(ns("distPlot"), height = "1400px", width = "100%"),

          ),
          shiny::tabPanel(
            "Tabell",
            shiny::tableOutput(ns("distTable"))
          ),
          shiny::tabPanel(
            "Trend",
            shiny::plotOutput(ns("TrendPlot"))
          ),
          shiny::tabPanel(
            "Om kvalitetsindikatoren",
            shiny::textOutput(ns("tekst_kvalitetsindikator"))
          ),
          shiny::tabPanel(
            "Samleindikatorfigur",
            shiny::plotOutput(ns("SamlePlot"))
          )
        )
      )
    )
  )
}



plots_ui2 <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::selectInput(
            inputId = ns("var"),
            label = "Variabel:",
            choices = c("PatientGender","SamletStType","HealthUnitShortName","InvPCI","Trombolysebeh")
          ),
        )
      ),
      shiny::column(
        width = 9,
        height=11,
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Figur",
            shiny::plotOutput(ns("distPlot2"), height = "500px", width = "100%")
          ),
          shiny::tabPanel(
            "Tabell",
            shiny::tableOutput(ns("distTable2"))
          )
        )
      )
    )
  )
}








plots_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      regData <- rapbase::loadRegData(query="SELECT * FROM pasientforlop_3;")
      data <- regData

      data$RHF[which(regData$RHF=="Helse SÃ¸r-Ã\u0098st")] <- "Helse Sør-Øst"

      data$HealthUnitShortName[which(data$HealthUnitShortName=="Ã\u0085lesund")] <- "Ålesund"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="BÃ¦rum")] <- "Bærum"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="BodÃ¸")] <- "Bodø"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="FÃ¸rde")] <- "Førde"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="GjÃ¸vik")] <- "Gjøvik"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="LÃ¦rdal")] <- "Lærdal"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="MosjÃ¸en")] <- "Mosjøen"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="SandnessjÃ¸en")] <- "Sandnessjøen"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="TÃ¸nsberg")] <- "Tønsberg"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="TromsÃ¸")] <- "Tromsø"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="UllevÃ¥l")] <- "Ullevål"
      data$HealthUnitShortName[which(data$HealthUnitShortName=="VesterÃ¥len")] <- "Vesterålen"




      output$distPlot <- renderPlot({
        req(data, input$var)
        makeQI(df = data, var = input$var,RHF=input$RHF,erMann=input$erMann,start_dato=input$datovalgGjsn[1],slutt_dato =input$datovalgGjsn[2])
      })

      output$tekst_over <- renderUI({
        tekst <- switch(input$var,
                        "KiC" = "<b>Reperfusjonsbehandling innen anbefalt tid ved STEMI </b>  <br><br>  ",
                        "KiD" = "<b>Koronar utredet ved NSTEMI</b>  <br><br>  ",
                        "KiF" = "<b>Utskrevet med antitrombotisk behandling</b>   <br><br> Lav måloppnåelse: <90 %, moderat måloppnåelse: 90 % - 94 %, høy måloppnåelse: >= 95 %",
                        "KiG" = "<b>Utskrevet med lipidsenkende medikament</b>  <br><br> Lav måloppnåelse: <90 %, moderat måloppnåelse: 90 % - 94 %, høy måloppnåelse: >= 95 %",
                        "KiH" = "<b>Utskrevet med betablokker hvis indikasjon</b>  <br><br> Lav måloppnåelse: <75 %, moderat måloppnåelse: 75 % - 84 %, høy måloppnåelse: >= 85 %",
                        "KiI" = "<b>Utskrevet med ACE-hemmer/All-antagonist hvis indikasjon</b>  <br><br> Lav måloppnåelse: <75 %, moderat måloppnåelse: 75 % - 84 %, høy måloppnåelse: >= 85 %",
                        "Ingen kvalitetsindikator valgt"

        )
        HTML(tekst)
      })

      output$tekst_kvalitetsindikator <- renderText({
        req(input$var)
        if (input$var == "KiC") {
          "Reperfusjonsbehandling innen anbefalt tid ved STEMI: Pasienter under 85 år som innen anbefalt tid ble behandlet med blodproppløsende medikament eller utblokking ved mistanke om tett hjerteåre og alvorlig hjerteinfarkt (STEMI). Lav måloppnåelse: <70 %, moderat måloppnåelse: 70 % - 84 %, høy måloppnåelse: >= 85 %."
        }
        else if (input$var == "KiF") {
          "Utskrevet med antitrombotisk behandling: Pasienter under 85 år med type 1 hjerteinfarkt som behandles med to mediament for å forebygge ny blodpropp etter hjerteinfarktet. Lav måloppnåelse: <90 %, moderat måloppnåelse: 90 % - 94 %, høy måloppnåelse: >= 95 %."
        } else if (input$var == "KiG") {
          "Utskrevet med lipidsenkende medikament: Pasienter under 85 år med type 1 hjerteinfarkt som behandles med kolesterolsenkende medisin etter hjerteinfarktet. Lav måloppnåelse: <90 %, moderat måloppnåelse: 90 % - 94 %, høy måloppnåelse: >= 95 %."
        }
        else if (input$var == "KiH") {
          "Utskrevet med betablokker hvis indikasjon. Lav måloppnåelse: <75 %, moderat måloppnåelse: 75 % - 84 %, høy måloppnåelse: >= 85 %."
        }
        else if (input$var == "KiI") {
          "Utskrevet med ACE-hemmer/All-antagonist hvis indikasjon. Lav måloppnåelse: <75 %, moderat måloppnåelse: 75 % - 84 %, høy måloppnåelse: >= 85 %."
        }else {
          "Velg en indikator for å se beskrivelse."
        }
      })

      output$distTable <- renderTable({
        req(data, input$var)
        makeQI_table(df = data, var = input$var,RHF=input$RHF,erMann=input$erMann,start_dato=input$datovalgGjsn[1],slutt_dato =input$datovalgGjsn[2])
      })

      output$TrendPlot <- renderPlot({
        req(data, input$var)
        makeQI_trend(df = data, var = input$var,RHF=input$RHF,erMann=input$erMann,start_dato=input$datovalgGjsn[1],slutt_dato =input$datovalgGjsn[2])
      })
      output$SamlePlot <- renderPlot({
        req(data, input$var)
        makeQI_all(df = data, var = input$var,RHF=input$RHF,erMann=input$erMann,start_dato=input$datovalgGjsn[1],slutt_dato =input$datovalgGjsn[2])
      })
    }
  )
}




plots_server2 <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Last inn data
      regData <- rapbase::loadRegData(query="SELECT * FROM pasientforlop_3;")
      regData$RHF[which(regData$RHF=="Helse SÃ¸r-Ã\u0098st")] <- "Helse Sør-Øst"

      # Figur og tabell


      #Histogram for kategoriske variabler
      output$distPlot2 <- shiny::renderPlot({
        req(regData)
        makeHist_kat(df = regData, var = input$var)
      })




    }
  )
}

