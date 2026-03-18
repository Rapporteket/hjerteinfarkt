#' Make a histogram, either plot or its data
#'
#' Short demo on how to produce dynamic content in a shiny app at Rapporteket
#'
#' @param df dataframe from which output is to be made
#' @param var string defining which varable in the data frame to use
#' @param bins numeric vector defining the number of equally large groups
#' @param makeTable Logical that if TRUE function will return a data frame
#' containin the bin borders and count within each bin
#'
#' @return a graphical object or data frame
#' @export
#'

makeQI_trend <- function(df, var, RHF,erMann,start_dato,slutt_dato)
{


    # Filtrer på valgt RHF hvis ikke "Alle"
    if (!is.null(RHF) && RHF != "Alle") {
      df <- df[df$RHF == RHF, ]
    }

  # Filtrer på Kjønn hvis ikke "Alle"
  if (!is.null(erMann) && erMann != 0) {
    df <- df[df$PatientGender == erMann, ]
  }


  # --- Parametrisering og konverteringer ---
  tz_oslo <- "Europe/Oslo"

  # Sikre at dato-argumenter er POSIXct
  if (!inherits(start_dato, "POSIXct")) {
    start_dato <- as.POSIXct(start_dato, tz = tz_oslo)
  }
  if (!inherits(slutt_dato, "POSIXct")) {
    slutt_dato <- as.POSIXct(slutt_dato, tz = tz_oslo)
  }

  #Fitrer på dato
  Innleggelsestidspunkt  <- as.POSIXct(df$Innleggelsestidspunkt ,format="%Y-%m-%d")
  df <- df %>%  dplyr::filter(Innleggelsestidspunkt >= start_dato & Innleggelsestidspunkt <= slutt_dato)

  ######################################################################################################
  # KiC: Reperfusjonsbehandling innen anbefalt tid ved STEMI
  if(var=="KiC")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator C.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=70,col="yellow")
    abline(h=85,col="green")
  }


  ######################################################################################################
  # KiD: Koronar utredet ved NSTEMI
  if(var=="KiD")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator D.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=70,col="yellow")
    abline(h=85,col="green")
  }


  ######################################################################################################
  # KiF: Utskrevet med antitrombotisk behandling
  if(var=="KiF")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator F.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=90,col="yellow")
    abline(h=95,col="green")
  }

  ########################################################################################################3
  # KiG: Utskrevet med lipidsenkende medikament
  if(var=="KiG")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator G.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=90,col="yellow")
    abline(h=95,col="green")
  }

  ########################################################################################################3
  # KiH: Utskrevet med betablokker hvis indikasjon
  if(var=="KiH")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator H.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=75,col="yellow")
    abline(h=85,col="green")
  }
  ########################################################################################################3
  # KiI: Utskrevet med ACE-hemmer/All-antagonist hvis indikasjon
  if(var=="KiI")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator I.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Trend_QI.R",local=TRUE)

    abline(h=75,col="yellow")
    abline(h=85,col="green")
  }


  invisible()
}

