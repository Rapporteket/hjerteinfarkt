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

makeQI <- function(df, var, RHF,erMann,start_dato,slutt_dato)
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

  #####################################################################################################3
  # KiC: Reperfusjonsbehandling innen anbefalt tid ved STEMI
  if(var=="KiC")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator C.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(70, 0, 70, y1 = temp+0.7,lwd=2,col="yellow")
    segments(85, 0, 85, y1 = temp+0.7,lwd=2,col="#33CC00")
  }



    #####################################################################################################3
  # KiD: Koronar angiografi ved NSTEMI
  if(var=="KiD")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator D.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(70, 0, 70, y1 = temp+0.7,lwd=2,col="yellow")
    segments(85, 0, 85, y1 = temp+0.7,lwd=2,col="#33CC00")
  }


    ######################################################################################################
  # KiF: Utskrevet med antitrombotisk behandling
  if(var=="KiF")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator F.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(90, 0, 90, y1 = temp+0.7,lwd=2,col="yellow")
    segments(95, 0, 95, y1 = temp+0.7,lwd=2,col="#33CC00")
  }

  ########################################################################################################3
  # KiG: Utskrevet med lipidsenkende medikament
  if(var=="KiG")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator G.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(90, 0, 90, y1 = temp+0.7,lwd=2,col="yellow")
    segments(95, 0, 95, y1 = temp+0.7,lwd=2,col="#33CC00")
  }

  ########################################################################################################3
  # KiH: Utskrevet med betablokker hvis indikasjon
  if(var=="KiH")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator H.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(75, 0, 75, y1 = temp+0.7,lwd=2,col="yellow")
    segments(85, 0, 85, y1 = temp+0.7,lwd=2,col="#33CC00")
  }

  ########################################################################################################3
  # KiI: Utskrevet med ACE-hemmer hvis indikasjon
  if(var=="KiI")
  {
    source("R/Kvalitetsindikatorer/Kvalitetsindikator I.R",local=TRUE)

    source("R/Kvalitetsindikatorer/Figur_QI.R",local=TRUE)

    segments(75, 0, 75, y1 = temp+0.7,lwd=2,col="yellow")
    segments(85, 0, 85, y1 = temp+0.7,lwd=2,col="#33CC00")
  }

  invisible()
}


