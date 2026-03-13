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
#' @examples
#' makeHist(df = mtcars, var = "mpg", bins = 5, makeTable = FALSE)
#'
#'



makeQI_all <- function(df, var, RHF,erMann,start_dato,slutt_dato)
{
    # Filtrer på valgt RHF hvis ikke "Alle"
    if (!is.null(RHF) && RHF != "Alle") {
      df <- df[df$RHF == RHF, ]
    }

    # Filtrer på Kjønn hvis ikke "Alle"
    if (!is.null(erMann) && erMann != 0) {
    df <- df[df$PatientGender == erMann, ]
    }

  #Fitrer på dato
  #df %>%  dplyr::filter(df$Innleggelsestidspunkt >= start_dato & df$Innleggelsestidspunkt <= slutt_dato)

  source("R/Kvalitetsindikatorer/Kvalitetsindikator C.R",local=TRUE)

  Teller_C <- sum(Teller)
  Nevner_C <- sum(Nevner)
  Andel_C <- Round(Teller_C/Nevner_C,3)*100

  source("R/Kvalitetsindikatorer/Kvalitetsindikator D.R",local=TRUE)

  Teller_D <- sum(Teller)
  Nevner_D <- sum(Nevner)
  Andel_D <- Round(Teller_D/Nevner_D,3)*100




    source("R/Kvalitetsindikatorer/Kvalitetsindikator F.R",local=TRUE)

    Teller_F <- sum(Teller)
    Nevner_F <- sum(Nevner)
    Andel_F <- Round(Teller_F/Nevner_F,3)*100

    source("R/Kvalitetsindikatorer/Kvalitetsindikator G.R",local=TRUE)

    Teller_G <- sum(Teller)
    Nevner_G <- sum(Nevner)
    Andel_G <- Round(Teller_G/Nevner_G,3)*100

    source("R/Kvalitetsindikatorer/Kvalitetsindikator H.R",local=TRUE)

    Teller_H <- sum(Teller)
    Nevner_H <- sum(Nevner)
    Andel_H <- Round(Teller_H/Nevner_H,3)*100

    source("R/Kvalitetsindikatorer/Kvalitetsindikator I.R",local=TRUE)

    Teller_I <- sum(Teller)
    Nevner_I <- sum(Nevner)
    Andel_I <- Round(Teller_I/Nevner_I,3)*100

    Andel_indikatorer <- c(Andel_I,Andel_H,Andel_G,Andel_F,Andel_D,Andel_C)

    par(mfrow=c(1,1),oma=c(2,20,2,2))
    temp <- barplot(Andel_indikatorer,horiz=TRUE,col="#4292c6",xlim=c(0,100),xaxt="n",border=NA,main="Kvalitetsindikatorer, periode: dato1 - dato2")
    axis(1,at=seq(0,100,20),labels=c("0 %","20 %","40 %","60 %","80 %","100 %"))

    Indikatorer <- c("I: ACE-hemmer/AII-antagonist","H: Betablokker","G: Lipidsenker","F: Antitrombotisk behandling","D: Koronar utredet ved NSTEMI","C: Reperfusjonsbehandling innen anbefalt tid ved STEMI")

    axis(2,at=temp,labels=Indikatorer,las=2)

    Andel_tekst <- paste(Andel_indikatorer, " %",sep="")

    text( x=0, y=temp-0.07, labels=Andel_tekst, col='white', cex=1, pos=4, font=1)

    par(mfrow=c(1,1))

  invisible()
}


