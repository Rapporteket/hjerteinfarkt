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


makeHist <- function(df, var, makeTable = FALSE)
{
   ######################################################################################################
   # KiF: Utskrevet med antitrombotisk behandling

   N_totalt <- dim(df)[1]

   ProsedyreEtPlan <- df$ProsedyreEtPlan #2 og 6 skal ekskluderes
   TilKirurgi <- rep(0,length(ProsedyreEtPlan))
   TilKirurgi[which(ProsedyreEtPlan==2 | ProsedyreEtPlan==6)] <- 1

   #################################################################################
   #Teller
   MedUtASA <- df$MedUtASA
   ASA_utreise <- rep(0,length(MedUtASA))
   ASA_utreise[which(MedUtASA==1)] <- 1

   MedUtADP <- df$MedUtADP
   ADP_utreise <- rep(0,length(MedUtADP))
   ADP_utreise[which(MedUtADP==1)] <- 1

   MedUtAnnenPlatehemming <- df$MedUtAnnenPlatehemming
   AnnenPlatehemming_utreise <- rep(0,length(MedUtAnnenPlatehemming))
   AnnenPlatehemming_utreise[which(MedUtAnnenPlatehemming==1 | MedUtAnnenPlatehemming==2 | MedUtAnnenPlatehemming==3 | MedUtAnnenPlatehemming==4)] <- 1

   MedUtAntikoagulasjon <- df$MedUtAntikoagulasjon
   Antikoagulasjon_utreise <- rep(0,length(MedUtAntikoagulasjon))
   Antikoagulasjon_utreise[which(MedUtAntikoagulasjon>=1 & MedUtAntikoagulasjon<=6)] <- 1

   Teller_SUM <- ASA_utreise+ADP_utreise+AnnenPlatehemming_utreise+Antikoagulasjon_utreise

   ################################################################################
   PatientAge <- df$PatientAge
   ValidererUtreiseSkjema <- df$ValidererUtreiseSkjema

   DoedUnderOpphold <- df$DoedUnderOpphold

   HarUtreiseSkjema <- rep(0,length(ValidererUtreiseSkjema))
   HarUtreiseSkjema[which(ValidererUtreiseSkjema=="1")] <- 1

   Under85 <- rep(0,length(PatientAge))
   Under85[which(PatientAge<85)] <- 1

   ################################################################################
   #Nevner
   Nevner <- rep(0,N_totalt)
   Nevner[which(DoedUnderOpphold==0 & HarUtreiseSkjema==1 & Under85==1 & TilKirurgi==0 & df$TypeInfarkt==1)] <- 1

   #Teller
   Teller <- rep(0,length(Nevner))
   Teller[which(Nevner==1 & Teller_SUM>1)] <- 1


   #######################################################################################################3
   #Finner andel per sykehus
   sykehus <- unique(df$HealthUnitShortName)
   n_sykehus <- length(sykehus)

   #x <- df[[var]]
   teller_sykehus <- rep(NA,n_sykehus)
   nevner_sykehus <- rep(NA,n_sykehus)


   for(i in 1:n_sykehus)
   {
    teller_sykehus[i] <- length(which(Teller==1 & df$HealthUnitShortName==sykehus[i]))
    nevner_sykehus[i] <- length(which(Nevner==1 & df$HealthUnitShortName==sykehus[i]))
   }

   andel_sykehus <- round(teller_sykehus/nevner_sykehus,2)
   resultat <- cbind(sykehus,teller_sykehus,nevner_sykehus,andel_sykehus)
   resultat <- resultat[order(resultat[,4]),]

   #temp <- barplot(as.numeric(resultat[,4]),horiz=TRUE,col="#4292c6",xlab="Median")
   #mtext(text=resultat[,1],side=2,at=temp,las=1,cex=0.8)

   andel_tekst <- paste(as.numeric(resultat[,4])*100," %",sep="")
   sykehus_N <- paste(resultat[,1]," N=(",resultat[,3],")",sep="")

   par(mar=c(3,11,2,2))
   temp <- barplot(as.numeric(resultat[,4])*100, yaxt="n",horiz=TRUE,las=1,xlim=c(0,100),xlab="Andel (%)",col="#4292c6",cex.lab=0.8,cex.axis=0.8,cex.names=0.8,border=NA,xaxt="n")
   mtext(text = sykehus_N, side = 2, at = temp, line = 0.4,las=1,cex=0.8)
   axis(1, at=seq(0,100,20),labels=c("0 %","20 %","40 %","60 %","80 %","100 %"),tick=TRUE,las=1,cex.axis=0.8)
   mtext("Første sykehus", side=2, line=10, adj=0.5, cex=0.8, col="black", outer=FALSE)
   text( x=0, y=temp-0.07, labels=andel_tekst, col='white', cex=0.75, pos=4, font=1)

   segments(90, 0, 90, y1 = temp+0.7,lwd=2,col="yellow")
   segments(95, 0, 95, y1 = temp+0.7,lwd=2,col="#33CC00")

}


makeHist_kat <- function(df, var, makeTable = FALSE)
{

   x <- df[[var]]

   hist(x)
   #hist(runif(1000,0,1))
}


makeHist_pol <- function(df, var, makeTable = FALSE)
{

   x <- df[[var]]

   hist(x)
   #hist(runif(1000,0,1))
}

