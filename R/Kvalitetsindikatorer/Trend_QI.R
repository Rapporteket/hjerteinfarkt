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



if (nrow(resultat) == 0) {
  plot.new()
  text(0.5, 0.5, "Ingen data tilgjengelig for valgt filter", cex = 1.2)
  return(invisible())
}

andel_tekst <- paste(as.numeric(resultat[,4])*100," %",sep="")
sykehus_N <- paste(resultat[,1]," N=(",resultat[,3],")",sep="")


#Plotte tidstrend
library(lubridate)
Mnd <- month(as.POSIXct(df$Innleggelsestidspunkt,format="%d.%m.%Y%H:%M"))
andel_mnd <- rep(NA,length(unique(Mnd)))

#Andel per måned nasjonalt
for(i in 1:length(unique(Mnd)))
{
  Mnd_i <- Mnd[i]
  andel_mnd[i] <- round(sum(Teller[which(Mnd==Mnd_i)])/sum(Nevner[which(Mnd==Mnd_i)]),2)*100
}

andel_mnd_enhet <- rep(NA,length(unique(Mnd)))
#Andel per måned nasjonalt
for(i in 1:length(unique(Mnd)))
{
  Mnd_i <- Mnd[i]
  andel_mnd_enhet[i] <- round(sum(Teller[which(Mnd==Mnd_i)])/sum(Nevner[which(Mnd==Mnd_i)]),2)*100
}


plot(andel_mnd_enhet,ylim=c(0,100),xlab="Måned",ylab="Andel",xaxt="n",yaxt="n",bty="n")
lines(andel_mnd_enhet)
axis(1,at=seq(1,length(andel_mnd)),labels=c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Des"),las=2)
axis(2,at=seq(0,100,20),labels=c("0 %","20 %","40 %","60 %","80 %","100 %"),las=2)
text(seq(1,length(andel_mnd),1),andel_mnd,labels=paste(andel_mnd," %",sep=""),pos=3,cex=1.2)
lines(c(90,87,92,95,93,95,92,95,92,96,94,91),col="red")



