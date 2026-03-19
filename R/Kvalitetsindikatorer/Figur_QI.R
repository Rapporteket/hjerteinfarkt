#Søylediagram for kvalitetsindikatorer

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

resultat <- resultat[-which(as.numeric(resultat[,3])<10),]


if (nrow(resultat) == 0) {
  plot.new()
  text(0.5, 0.5, "Ingen data tilgjengelig for valgt filter", cex = 1.2)
  return(invisible())
}


andel_tekst <- paste(as.numeric(resultat[,4])*100," %",sep="")
sykehus_N <- paste(resultat[,1]," N=(",resultat[,3],")",sep="")

par(mar=c(3,11,2,2))
temp <- barplot(as.numeric(resultat[,4])*100, yaxt="n",horiz=TRUE,las=1,xlim=c(0,100),xlab="Andel (%)",col="#4292c6",cex.lab=1,cex.axis=1,cex.names=1,border=NA,xaxt="n")
mtext(text = sykehus_N, side = 2, at = temp, line = 0.4,las=1,cex=1)
axis(1, at=seq(0,100,20),labels=c("0 %","20 %","40 %","60 %","80 %","100 %"),tick=TRUE,las=1,cex.axis=1)
mtext("Første sykehus", side=2, line=10, adj=0.5, cex=1, col="black", outer=FALSE)
text( x=0, y=temp-0.07, labels=andel_tekst, col='white', cex=1, pos=4, font=1)
