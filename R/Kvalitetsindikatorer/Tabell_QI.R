sykehus <- unique(df$HealthUnitShortName)
n_sykehus <- length(sykehus)

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

andel_tekst <- paste(as.numeric(resultat[,4])*100," %",sep="")
sykehus_N <- paste(resultat[,1]," N=(",resultat[,3],")",sep="")

#Tabell til output
tabell <- resultat
tabell <- tabell[order(as.numeric(tabell[,4]),decreasing=TRUE),]
tabell[,4] <- paste(as.numeric(tabell[,4])*100," %",sep="")
colnames(tabell) <- c("Enhet","Teller","Nevner","Andel")
