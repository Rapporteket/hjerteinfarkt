#Kvalitetsindikator H: Utskrevet med betablokker hvis indikasjon

N_totalt <- dim(df)[1]

PatientAge <- df$PatientAge
DoedUnderOpphold <- df$DoedUnderOpphold
ValidererUtreiseSkjema <- df$ValidererUtreiseSkjema
EkkoEF <- df$EkkoEF
TidlKroniskHjertesvikt <- df$TidlKroniskHjertesvikt
KomplHjertesvikt <- df$KomplHjertesvikt
MedUtBetablokker <- df$MedUtBetablokker
Innleggelsesaar <- df$Innleggelsesaar

EF3eller4 <- rep(0,length(EkkoEF))
EF3eller4[which(EkkoEF==3 | EkkoEF==4)] <- 1
#ok

Utvalg <- rep(0,N_totalt)
Utvalg[which(EF3eller4==1 | TidlKroniskHjertesvikt==1 | KomplHjertesvikt==1)] <- 1

#Nevner
Nevner <- rep(0,N_totalt)
Nevner[which(Innleggelsesaar>=2017 & PatientAge<85 & DoedUnderOpphold==0 & ValidererUtreiseSkjema=="1" & Utvalg==1)] <- 1

table(Nevner,Innleggelsesaar)

#Teller
Teller <- rep(0,length(Nevner))
Teller[which(Nevner==1 & MedUtBetablokker==1)] <- 1

Nevner_IndH <- Nevner
Teller_IndH <- Teller
