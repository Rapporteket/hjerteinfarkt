#Kvalitetsindikator G : Utskrevet med lipidsenkende medikament

N_totalt <- dim(df)[1]

PatientAge <- df$PatientAge
ValidererUtreiseSkjema <- df$ValidererUtreiseSkjema
DoedUnderOpphold <- df$DoedUnderOpphold

HarUtreiseSkjema <- rep(0,length(ValidererUtreiseSkjema))
HarUtreiseSkjema[which(ValidererUtreiseSkjema=="1")] <- 1

Under85 <- rep(0,length(PatientAge))
Under85[which(PatientAge<85)] <- 1

#Nevner
Nevner <- rep(0,N_totalt)
Nevner[which(DoedUnderOpphold==0 & HarUtreiseSkjema==1 & Under85==1 & df$TypeInfarkt==1 )] <- 1


#Teller
MedUtStatinAndreLipidsenkere <- df$MedUtStatinAndreLipidsenkere

Teller <- rep(0,length(Nevner))
Teller[which(Nevner==1 & df$MedUtStatinAndreLipidsenkere==1)] <- 1
