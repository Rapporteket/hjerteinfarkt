#Kvalitetsindikator F: Utskrevet med antitrombotisk behandling

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
