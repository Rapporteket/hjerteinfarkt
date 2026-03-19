#Kvalitetsindikator C: Reperfusjonsbehandling innen anbefalt tid ved STEMI

#
#df <- df[-which(is.na(df$InvPCI)|is.na(df$PrehospEKG)),]

df <- df[which(df$SamletStType==1),]


df <- df[-which(is.na(df$InvPCI)),]


################################################################################
variabelnavn <- colnames(df)

SykehusForlop <- df$SykehusForlop

Innleggelsesaar <- df$Innleggelsesaar

N_totalt <- dim(df)[1]
################################################################################
AntallBasisSkjema <-  df$AntallBasisSkjema

ValidererBasisSkjema <- df$ValidererBasisSkjema
SymptomdebutAntattTid <- df$SymptomdebutAntattTid

HarBasisSkjema <- rep(0,length(AntallBasisSkjema))
HarBasisSkjema[which(AntallBasisSkjema != 0)] <- 1

BasisSkjema <- rep(0,length(ValidererBasisSkjema))
BasisSkjema[which(ValidererBasisSkjema=="1")] <- 1
#########################################################################################################
# Formaterer tidsvariabler ##############################################################################
#########################################################################################################
Symptomdebut <- as.POSIXct(df$Symptomdebut,format="%Y-%m-%d%H:%M")
TidUkjentSymptomdebut <-  df$TidUkjentSymptomdebut

ForsteKontakt <- as.POSIXct(df$ForsteKontakt,format="%Y-%m-%d%H:%M")
TidUkjentForsteKontakt <- df$TidUkjentForsteKontakt

PrehospEKG <- df$PrehospEKG
PrehospEKGLokasjon <- df$PrehospEKGLokasjon
PrehospEkGtid <- as.POSIXct(df$PrehospEkGtid,format="%Y-%m-%d%H:%M")
TidUkjentPrehospitaltEkg <- df$TidUkjentPrehospitaltEkg

#Reperfusjonsbehandling
Trombolyse <- df$Trombolysebeh
Trombolysetid  <- as.POSIXct(df$Trombolysetid ,format="%Y-%m-%d%H:%M")
TidUkjentTrombolysetid <- df$TidUkjentTrombolysetid

InvKunKorAngio <- df$InvKunKorAngio
InvKunKorAngioTid  <- as.POSIXct(df$InvKunKorAngioTid ,format="%Y-%m-%d%H:%M")
TidUkjentInvKunKorAngioTid <- df$TidUkjentInvKunKorAngioTid

InvPCI <- df$InvPCI
InvPCITid  <- as.POSIXct(df$InvPCITid ,format="%Y-%m-%d%H:%M")
TidUkjentInvPCITid <- df$TidUkjentInvPCITid

Innleggelsestidspunkt <- df$Innleggelsestidspunkt
Innleggelsestidspunkt <- as.POSIXct(df$Innleggelsestidspunkt,format="%Y-%m-%d%H:%M")
################################################################################
#Finner første reperfusjonsbehandling
Reperfusjonsbehandling <- rep(NA,length(Innleggelsesaar))
for(i in 1:length(Innleggelsesaar))
{
  if(df$Trombolysebeh[i]==1 | df$Trombolysebeh[i]==2)
  {
    Reperfusjonsbehandling[i] <- 1 #Trombolyse
  }
  else if(df$InvKunKorAngio[i]==1 & df$InvPCI[i]!=1)
  {
    Reperfusjonsbehandling[i] <- 2
  }
  else if(df$InvPCI[i]==1 & df$InvKunKorAngio[i]!=1)
  {
    Reperfusjonsbehandling[i] <- 3
  }
  else if(df$InvKunKorAngio[i]==1 & df$InvPCI[i]==1)
  {
    Reperfusjonsbehandling[i] <- 4
  }
}
################################################################################
#Symptomdebut under 12 timer (<720 min)
Symptomdebut_FMK <- rep(NA,length(df$Symptomdebut))
for(i in 1:length(df$Symptomdebut))
{
  #FMK og sympt kjent
  if(df$TidUkjentForsteKontakt[i]==1 & df$TidUkjentSymptomdebut[i]==1)
  {
    Symptomdebut_FMK[i] <- as.numeric(difftime(ForsteKontakt[i],Symptomdebut[i],units="mins"))
  }
  #FMK kjent og antatt symptomdebut
  else if(df$TidUkjentForsteKontakt[i]==1 & df$SymptomdebutAntattTid[i]==1)
  {
    Symptomdebut_FMK[i] <- 720
  }
  #EKG ny variabel
  #Ukjent FMK , kjent prehosp EKG = EKG-10 min og kjent symptomdebut
  else if(df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1 & df$TidUkjentSymptomdebut[i]==1) #Diagnostisk EKG tatt prehospitalt
  {
    Symptomdebut_FMK[i] <- as.numeric(difftime(PrehospEkGtid[i],Symptomdebut[i],units="mins"))-10
  }
  #Ukjent FMK, kjent prehosp EKG = EKG-10 min og antatt sympotmdebut
  else if(df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1 & df$SymptomdebutAntattTid[i]==1) #Diagnostisk EKG tatt prehospitalt
  {
    Symptomdebut_FMK[i] <- 720
  }
  # #EKG gammel variabel
  # #Ukjent FMK , kjent prehosp EKG = EKG-10 min og kjent symptomdebut
  # else if(df$PrehospEKG[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1 & df$TidUkjentSymptomdebut[i]==1) #Diagnostisk EKG tatt prehospitalt
  # {
  #   Symptomdebut_FMK[i] <- as.numeric(difftime(PrehospEkGtid[i],Symptomdebut[i],units="mins"))-10
  # }
  # #Ukjent FMK, kjent prehosp EKG = EKG-10 min og antatt sympotmdebut
  # else if(df$PrehospEKG[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1 & df$SymptomdebutAntattTid[i]==1) #Diagnostisk EKG tatt prehospitalt
  # {
  #   Symptomdebut_FMK[i] <- 720
  # }
}
#################################################################################
#Tid til første reperfusjon
TidTilReperfusjon <- rep(NA,length(df$Innleggelsesaar))
for(i in 1:length(df$Innleggelsesaar))
{
  #EKG kjent
  if(Reperfusjonsbehandling[i]==1 & df$TidUkjentTrombolysetid[i]==1 & df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(Trombolysetid[i],PrehospEkGtid[i],units="mins"))
  }
  else if(Reperfusjonsbehandling[i]==2 & df$TidUkjentInvKunKorAngioTid[i]==1 & df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(InvKunKorAngioTid[i],PrehospEkGtid[i],units="mins"))
  }
  else if(Reperfusjonsbehandling[i]==3 & df$TidUkjentInvPCITid[i]==1 & df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(InvPCITid[i],PrehospEkGtid[i],units="mins"))
  }
  else if(Reperfusjonsbehandling[i]==4 & df$TidUkjentInvKunKorAngioTid[i]==1 & df$TidUkjentInvPCITid[i]==1 & df$PrehospEKGLokasjon[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  {
    TidTilReperfusjon[i] <-  min(as.numeric(difftime(InvPCITid[i],PrehospEkGtid[i],units="mins")),
                                 as.numeric(difftime(InvKunKorAngioTid[i],PrehospEkGtid[i],units="mins")))
  }
  # #EKG kjent (årene før 2019....)
  # else if(Reperfusjonsbehandling[i]==1 & df$TidUkjentTrombolysetid[i]==1 & df$PrehospEKG[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  # {
  #   TidTilReperfusjon[i] <-  as.numeric(difftime(Trombolysetid[i],PrehospEkGtid[i],units="mins"))
  # }
  # else if(Reperfusjonsbehandling[i]==2 & df$TidUkjentInvKunKorAngioTid[i]==1 & df$PrehospEKG[i]==1 & df$TidUkjentPrehospitaltEkg[i]==1)
  # {
  #   TidTilReperfusjon[i] <-  as.numeric(difftime(InvKunKorAngioTid[i],PrehospEkGtid[i],units="mins"))
  # }
  # else if(Reperfusjonsbehandling[i]==3 & df$TidUkjentInvPCITid[i]==1 & df$PrehospEKG[i]==1 & TidUkjentPrehospitaltEkg[i]==1)
  # {
  #   TidTilReperfusjon[i] <-  as.numeric(difftime(InvPCITid[i],PrehospEkGtid[i],units="mins"))
  # }
  # else if(Reperfusjonsbehandling[i]==4 & TidUkjentInvKunKorAngioTid[i]==1 & TidUkjentInvPCITid[i]==1 & PrehospEKG[i]==1 & TidUkjentPrehospitaltEkg[i]==1)
  # {
  #   TidTilReperfusjon[i] <-  min(as.numeric(difftime(InvPCITid[i],PrehospEkGtid[i],units="mins")),
  #                                as.numeric(difftime(InvKunKorAngioTid[i],PrehospEkGtid[i],units="mins")))
  # }




  #EKG ukjent = FMK-10 min
  else if(Reperfusjonsbehandling[i]==1 & TidUkjentTrombolysetid[i]==1 & TidUkjentForsteKontakt[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(Trombolysetid[i],ForsteKontakt[i],units="mins"))-10
  }
  else if(Reperfusjonsbehandling[i]==2 & TidUkjentInvKunKorAngioTid[i]==1 & TidUkjentForsteKontakt[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(InvKunKorAngioTid[i],ForsteKontakt[i],units="mins"))-10
  }
  else if(Reperfusjonsbehandling[i]==3 & TidUkjentInvPCITid[i]==1 & TidUkjentForsteKontakt[i]==1)
  {
    TidTilReperfusjon[i] <-  as.numeric(difftime(InvPCITid[i],ForsteKontakt[i],units="mins"))-10
  }
  else if(Reperfusjonsbehandling[i]==4 & TidUkjentInvKunKorAngioTid[i]==1 & TidUkjentInvPCITid[i]==1 & TidUkjentForsteKontakt[i]==1)
  {
    TidTilReperfusjon[i] <-  min(as.numeric(difftime(InvPCITid[i],ForsteKontakt[i],units="mins")),
                                 as.numeric(difftime(InvKunKorAngioTid[i],ForsteKontakt[i],units="mins")))-10
  }
}
##################################################################################
#Reperfusjon anbefalt tid
ReperfusjonAnbefaltTid <- rep(0,length(Innleggelsesaar))
ReperfusjonAnbefaltTid[which(Reperfusjonsbehandling==1 & TidTilReperfusjon<=20)] <- 1
ReperfusjonAnbefaltTid[which(Reperfusjonsbehandling==2 & TidTilReperfusjon<=90)] <- 1
ReperfusjonAnbefaltTid[which(Reperfusjonsbehandling==3 & TidTilReperfusjon<=90)] <- 1
ReperfusjonAnbefaltTid[which(Reperfusjonsbehandling==4 & TidTilReperfusjon<=90)] <- 1

################################################################################
#Type reperfusjon
Trombolysebehandling <- rep(0,length(Trombolyse))
Trombolysebehandling[which(Trombolyse==1|Trombolyse==2)] <- 1
Angio <- rep(0,length(InvKunKorAngio))
Angio[which(InvKunKorAngio==1)] <- 1
PCI <- rep(0,length(InvPCI))
PCI[which(InvPCI==1)] <- 1

#Nevner: sykehistorie < 12 timer, STEMI, <85 år, utenfor sykehus, EKG prehosp, basisskjema & (trombolyse, angio eller PCI - der ango/PCI er inne 12 timer fra innleggelse til innstikk),
Nevner <- rep(0,length(Innleggelsesaar))
Nevner[which(HarBasisSkjema==1 & df$SamletStType==1 & df$PatientAge<85 & df$HvorBefantPasientenSeg==1 & (df$PrehospEKGLokasjon==1|df$PrehospEKG==1) &  Reperfusjonsbehandling>0 &  Symptomdebut_FMK<=720 & !is.na(TidTilReperfusjon) & !is.na(Symptomdebut_FMK))] <- 1

Teller <- rep(0,length(Innleggelsesaar))
Teller[which(Nevner==1 & ReperfusjonAnbefaltTid==1)] <- 1

#Nevner_IndC <- Nevner
#Teller_IndC <- Teller
