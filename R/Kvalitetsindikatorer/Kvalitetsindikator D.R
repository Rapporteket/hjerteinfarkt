variabelnavn <- colnames(df)

SykehusForlop <- df$SykehusForlop

Innleggelsesaar <- df$Innleggelsesaar

################################################################################
# Kvalitetsindikator D - Invasivt utredet ved NSTEMI ###########################
################################################################################
N_totalt <- dim(df)[1]

#Fjerne de med kontraindikasjoner fom 2024. Kontraindikasjoner: -1 = Velg verdi, 1 = Pasienten ønsket ikke invasiv utredning, 2 = Kontraindikasjoner mot invasiv utredning (alder, medisinsk tilstand),
#3 = Pasienten er tidligere invasivt utredet i samme sykdomsforløp, 4 = Andre årsaker, 9 = Ukjent
IkkeKontraindikasjon <- rep(0,nrow(df))
IkkeKontraindikasjon[which(df$InvAarsakIkkeUtfort==-1| df$InvAarsakIkkeUtfort==3 |df$InvAarsakIkkeUtfort==4 |df$InvAarsakIkkeUtfort==9 )] <- 1

#Nevner
Nevner <- rep(0,nrow(df))
Nevner[which(df$SamletStType==0 & df$PatientAge<85 & df$HvorBefantPasientenSeg==1 & df$ValidererBasisSkjema=="1" & IkkeKontraindikasjon==1)] <- 1
table(Nevner,Innleggelsesaar)

#prosedyrer i forløp
ProsedyreIForlop <- df$ProsedyreIForlop
ProsedyreEtPlan <- df$ProsedyreEtPlan
UtskrivesTil2 <- df$UtskrivesTil2

#Antall overfxrt til angio/PCI bede i forlxp og ved slutten av forlxpet -> mangler muligens skjema fra siste sykehus
#Overfxrt angio/PCI bede i forlxp og til slutt i forlxpet
Overfort_prosedyre <- which(ProsedyreIForlop==1 & ProsedyreEtPlan==1)
length(Overfort_prosedyre)

#Overfxrt angio/PCI men invasiv utredning ikke utfxrt
Overfort_invasiv <- which(ProsedyreIForlop==1 & is.na(df$InvasivtSykehus))

Registrert_InvasivtSykehus <- ifelse(grepl(102402,df$SykehusForlop) |grepl(102966,df$SykehusForlop)|grepl(104284,df$SykehusForlop)|grepl(109880,df$SykehusForlop)|
                                       grepl(700263,df$SykehusForlop)|   grepl(700726,df$SykehusForlop)|   grepl(701317,df$SykehusForlop)| grepl(706214,df$SykehusForlop)|
                                       grepl(4218014,df$SykehusForlop)|grepl(700422,df$SykehusForlop),"Registrert Invasivt sykehus","Ikke registrert Invasivt sykehus")

Overfort_angio <- rep(0,dim(df)[1])
Overfort_angio[which(ProsedyreIForlop==1)] <- 1

Ut_angio <- rep(0,dim(df)[1])
Ut_angio[which(ProsedyreEtPlan==1)] <- 1

Invasivt_forlop <- rep(0,dim(df)[1])
Invasivt_forlop[which(Registrert_InvasivtSykehus=="Registrert Invasivt sykehus")] <- 1
table(Overfort_angio,Registrert_InvasivtSykehus)

id2 <- which(Ut_angio==1 & Invasivt_forlop==0)
length(id2)

id <- which(Overfort_angio==1 & Invasivt_forlop==0)
length(id)
df$SkjemaGUID[id][1:10]


#alle som oppfyller id skal ekskluderes fra nevneren i indikator D - sjekk hvordan det pevirker resultatene
Nevner_ny <- Nevner
Nevner_ny[id] <- 0

###################################################################################
#Teller
InvKunKorAngio <- df$InvKunKorAngio
InvPCI <- df$InvPCI

Angio <- rep(0,length(InvKunKorAngio))
Angio[which(InvKunKorAngio==1)] <- 1
PCI <- rep(0,length(InvPCI))
PCI[which(InvPCI==1)] <- 1

AngioEllerPCI <- rep(0,length(InvKunKorAngio))
AngioEllerPCI[which(Angio==1 | PCI==1)] <- 1

Teller <- rep(0,length(Nevner))
Teller[which(Nevner==1 & (AngioEllerPCI==1|df$InvCTkorangio==1))] <- 1


Nevner <- Nevner_ny
Teller <- Teller

Nevner_IndD <- Nevner_ny
Teller_IndD <- Teller

Nevner <- Nevner_IndD
Teller <- Teller_IndD
