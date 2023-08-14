
sejdigModif<-read.csv2("./data/sejour2.csv",header = T)

load("./data/ZoneDesHop20.RData")
load("./data/ZoneDesHop40.RData")
load("./data/ZoneDesHop60.RData")
load("./data/ZoneDesHop80.RData")
ZoneDesHop100<-readRDS("./data/ZoneDesHop100.RDS")
listeHop<-readRDS("./data/listeHop.RDS")

# Sample list of hospital names
hospital_names <-names(ZoneDesHop100)
anonymized_codes <- paste("Hospital", seq_along(hospital_names))
saveRDS(anonymized_codes,file = "listeHop.RDS")

hospital_mapping <- setNames(anonymized_codes, hospital_names)

(nms_1 <- names(ZoneDesHop40))
names(ZoneDesHop40) <- unname(hospital_mapping[nms_1])


(nms_2 <- names(ZoneDesHop80))
names(ZoneDesHop80) <- unname(hospital_mapping[nms_2])


(nms_3 <- names(ZoneDesHop100))
names(ZoneDesHop100) <- unname(hospital_mapping[nms_3])


sejdigModif$Libéllé.CHIC
disease_mapping <- unique(sejdigModif$Libéllé.CHIC)
generic_terms <- paste0("Disease", seq_along(disease_mapping))
sejdigModif$Disease <- generic_terms[match(sejdigModif$Libéllé.CHIC, disease_mapping)]

sejdigModif$Hospital<-unname(hospital_mapping[sejdigModif$Hopital])

table(sejdigModif$Hospital)
table(sejdigModif$Hopital)

saveRDS(ZoneDesHop40,file = "ZoneDesHop40.RDS")
saveRDS(ZoneDesHop80,file = "ZoneDesHop80.RDS")
saveRDS(ZoneDesHop100,file = "ZoneDesHop100.RDS")


sejdigModif<-sejdigModif%>%select(
  -c(Hopital,Libéllé.CHIC,GHM,dp,annee1)
)

names(sejdigModif)

sejdigModif<-sejdigModif%>%rename(
  hospital_code=finessGeoDP,Age=Majorite,Emmergency=Urgence,severity=Diagnostic,
  Gender=sexe
)

sejdigModif$Age[sejdigModif$Age=="Majeur"] <-"Adult"
sejdigModif$Age[sejdigModif$Age=="Mineur"] <-"Underage"

sejdigModif$Gender[sejdigModif$Gender=="Femme"] <-"Woman"
sejdigModif$Gender[sejdigModif$Gender=="Homme"] <-"Man"

sejdigModif$Emmergency[sejdigModif$Emmergency=="Oui"] <-"Yes"
sejdigModif$Emmergency[sejdigModif$Emmergency=="Non"] <-"No"

sejdigModif$severity[sejdigModif$severity=="Sans cancer"] <-"No"
sejdigModif$severity[sejdigModif$severity=="Cancer"] <-"Yes"

saveRDS(sejdigModif,file = "sejour.RDS")


