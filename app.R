##LES PACKAGES-------------
library(shiny);library(shinydashboard);library(tidyverse);library(stringr);
library(DescTools);library(ggsci);library(lubridate);library(dplyr);
library(sf);library(sp);library(rgdal);library(readxl);library(RColorBrewer);
library(leaflet);library(rmapshaper);library(gridExtra);library(avallecam)
library(maptools);library(geosphere);library(leaflet.minicharts);library(DT)



## IMPORTING DATASETS AND DATAMANAGEMENT
LibCom<-read.csv2("./data/LibCom.csv",header = T)
LibCom<-LibCom%>%filter(substr(codeGeo,1,2)%in%c(
  "75","77","78","91","92","93","94","95"))

sejdig<-readRDS("./data/sejour2.RDS")
sejdig$Age<-as.character(sejdig$Age)
CoordHop<-sejdig%>%distinct(Hospital,.keep_all = T)
CoordHop$latHop<-as.numeric(as.character(CoordHop$latHop))
CoordHop$lngHop<-as.numeric(as.character(CoordHop$lngHop))
sejdig$Disease<-as.factor(sejdig$Disease)

sejourCom<-sejdig%>%filter(substr(codeGeo,1,2)%in%c(
  "75","77","78","91","92","93","94","95"))

sejourCom2<-sejourCom%>%left_join(LibCom,by="codeGeo")

#LES SEJOURS PAR ZONE DE RECRUTEMENT
ZoneDesHop40<-readRDS("./data/ZoneDesHop40.RDS")
ZoneDesHop80<-readRDS("./data/ZoneDesHop80.RDS")
ZoneDesHop100<-readRDS("./data/ZoneDesHop100.RDS")
listeHop<-unique(sejdig$Hospital)
#DONNEES POPULATION
popCom <-read_xlsx("./data/codepost2020.xlsx", col_names = TRUE)
popCom <- popCom %>%
  rename(code_pmsi="Code géographique PMSI 2020",
         pop_pmsi="Population 2017 desservie par le code géo PMSI") %>%
  select(code_pmsi,pop_pmsi) %>%
  distinct(code_pmsi, .keep_all = TRUE)


##LES SHAPEFILES------------------------------------------------------------

#SHAPEFILE DEPARTEMENT
shpDpt<-st_read("./data/departements-20180101.shp") %>%
  st_transform(crs="+init=epsg:4326") %>% ms_simplify(.)
names(st_geometry(shpDpt)) = NULL

# #Shapefile commune
shpCom0<-st_read("./data/atih_metropole_fondpmsi2017_z.shp") %>%
  st_transform(crs="+init=epsg:4326") %>%  ms_simplify(.)
names(st_geometry(shpCom0)) = NULL
shpCom<-shpCom0
shpCom$code_pmsi<-as.character(shpCom$code_pmsi)
shpCom<-shpCom%>%left_join(popCom, by="code_pmsi")

#Retrieve latitude and longitude out of shapefile
geoComfl<-shpCom0
geoComfl$code_pmsi<-as.character(geoComfl$code_pmsi)
sf_use_s2(FALSE)
geoComfl$centroid <- st_centroid(geoComfl$geometry) #Calcul du centre des geométrie
geoComfl<-st_drop_geometry(geoComfl) #Enlever geometrie POLYGON
geoComfl<-st_as_sf(geoComfl) #Retransformer en spatial data avec géométrie POINT
geoComfl<-geoComfl%>%
  st_coordinates_tidy()%>%#Extraction à partir de centroid des coordonnées géo
  rename(Com_lng=X,Com_lat=Y,codeGeo=code_pmsi)
geoComfl$Com_lat<-as.numeric(as.character(geoComfl$Com_lat))
geoComfl$Com_lng<-as.numeric(as.character(geoComfl$Com_lng))


#FUNCTION ALLOWING TO FILTER DATASETS

get_filtered_data<- function(data,severe,age,urgency,speciality){
  if(severe=="Total"&urgency=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data
  }else if(severe=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Emmergency==urgency
      )
  }else if (severe=="Total"&urgency=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality
      )

  }else if(severe=="Total"&urgency=="Total"&speciality=="Total"){
    resultat<-data%>%
      filter(
        Age==age
      )

  }else if(urgency=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        severity==severe
      )
  }else if(severe=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality,
        Emmergency==urgency
      )

  }else if(severe=="Total"&speciality=="Total"){
    resultat<-data%>%
      filter(
        Age==age,
        Emmergency==urgency
      )

  }else if (severe=="Total"&urgency=="Total"){

    resultat<-data%>%
      filter(
        Disease==speciality,
        Age==age
      )

  }else if(speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Emmergency==urgency
      )


  }else if(speciality=="Total"&urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Age==age
      )

  }else if(age=="Total"&urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality
      )
  }else if(severe=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality,
        Age==age,
        Emmergency==urgency
      )

  }else if(speciality=="Total"){

    resultat<-data%>%
      filter(
        severity==severe,
        Age==age,
        Emmergency==urgency
      )

  }else if (urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Age==age
      )
  }else if (age=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Emmergency==urgency
      )

  }else{
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Age==age,
        Emmergency==urgency
      )

  }
  return(resultat)
}




get_filtered_dataZone <- function(data,severe,age,ZipCode,urgency,speciality){
  if(severe=="Total"&urgency=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        codeGeo%in%ZipCode
      )
  }else if(severe=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )
  }else if (severe=="Total"&urgency=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality,
        codeGeo%in%ZipCode
      )

  }else if(severe=="Total"&urgency=="Total"&speciality=="Total"){
    resultat<-data%>%
      filter(
        Age==age,
        codeGeo%in%ZipCode
      )

  }else if(urgency=="Total"&speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
      severity==severe,
      codeGeo%in%ZipCode
    )
  }else if(severe=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }else if(severe=="Total"&speciality=="Total"){
    resultat<-data%>%
      filter(
        Age==age,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }else if (severe=="Total"&urgency=="Total"){

    resultat<-data%>%
      filter(
        Disease==speciality,
        Age==age,
        codeGeo%in%ZipCode
      )

  }else if(speciality=="Total"&age=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )


  }else if(speciality=="Total"&urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Age==age,
        codeGeo%in%ZipCode
      )

  }else if(age=="Total"&urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        codeGeo%in%ZipCode
      )
  }else if(severe=="Total"){
    resultat<-data%>%
      filter(
        Disease==speciality,
        Age==age,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }else if(speciality=="Total"){

    resultat<-data%>%
      filter(
        severity==severe,
        Age==age,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }else if (urgency=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Age==age,
        codeGeo%in%ZipCode
      )
  }else if (age=="Total"){
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }else{
    resultat<-data%>%
      filter(
        severity==severe,
        Disease==speciality,
        Age==age,
        Emmergency==urgency,
        codeGeo%in%ZipCode
      )

  }
  return(resultat)
}






## FUNCTIONS ALLOWING TO CREATE---------------------------

# FUNCTION 1
get_map_commune <- function(data,var,palette,Hop,zoom){
  if(nrow(data)!=0|(length(data)&dim(data)&length(data)!=0)){
    # Choix de la variable
    if(var=="nb_patient_tot"){
      label <- "Nb de sejours"
      mypalette <- palette$tot
      data <- data %>% rename(var=nb_patient_tot)
    }else{
      label <- "Nb sej./10000 hbt."
      mypalette <- palette$pop
      data <- data %>% rename(var=nb_patient_pop) %>%
        mutate(var = round(var, digits = 2))
    }
    # Text for tooltips
    mytext <- paste("<b>", data$libgeo_cp,"</b>","<br/>",
                    "Code PMSI : ", data$code_pmsi,"<br/>",
                    label," : ",data$var,
                    sep="") %>%
      lapply(htmltools::HTML)
    # Carte


    leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
              lat = CoordHop[CoordHop$Hospital==Hop,"latHop"],zoom = zoom) %>%
      addMarkers(layerId = Hop, lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
                 lat = CoordHop[CoordHop$Hospital==Hop,"latHop"], popup = Hop) %>%
      addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                  fillColor = "white",stroke=TRUE, weight = 2)%>%
      addPolygons(
        fillColor = ~mypalette(var),
        stroke=TRUE,
        fillOpacity = 0.7,
        color="white",
        weight=0.3,
        dashArray = "3",
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "left")) %>%
      addLegend( pal=mypalette, values=~var, opacity=0.9,
                 title = label, position = "topright" )
  }else{
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
              lat = CoordHop[CoordHop$Hospital==Hop,"latHop"],zoom = 10) %>%
      addMarkers(layerId = Hop, lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
                 lat = CoordHop[CoordHop$Hospital==Hop,"latHop"], popup = Hop)
  }
}

# FUNCTION 2
get_MktShare<-function(data1,zoneRec,zoneCol,colors,vlng,vlat,lHop1,lHop2,zoom){

  if(nrow(data1)!=0){

    leaflet(data1)%>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = vlng, lat = vlat, zoom = zoom) %>% # coord. Créteil (pref. Val de Marne)
      addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                  fillColor = "white",stroke=TRUE, weight = 3)%>%
      addPolygons(data =zoneRec,color = "brown",fillOpacity =0.3 ,
                  fillColor = zoneCol,stroke=TRUE, weight = 1)%>%
      addMinicharts(
        data1$Com_lng, data1$Com_lat,
        type = "pie",
        chartdata = data1[, c("Sej.Hop1", "Sej.Hop2", "Autres")],
        colorPalette = colors,
        popup=popupArgs(
          labels=c(lHop1,lHop2, "Autres centres"),
          html=paste0("<h5>","<b>", data1$libgeo_cp,"</b>","</h5>",
                      "<p style='font-size:8px'>",
                      "<b>","Code PMSI : ","</b>", data1$codeGeo, "<br>",
                      "<b>","Nombre sejours:","</b>",data1$sejour.com,"<br>",
                      lHop1,": ",round(data1$partHop1,1),"%","<br>",
                      lHop2,": ",round(data1$partHop2,1),"%","</p>")

        ),
        width = 3*7 * sqrt(sqrt(data1$sejour.com)/sqrt(max(data1$sejour.com)))

      )}else{

        leaflet(data1)%>% addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = vlng, lat = vlat, zoom = 10) %>% # coord. Créteil (pref. Val de Marne)
          addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                      fillColor = "white",stroke=TRUE, weight = 3)%>%
          addPolygons(data =zoneRec,color = "brown",fillOpacity =0.3 ,
                      fillColor = zoneCol,stroke=TRUE, weight = 1)
      }

}



## FUNCTION 3
get_map_Conc <- function(data,Hop,zoom,dataHop1,dataHop2,
                         lng,lat,radius,ZoneCompet,mypalette,
                         values,mytextZone){
  if(nrow(data)!=0|(length(data)&dim(data)&length(data)!=0)){


    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
       setView(lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
              lat = CoordHop[CoordHop$Hospital==Hop,"latHop"],zoom = zoom)%>%
      addMarkers(layerId = Hop, lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
                 lat = CoordHop[CoordHop$Hospital==Hop,"latHop"], popup = Hop) %>%
      addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                  fillColor = "white",stroke=TRUE, weight = 2)%>%
    addPolygons(data = ZoneCompet,
      fillColor = "lightblue",
      stroke=TRUE,
      fillOpacity = 0.4,
      color="white",
      weight=0.3,
      dashArray = "3",
      label = mytextZone,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "10px",
        direction = "left"))  %>%
    addCircles(data=data,lng=lng,lat=lat,radius = radius,color = "blue",
               popup =~ paste(Hospital,":",nb_patient,"(",pct,"%)")) %>%
      addCircles(data=dataHop1,lng=lng,lat=lat,radius = radius,color = "red",
                 popup =~ paste(Hospital,":",nb_patient,"(",pct,"%)")) %>%
     addCircles(data=dataHop2,lng=lng,lat=lat,radius = radius,color = "green",
                popup =~ paste(Hospital,":",nb_patient,"(",pct,"%)"))%>%
      addLegend(pal=mypalette, values=values, opacity=0.9,
                position = "topright")
  }else{
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron)%>%
      setView(lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
              lat = CoordHop[CoordHop$Hospital==Hop,"latHop"],zoom = 10) %>%
      addMarkers(layerId = Hop, lng = CoordHop[CoordHop$Hospital==Hop,"lngHop"],
                 lat = CoordHop[CoordHop$Hospital==Hop,"latHop"], popup = Hop)
  }
}

# FUNCTION 4
get_map <- function(data, var="nb_patients",
                    titre_legende="Nb de sejours", palette, mybins,zoom){  # je fixe une valeur par défaut
  # pour var et titre_légende
  # Je renome la variable à représenter en "var"
  names(data) <- gsub(var,"var",names(data))

  # Palette de couleur
  mypalette <- colorBin(palette,bins=mybins,na.color = "transparent")

  # Text for tooltips
  mytext <- paste(
    "<b>", data$libgeo_cp,"</b>","<br/>",
    "Code PMSI : ", data$code_pmsi,"<br/>",
    titre_legende," : ", round(data$var,digits = 2),
    sep="") %>%
    lapply(htmltools::HTML)

  # Carte
  map <- leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 2.5169676, lat = 48.7592362, zoom = 9) %>%          # coordonnées GPS du CHIV

    addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                fillColor = "white",stroke=TRUE, weight = 2)%>%
    addPolygons(
      fillColor = ~mypalette(var),
      stroke=TRUE,
      fillOpacity = 0.7,
      color="white",
      weight=0.3,
      dashArray = "3",
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.8,
        bringToFront = TRUE),
      label = mytext,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "right")) %>%
    addLegend( pal=mypalette, values=~var, opacity=0.9,
               title = titre_legende, position = "topright" )
  map
}


# FUNCTION 5
get_map1 <- function(data,data2, var="nb_patients",
                     titre_legende="Nb de sejours", palette, mybins,zoom){  # je fixe une valeur par défaut
  # pour var et titre_légende
  # Je renome la variable à représenter en "var"
  names(data) <- gsub(var,"var",names(data))
  names(data2)<-gsub(var,"var",names(data2))
  # Palette de couleur
  mypalette <- colorBin(palette,bins=mybins,na.color = "transparent")

  # Text for tooltips
  mytext <- paste(
    "<b>", data$libgeo_cp,"</b>","<br/>",
    "Code PMSI : ", data$code_pmsi,"<br/>",
    "Nombre de séjours"," : ", round(data$var,digits = 2),
    sep="") %>%
    lapply(htmltools::HTML)

  mytext2 <- paste(
    "<b>", data2$libgeo_cp,"</b>","<br/>",
    "Code PMSI : ", data2$code_pmsi,"<br/>",
    titre_legende," : ", round(data2$var,digits = 2),
    sep="") %>%
    lapply(htmltools::HTML)

  # Carte
  map <- leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 2.5169676, lat = 48.7592362, zoom = 9) %>%          # coordonnées GPS du CHIV

    addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                fillColor = "white",stroke=TRUE, weight = 2)%>%
    addPolygons(
      fillColor = ~mypalette(var),
      stroke=TRUE,
      fillOpacity = 1,
      color="white",
      weight=0.3,
      dashArray = "3",
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = mytext,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "right")) %>%
    # coordonnées GPS du CHIV
    addPolygons(data = data2,
                fillColor = ~mypalette(var),
                stroke=TRUE,
                fillOpacity = 0,
                color="blue",
                weight=1.8,
                dashArray = "1",
                highlight = highlightOptions(
                  weight = 3,
                  color = "blue",
                  dashArray = "",
                  fillOpacity = 0,
                  bringToFront = TRUE),
                label = mytext2,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "right"))%>%
    addLegend( pal=mypalette, values=~var, opacity=0.9,
               title = titre_legende, position = "topright" )
  map
}

# FUNCTION 6
get_map2 <- function(data,data2, var="nb_patients",var2="nb_patients",
                    titre_legende="Nb de sejours", palette, mybins,zoom){  # je fixe une valeur par défaut
  # pour var et titre_légende
  # Je renome la variable à représenter en "var"
  names(data) <- gsub(var,"var",names(data))
  names(data2)<-gsub(var,"var",names(data2))
  names(data) <- gsub(var2,"var2",names(data))
  names(data2)<-gsub(var2,"var2",names(data2))

  # Palette de couleur
  mypalette <- colorFactor(palette,domain=mybins,na.color = "transparent")

  # Text for tooltips
  mytext <- paste(
    "<b>", data$libgeo_cp,"</b>","<br/>",
    "Code PMSI : ", data$code_pmsi,"<br/>",
    titre_legende," : ", round(data$var,digits = 2),
    sep="") %>%
    lapply(htmltools::HTML)

  mytext2 <- paste(
    "<b>", data2$libgeo_cp,"</b>","<br/>",
    "Code PMSI : ", data2$code_pmsi,"<br/>",
    titre_legende," : ", round(data2$var,digits = 2),
    sep="") %>%
    lapply(htmltools::HTML)

  # Carte
  map <- leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 2.5169676, lat = 48.7592362, zoom = 9) %>%          # coordonnées GPS du CHIV

    addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                fillColor = "white",stroke=TRUE, weight = 2)%>%
    addPolygons(
      fillColor = ~mypalette(var2),
      stroke=TRUE,
      fillOpacity = 0.7,
      color="white",
      weight=0.3,
      dashArray = "3",
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.8,
        bringToFront = TRUE),
      label = mytext,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "right")) %>%
              # coordonnées GPS du CHIV
    addPolygons(data = data2,
      fillColor = ~mypalette(var2),
      stroke=TRUE,
      fillOpacity = 0,
      color="red",
      weight=1.8,
      dashArray = "1",
      highlight = highlightOptions(
        weight = 3,
        color = "red",
        dashArray = "",
        fillOpacity = 0,
        bringToFront = TRUE),
      label = mytext2,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "right"))%>%
    addLegend( pal=mypalette, values=~var2, opacity=0.9,
               title = titre_legende, position = "topright" )
  map
}



get_flowmap<-function(dataFlow,vlng,vlat,
                      Com,lwd){

  flowLine<-st_drop_geometry(dataFlow)
  flowLine2 <- gcIntermediate(flowLine[,3:4], flowLine[,c(7,8)],
                              sp = TRUE, addStartEnd = TRUE)
  flowLine2$nbre <- flowLine$nb_patient
  flowLine2$origines <- as.character(flowLine$libgeo_cp)
  flowLine2$destinations <- as.character(flowLine$Hospital)

  flowLine2Com<-flowLine2[flowLine2$origines==Com,]

  Label <- paste0(flowLine2Com$origines, " a ",
                  flowLine2Com$destinations, ': ',
                   as.character(flowLine2Com$nbre))



  pal <- colorFactor(c("transparent"), flowLine2Com$destinations)

  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                fillColor = "white",stroke=TRUE, weight = 3)%>%

    addCircles(data=dataFlow,lng = ~ lngHop, lat = ~ latHop, weight = 0,
               radius = ~300,
               popup = ~ paste(Hospital),
               color = "darkred", fillOpacity = 0.7)%>%
    addCircles(data=dataFlow,lng = ~ Com_lng, lat = ~ Com_lat, weight = 0,
               radius = ~30,
               popup = ~ paste(libgeo_cp),
               color = "green", fillOpacity = 0.7)%>%
    setView(lng = vlng, lat =vlat,  zoom = 10) %>%          # coordonnees GPS du CHIV
    addPolylines(data = flowLine2Com, weight = ~ nbre/lwd,
                 label = Label,
                 group = ~origines, color = ~pal(destinations))

}




## INTERFACE USER----------------------------

ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Analyses de la concurrence inter-hospitalière en IDF"),
  dashboardSidebar(width = 250,div(style="font-size:10px;",
                                   sidebarMenu(
                                     menuItem( "Competition analysis",
                                               menuSubItem("Map of hospitals", tabName = "StatHopital"),
                                               menuSubItem("Map of city",tabName = "StatCommune"),

                                                           div(style="font-size:12px;",


                                                               
                                                               selectInput("disease", 
                                                                           label = div(style = "display: flex; align-items: center;",
                                                                                       HTML(paste0('<i class="fas fa-user-md" style="color: white; margin-right: 5px;" title="Doctor"></i>')),
                                                                                       "Speciality"),
                                                                           choices = c("Total",levels(as.factor(sejdig$Disease))),
                                                                           selected="Total"
                                                               ),
                                                               
                                                              

                                                               uiOutput("ui"),

                                                              
                                                               
                                                               selectInput("emmergency", 
                                                                           label = div(style = "display: flex; align-items: center;",
                                                                                       HTML(paste0('<i class="fas fa-ambulance" style="color: white; margin-right: 5px;" title="Ambulance"></i>')),
                                                                                       "Emmergency"),
                                                                           choices = c("Total",levels(as.factor(sejdig$Emmergency))),
                                                                           selected="Total"
                                                               ),

                                                               selectInput(inputId = "age",label=div(style = "display: flex; align-items: center;",
                                                                                                     HTML(paste0('<i class="fas fa-user" style="color: white; margin-right: 10px;" title="Doctor"></i>')),
                                                                                                     HTML(paste0('<i class="fas fa-child" style="color: white; margin-right: 5px;" title="Child"></i>')),
                                                                                                     "Status"),
                                                                           choices = c("Total",levels(as.factor(sejdig$Age))),
                                                                           selected="Total"),

                                                               selectInput(inputId = "area",label=div(style = "display: flex; align-items: center;",
                                                                                                      HTML(paste0('<i class="fas fa-bullseye" style="color: white; margin-right: 5px;" title="Child"></i>')),
                                                                                                      "Type of area"),
                                                                           choices = c("All patients","Recruitment area","Attraction area"),
                                                                           selected="Recruitment area"),

                                                               selectInput(inputId ="indicator",
                                                                           label = div(style = "display: flex; align-items: center;",
                                                                                       HTML(paste0('<i class="fas fa-chart-line" style="color: white; margin-right: 5px;" title=""></i>')),
                                                                                       "Indicators"),
                                                                           choices = c("Patient number"="nb_patient_tot",
                                                                                       "Patient number by population"="nb_patient_pop")))



                                                           )







                                   )
  )
  ),
  dashboardBody(
    tags$style(type = "text/css",
               "html, body {width:100%;height:100%}",

               ".leaflet .legend {
                                         font-size: 8px;
                                       line-height: 6px;}",

               ".leaflet .legend i{
                                       width: 8px;
                                       height: 8px;
                                       }"
    ),

    tabItems(

      tabItem(
        tabName = "StatHopital",div(style="font-size:11px;",

                                    fluidRow(box(selectInput(inputId = "hospital1",choices = c(listeHop),
                                                             label = "choose the hospital 1"),
                                                 width = 6,height = 70,
                                                 solidHeader = TRUE),
                                             box(selectInput(inputId = "hospital2",choices = rev(c(listeHop)),
                                                             label = "Choose the hospital 2"),
                                                 width = 6,height = 70,
                                                 solidHeader = TRUE)),
                                    fluidRow( tabsetPanel(
                                      tabPanel("Zone de recrutement",
                                               box(leafletOutput("ZoneHop1")),box(leafletOutput("ZoneHop2"))
                                      ),
                                      tabPanel("Market share", box(leafletOutput("MktShareZ1")),
                                               box(leafletOutput("MktShareZ2"))),
                                      tabPanel("Competition", leafletOutput("Comp1"))
                                    ))
        )),

      tabItem(
        tabName = "StatCommune",div(style="font-size:11px;",

                                    fluidRow(box(selectInput(inputId = "city",choices = c(levels(as.factor(LibCom$libgeo_cp))),
                                                             label = "Choose the city"),
                                                 width = 6,height = 70,
                                                 solidHeader = TRUE)),
                                    fluidRow( tabsetPanel(
                                      tabPanel("Number of stay by city",
                                               leafletOutput("sejourCom")
                                      ),
                                      tabPanel("Competition in the city",
                                               leafletOutput("HHI_IDF")
                                      ),
                                      tabPanel("Flow from city toward hospitals",
                                               leafletOutput ("Commune2")),
                                      tabPanel("Flow from city toward hospitals",
                                               dataTableOutput ("Commune3"))
                                    ))
        ))
    )



    )
)


#
#SERVEUR----------------------------------------------------------------------

server <- function(input, output) {

  output$ui <- renderUI({
    if (is.null(input$disease))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$disease,
           "Total" = selectInput("ssDiag", "severity",
                                 choices = c("Total","Severe","No severe")),
           "Disease1" = selectInput("ssDiag", "severity",
                                                             choices = c("Total","Severe","No severe")),
           "Disease2" = selectInput("ssDiag", "severity",
                                                                choices = c("Total","Severe","No severe")),
           "Disease3" = selectInput("ssDiag", "severity",
                                       choices = c("Total","Severe","No severe")),
           "Disease4" = selectInput("ssDiag", "severity",
                                               choices = c("Total","Severe","No severe")),

           "Disease5" = selectInput("ssDiag", "severity",
                                             choices = c("Total","Severe","No severe")),
           "Disease6" = selectInput("ssDiag", "severity",
                                       choices = c("Total","Severe","No severe")),
           "Disease7" = selectInput("ssDiag", "severity",
                                                                 choices = c("Total","Severe","No severe")),
           "Disease8" = selectInput("ssDiag", "severity",
                                                  choices = c("Total")),
           "Disease9" = selectInput("ssDiag", "severity",
                                                choices = c("Total","Severe","No severe")),
           "Disease10" = selectInput("ssDiag", "severity",
                                            choices = c("Total","Severe","No severe"))
    )
  })




##ZONE DE RECRUTEMENT DE l'HOPITAL 1----------------------------------------

  indicateur_carte <- reactive({
    input$indicator
  })

  ZoneDesHop<-reactive({

    if (input$area=="All patients"){
      ZoneDesHop100
    }else if(input$area=="Recruitment area"){
      ZoneDesHop80
    }else{
      ZoneDesHop40
    }
  })


  Zoom<-reactive({

    if (input$area=="All patients"){
      9
    }else if(input$area=="Recruitment area"){
      10
    }else{
      11
    }
  })



  ### Cartographie zone Hopital 1
  ZoneHop1<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode =ZoneDesHop()[[input$hospital1]],
    urgency = input$emmergency)})

  output$ZoneHop1 <- renderLeaflet({


    #  ZONE DE RECRUTEMENT / PALLETTE HOPITAL 1

    couleurs <- brewer.pal(name="YlOrRd",n=9)[2:9]
    # Palette pour Nombre de patients

    mybins_tot<-if (max(Max1(),Max2())>2500){
      c(0,10,50,100,250,500,1000,2000,3000)
    }else if(max(Max1(),Max2())<=2500&max(Max1(),Max2())>2000){
      c(0,10,50,100,250,500,1000,1500,2500)
    }else if(max(Max1(),Max2())<=2000&max(Max1(),Max2())>1500){
      c(0,10,50,100,250,500,1000,1500,2000)
    }else if (max(Max1(),Max2())<=1500&max(Max1(),Max2())>1000){
      c(0,10,50,100,250,500,750,1000,1500)
    }else if(max(Max1(),Max2())<=1000&max(Max1(),Max2())>750){
      c(0,10,30,50,100,250,500,750,1000)
    }else if(max(Max1(),Max2())<=750&max(Max1(),Max2())>500){
      c(0,10,20,30,50,100,250,500,750)
    }else if(max(Max1(),Max2())<=500&max(Max1(),Max2())>300){
      c(0,10,20,30,50,100,200,300,500)
    }else if(max(Max1(),Max2())<=300&max(Max1(),Max2())>200){
      c(0,10,20,30,40,50,100,200,300)
    }else if(max(Max1(),Max2())<=200&max(Max1(),Max2())>100){
      c(0,10,20,30,40,50,100,150,200)
    }else if(max(Max1(),Max2())<=100&max(Max1(),Max2())>50){
      c(0,5,10,15,20,30,50,75,100)
    }else if(max(Max1(),Max2())<=50&max(Max1(),Max2())>30){
      c(0,5,10,15,20,30,40,50)
    }else if (max(Max1(),Max2())<=30&max(Max1(),Max2())>20){
      c(0,5,10,15,20,25,30)
    }else if(max(Max1(),Max2())<=20&max(Max1(),Max2())>10){
      c(0,5,10,15,20)
    }else{c(0,1,2,5,10)}


    mypalette_tot <- colorBin(couleurs,bins=mybins_tot,na.color = "transparent")

    #Palette pour Nombre de patients rapporté à la population
    mybins_pop <- if (
      (unique(ZoneHop1()$severity)=="Severe"& length(unique(ZoneHop1()$severity))==1) &
      (unique(ZoneHop1()$Emmergency)=="Emmergency"& length(unique(ZoneHop1()$Emmergency))==1) &
      (unique(ZoneHop1()$Age)=="Underage"& length(unique(ZoneHop1()$Age))==1)
    ){
      c(0,0.5,1,2,3,4,5,6,10)
    }else if(
      (unique(ZoneHop1()$severity)=="Severe"& length(unique(ZoneHop1()$severity))==1) |
      (unique(ZoneHop1()$Age)=="Underage"& length(unique(ZoneHop1()$Age))==1)
    )
    {
      c(0,0.5,1,2,4,6,12,15,25)
    }else if((unique(ZoneHop1()$Emmergency)=="Emmergency"& length(unique(ZoneHop1()$Emmergency))==1))
    {
      c(0,1,2,3,4,5,10,20,40)
    }else{
      c(0,1,2,5,10,25,50,100,200)
    }
    mypalette_pop <- colorBin(couleurs,bins=mybins_pop,na.color = "transparent")

    # Sauvegarde des palettes dans une liste
    palette <- list(tot=mypalette_tot,pop=mypalette_pop)


    ZoneHop1bis<-ZoneHop1()%>%filter(Hospital==input$hospital1)

    ComRecHop1<-ZoneHop1bis%>%group_by(codeGeo)%>%
      add_tally(name = "nb_patient_tot") %>%  # nb de patient par commune
      ungroup()%>%distinct(codeGeo, .keep_all = TRUE)

    #FUSION DES TABLES POP HOPIT ET SHAPEFILES
    dfComRecHop1 <- shpCom %>%
      left_join(ComRecHop1,by=c("code_pmsi"="codeGeo")) %>%
      filter(!is.na(nb_patient_tot))%>%
      mutate(nb_patient_pop=10000*nb_patient_tot/pop_pmsi)

    get_map_commune(data=dfComRecHop1,var = indicateur_carte(),palette=palette,
                    Hop = input$hospital1,zoom = Zoom())


  })


  ##            ZONE DE RECRUTEMENT DE L'HOPITAL 2
  ### Cartographie zone Hopital 2

  ZoneHop2<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode =ZoneDesHop()[[input$hospital2]],
    urgency =input$emmergency)
  })

  output$ZoneHop2 <- renderLeaflet({


    # PALLETTE HOPITAL 2
    ## Palette pour Nombre de patients

    couleurs2 <- brewer.pal(name="YlOrRd",n=9)[2:9]

    mybins_tot2<-if (max(Max1(),Max2())>2500){
      c(0,10,50,100,250,500,1000,2000,3000)
    }else if(max(Max1(),Max2())<=2500&max(Max1(),Max2())>2000){
      c(0,10,50,100,250,500,1000,1500,2500)
    }else if(max(Max1(),Max2())<=2000&max(Max1(),Max2())>1500){
      c(0,10,50,100,250,500,1000,1500,2000)
    }else if (max(Max1(),Max2())<=1500&max(Max1(),Max2())>1000){
      c(0,10,50,100,250,500,750,1000,1500)
    }else if(max(Max1(),Max2())<=1000&max(Max1(),Max2())>750){
      c(0,10,30,50,100,250,500,750,1000)
    }else if(max(Max1(),Max2())<=750&max(Max1(),Max2())>500){
      c(0,10,20,30,50,100,250,500,750)
    }else if(max(Max1(),Max2())<=500&max(Max1(),Max2())>300){
      c(0,10,20,30,50,100,200,300,500)
    }else if(max(Max1(),Max2())<=300&max(Max1(),Max2())>200){
      c(0,10,20,30,40,50,100,200,300)
    }else if(max(Max1(),Max2())<=200&max(Max1(),Max2())>100){
      c(0,10,20,30,40,50,100,150,200)
    }else if(max(Max1(),Max2())<=100&max(Max1(),Max2())>50){
      c(0,5,10,15,20,30,50,75,100)
    }else if(max(Max1(),Max2())<=50&max(Max1(),Max2())>30){
      c(0,5,10,15,20,30,40,50)
    }else if (max(Max1(),Max2())<=30&max(Max1(),Max2())>20){
      c(0,5,10,15,20,25,30)
    }else if(max(Max1(),Max2())<=20&max(Max1(),Max2())>10){
      c(0,5,10,15,20)
    }else{c(0,1,2,5,10)}



    mypalette_tot2 <- colorBin(couleurs2,bins=mybins_tot2,na.color = "transparent")

    #Palette pour Nombre de patients rapporté à la population
    mybins_pop2 <- if (
      (unique(ZoneHop2()$severity)=="Severe"& length(unique(ZoneHop2()$severity))==1) &
      (unique(ZoneHop2()$Emmergency)=="Emmergency"& length(unique(ZoneHop2()$Emmergency))==1) &
      (unique(ZoneHop2()$Age)=="Underage"& length(unique(ZoneHop2()$Age))==1)
    ){
      c(0,0.5,1,2,3,4,5,6,10)
    }else if(
      (unique(ZoneHop2()$severity)=="Severe"& length(unique(ZoneHop2()$severity))==1) |
      (unique(ZoneHop2()$Age)=="Underage"& length(unique(ZoneHop2()$Age))==1)
    )
    {
      c(0,0.5,1,2,4,6,12,15,25)
    }else if((unique(ZoneHop2()$Emmergency)=="Emmergency"& length(unique(ZoneHop2()$Emmergency))==1))
    {
      c(0,1,2,3,4,5,10,20,40)
    }else{
      c(0,1,2,5,10,25,50,100,200)
    }
    mypalette_pop2 <- colorBin(couleurs2,bins=mybins_pop2,na.color = "transparent")

    # Sauvegarde des palettes dans une liste
    palette2 <- list(tot=mypalette_tot2,pop=mypalette_pop2)


    ZoneHop2bis<-ZoneHop2()%>%filter(Hospital==input$hospital2)

    ComRecHop2<-ZoneHop2bis%>%group_by(codeGeo)%>%
      add_tally(name = "nb_patient_tot") %>%  # nb de patient par commune
      ungroup()%>%distinct(codeGeo, .keep_all = TRUE)

    #FUSION DES TABLES POP HOPIT ET SHAPEFILES
    dfComRecHop2 <- shpCom %>%
      left_join(ComRecHop2,by=c("code_pmsi"="codeGeo")) %>%
      filter(!is.na(nb_patient_tot))%>%
      mutate(nb_patient_pop=10000*nb_patient_tot/pop_pmsi)

    get_map_commune(data=dfComRecHop2,var = indicateur_carte(),palette=palette2,
                    Hop = input$hospital2,zoom = Zoom())

  })


  Max1<-reactive({
    ZoneHop1bis<-ZoneHop1()%>%filter(Hospital==input$hospital1)

    ComRecHop1<-ZoneHop1bis%>%group_by(codeGeo,hospital_code)%>%
      add_tally(name = "nb_patient_tot") %>%  # nb de patient par commune
      ungroup()%>%distinct(codeGeo,hospital_code, .keep_all = TRUE)
        max(ComRecHop1$nb_patient_tot)
  })

  Max2<-reactive({
    ZoneHop2bis<-ZoneHop2()%>%filter(Hospital==input$hospital2)
    ComRecHop2<-ZoneHop2bis%>%group_by(codeGeo,hospital_code)%>%
      add_tally(name = "nb_patient_tot") %>%  # nb de patient par commune
      ungroup()%>%distinct(codeGeo,hospital_code, .keep_all = TRUE)
    max(ComRecHop2$nb_patient_tot)
  })


  ############################################################"
  ######### PART DE MARCHE ZONE HOPITAL 1
  #######################################################
  ZoneMk1<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode =ZoneDesHop()[[input$hospital1]],
    urgency =input$emmergency)})


  ZoneHop1.1<-reactive({ZoneMk1()
  })


  Zone01<-reactive({
    ZoneMk1()%>%select(codeGeo,Hospital)%>%distinct(codeGeo,.keep_all = T)

  })
  Zone1<-reactive({
    shpCom0%>%right_join(Zone01(),by=c("code_pmsi"="codeGeo"))
  })


  colors <- c("#FAA43A","#E5126F","#758bd1")

  #MARCHE DE L'HOPITAL 1 DANS SA PROPRE ZONE
  dfMktHop1_Z1<-reactive({ZoneHop1.1()%>%
      group_by(codeGeo,Hospital)%>%count()%>%# sejour par commune et par hopital
      ungroup()%>%group_by(codeGeo)%>%add_tally(n,name="sejour.com")%>%# sejour tot commune
      filter(Hospital==input$hospital1)%>%select(codeGeo,Hospital,n,sejour.com)%>%
      rename(Hospital1=Hospital,Sej.Hop1=n)})
  #################################################################
  #MARCHE DE L'HOPITAL 2 DANS LA ZONE DE L'HOPITAL 1
  dfMktHop2_Z1<-reactive({ZoneHop1.1()%>%group_by(codeGeo,Hospital)%>%
      count()%>%ungroup()%>%filter(Hospital==input$hospital2)%>%
      select(codeGeo,Hospital,n)%>%rename(Hospital2=Hospital,Sej.Hop2=n)})

  ############################################################
  dfMKtZ1<-reactive({dfMktHop1_Z1()%>%left_join(dfMktHop2_Z1(),by="codeGeo")%>%
      replace_na(list(Hospital2 = input$hospital2, Sej.Hop2 = 0,Hospital1= input$hospital1,Sej.Hop1=0))%>%
      mutate(partHop1=100*Sej.Hop1/sejour.com,partHop2=100*Sej.Hop2/sejour.com,
             Autres=(sejour.com-(Sej.Hop1+Sej.Hop2)))%>%
      select(codeGeo,Hospital1,Sej.Hop1,partHop1,Hospital2,Sej.Hop2,partHop2,Autres,
             sejour.com)})

  dfMKtZ1_F<-reactive({dfMKtZ1()%>%left_join(geoComfl,by=c("codeGeo"))%>%
      filter(!is.na(libgeo_cp))})

  output$Marche<-DT::renderDataTable({Zone1()})

  output$MktShareZ1 <- renderLeaflet({
    get_MktShare(data1 = dfMKtZ1_F(),zoneRec=Zone1(),
                 zoneCol="#B276B2",vlng=CoordHop[CoordHop$Hospital==input$hospital1,"lngHop"],
                 vlat=CoordHop[CoordHop$Hospital==input$hospital1,"latHop"],colors = colors,
                 lHop1 =input$hospital1,lHop2 = input$hospital2,zoom=Zoom() )
  })




  ######### PART DE MARCHE ZONE HOPITAL 2
  ZoneMk2<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode =ZoneDesHop()[[input$hospital2]],
    urgency = input$emmergency  )})


  ZoneHop2.2<-reactive({ZoneMk2()
  })


  Zone02<-reactive({
    ZoneMk2()%>%select(codeGeo,Hospital)%>%distinct(codeGeo,.keep_all = T)

  })
  Zone2<-reactive({
    shpCom0%>%right_join(Zone02(),by=c("code_pmsi"="codeGeo"))
  })


  colors2 <- c("#FAA43A","#E5126F","#758bd1")

  #MARCHE DE HOPITAL2 DANS SA PROPRE ZONE
  dfMktHop2_Z2<-reactive({ZoneHop2.2()%>%
      group_by(codeGeo,Hospital)%>%count()%>%# sejour par commune et par hopital
      ungroup()%>%group_by(codeGeo)%>%add_tally(n,name="sejour.com")%>%# sejour tot commune
      filter(Hospital==input$hospital2)%>%select(codeGeo,Hospital,n,sejour.com)%>%
      rename(Hospital2=Hospital,Sej.Hop2=n)})
  #################################################################
  #MARCHE DE HOPITAL1  DANS LA ZONE DE HOPITAL 2
  dfMktHop1_Z2<-reactive({ZoneHop2.2()%>%group_by(codeGeo,Hospital)%>%
      count()%>%ungroup()%>%filter(Hospital==input$hospital1)%>%
      select(codeGeo,Hospital,n)%>%rename(Hospital1=Hospital,Sej.Hop1=n)})

  ############################################################
  dfMKtZ2<-reactive({dfMktHop2_Z2()%>%left_join(dfMktHop1_Z2(),by="codeGeo")%>%
      replace_na(list(Hospital1 = input$hospital1, Sej.Hop1 = 0,Hospital2= input$hospital2,Sej.Hop2=0))%>%
      mutate(partHop2=100*Sej.Hop2/sejour.com,partHop1=100*Sej.Hop1/sejour.com,
             Autres=(sejour.com-(Sej.Hop2+Sej.Hop1)))%>%
      select(codeGeo,Hospital1,Sej.Hop1,partHop1,Hospital2,Sej.Hop2,partHop2,Autres,
             sejour.com)})

  dfMKtZ2_F<-reactive({dfMKtZ2()%>%left_join(geoComfl,by=c("codeGeo"))%>%
      filter(!is.na(libgeo_cp))})


  output$MktShareZ2 <- renderLeaflet({
    get_MktShare(data1 = dfMKtZ2_F(),zoneRec=Zone2(),
                 zoneCol="#B276B2",vlng=CoordHop[CoordHop$Hospital==input$hospital2,"lngHop"],
                 vlat=CoordHop[CoordHop$Hospital==input$hospital2,"latHop"],colors = colors2,
                 lHop1 =input$hospital1,lHop2 = input$hospital2,Zoom() )
  })



  ## 5 HOPITAUX CONCURRENTS


  ZoneConc1<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode =ZoneDesHop()[[input$hospital1]],
    urgency =  input$emmergency)})


  ZoneComp<-reactive({
    ZoneConc1()%>%select(codeGeo,Hospital)%>%distinct(codeGeo,.keep_all = T)

  })
  ZoneCompet<-reactive({
    shpCom0%>%right_join(ZoneComp(),by=c("code_pmsi"="codeGeo"))
  })


  dfHop1_Conc01<-reactive({ZoneConc1()%>%select(codeGeo,Hospital,lngHop,latHop)%>%
      group_by(Hospital) %>%add_tally(name = "nb_patient")%>%# nb de patient par commune
      distinct(Hospital,.keep_all = T)%>%
      arrange(desc(nb_patient))%>%ungroup()%>%
      mutate(pct = round(nb_patient/sum(nb_patient)*100,1),
             cum_pct = cumsum(pct),
             lngHop= as.numeric(as.character(lngHop)),
             latHop= as.numeric(as.character(latHop))) %>%top_n(n=-5)
  })

  dfHop1_Conc1<-reactive({
    shpCom%>%left_join(dfHop1_Conc01() ,by=c("code_pmsi"="codeGeo"))%>%
    filter(!is.na(nb_patient))
    })


  dataHop1.1<-reactive({ZoneConc1()%>%select(codeGeo,Hospital,lngHop,latHop)%>%
      group_by(Hospital) %>%add_tally(name = "nb_patient")%>%# nb de patient par commune
      distinct(Hospital,.keep_all = T)%>%
      arrange(desc(nb_patient))%>%ungroup()%>%
      mutate(pct = round(nb_patient/sum(nb_patient)*100,1),
             cum_pct = cumsum(pct),
             lngHop= as.numeric(as.character(lngHop)),
             latHop= as.numeric(as.character(latHop))) %>%
      filter(Hospital==input$hospital1)
  })



  dataHop1.2<-reactive({ZoneConc1()%>%select(codeGeo,Hospital,lngHop,latHop)%>%
      group_by(Hospital) %>%add_tally(name = "nb_patient")%>%# nb de patient par commune
      distinct(Hospital,.keep_all = T)%>%
      arrange(desc(nb_patient))%>%ungroup()%>%
      mutate(pct = round(nb_patient/sum(nb_patient)*100,1),
             cum_pct = cumsum(pct),
             lngHop= as.numeric(as.character(lngHop)),
             latHop= as.numeric(as.character(latHop))) %>%
      filter(Hospital==input$hospital2)
  })


  output$Conc1 <- renderDataTable({
    dataHop1.2()

  })

  mytextZone<-reactive({


    if (input$area=="All patients"){
      mytext <- paste(
        "<b>","Ici nous avons toutes communes où recrutent:","</b>","<br/>",
        "<b>",input$hospital1,"</b>") %>%
        lapply(htmltools::HTML)
    }else if(input$area=="Recruitment area"){
      mytext <- paste(
        "<b>","Zone de récrutement de:","</b>","<br/>",
        "<b>",input$hospital1,"</b>","<br/>",
        "<b>","Elle correspond à la zone où l'établissement","<br/>",
        "récrute 80% de ces patients","</b>") %>%
        lapply(htmltools::HTML)
    }else{
      mytext <- paste(
        "<b>","Zone d'attraction de:","</b>","<br/>",
        "<b>",input$hospital1,"</b>","<br/>",
        "<b>","Elle correspond à la zone où l'établissement","<br/>",
        "récrute 40% de ces patients","</b>") %>%
        lapply(htmltools::HTML)
      }


  }

  )



  output$Comp1<-renderLeaflet(




    get_map_Conc(data=dfHop1_Conc1(),
                 dataHop1=dataHop1.1(),
                 dataHop2=dataHop1.2(),
                 Hop=input$hospital1,
                 ZoneCompet=ZoneCompet(),
                 zoom=Zoom(),
                 lng=~lngHop,
                 lat=~latHop,
                 radius=~sqrt(pct)*1000/1.5,
                 mypalette = colorFactor(
                   palette = c("blue","darkgreen","darkred"),
                   domain = c(input$hospital1,input$hospital2,"Autres hopitaux")
                 ),
                 values =c(input$hospital1,input$hospital2,"Autres hopitaux"),
                 mytextZone=mytextZone())
  )




  ##  FLUX PATIENTS HOPITAL2

  ZoneFlow2<-reactive({get_filtered_dataZone(
    data = sejdig,
    severe=input$ssDiag2,
    speciality=input$disease2,
    age=input$age2,
    ZipCode =ZoneDesHop[[input$hospital4]],
    urgency = input$emmergency2)})

  Zone04<-reactive({
    ZoneFlow2()%>%select(codeGeo,Hospital)%>%distinct(codeGeo,.keep_all = T)

  })
  Zone4<-reactive({
    shpCom0%>%right_join(Zone04(),by=c("code_pmsi"="codeGeo"))
  })

  dfHop2_flux<-reactive({ZoneFlow2()%>%select(codeGeo,Hospital,lngHop,latHop)%>%
      group_by(codeGeo,Hospital) %>%add_tally(name = "nb_patient")%>%# nb de patient par commune
      distinct(codeGeo,Hospital,.keep_all = T)%>%
      arrange(codeGeo, desc(nb_patient))%>%group_by(codeGeo)%>%
      mutate(pct = round(nb_patient/sum(nb_patient)*100,1),
             cum_pct = cumsum(pct),
             lngHop= as.numeric(as.character(lngHop)),
             latHop= as.numeric(as.character(latHop))) %>%top_n(n=-2)
  })



  #############################################################################
  ##INDICE HHI HOPITAL 1
  ##################################################"
  HHI_IDF<-reactive({get_filtered_data(
    data = sejourCom,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    urgency = input$emmergency)})



  dfHHI_IDF<-reactive({HHI_IDF()%>%group_by(codeGeo,Hospital)%>%count()%>%
      ungroup()%>%group_by(codeGeo)%>%add_tally(n,name="sejour.com")%>%
      mutate(marche=n/sejour.com)%>%mutate(marcheCarre=marche^2)%>%
      mutate(HHI=sum(marcheCarre))%>%distinct(codeGeo,.keep_all = T)%>%
      mutate(hhiLab=cut(HHI,c(0,0.10,0.15,0.25,1),
                        labels = c("Hyperconcurrence niveau 2: < 0.1 ",
                                   "Hyperconcurrence niveau 1: 0.1-0.15",
                                   "Concurrence moyenne: 0.15-0.25",
                                   "Absence de concurrence: > 0.25")))})

  dfHHI_IDF2<-reactive({shpCom%>%left_join(dfHHI_IDF(),by=c("code_pmsi"="codeGeo"))%>%
      filter(!is.na(Hospital))})

  dfHHI_IDF2.2<-reactive({
    shpCom%>%left_join(dfHHI_IDF(),by=c("code_pmsi"="codeGeo"))%>%
      filter(!is.na(Hospital)&libgeo_cp==input$city)%>%select(-c(1,2,4,5))

    })



  output$sejourCom <- renderLeaflet({


    mybins_com<-if (max(dfHHI_IDF()$sejour.com)>8000){
      c(0,50,100,200,500,1000,2000,5000,10000)
    }else if(max(dfHHI_IDF()$sejour.com)<=8000&max(dfHHI_IDF()$sejour.com)>6000){
      c(0,50,100,200,500,1000,2000,5000,8000)
    }else if(max(dfHHI_IDF()$sejour.com)<=6000&max(dfHHI_IDF()$sejour.com)>4000){
      c(0,50,100,200,500,1000,2000,4000,6000)
    }else if (max(dfHHI_IDF()$sejour.com)<=4000&max(dfHHI_IDF()$sejour.com)>3000){
      c(0,20,50,100,250,500,1000,2000,4000)
    }else if(max(dfHHI_IDF()$sejour.com)<=3000&max(dfHHI_IDF()$sejour.com)>2000){
      c(0,20,50,100,250,500,1000,2000,3000)
    }else if(max(dfHHI_IDF()$sejour.com)<=2000&max(dfHHI_IDF()$sejour.com)>1000){
      c(0,10,20,50,100,250,500,1000,2000)
    }else if(max(dfHHI_IDF()$sejour.com)<=1000&max(dfHHI_IDF()$sejour.com)>500){
      c(0,10,20,30,50,100,250,500,1000)
    }else if(max(dfHHI_IDF()$sejour.com)<=500&max(dfHHI_IDF()$sejour.com)>200){
      c(0,10,20,30,50,100,200,300,500)
    }else if(max(dfHHI_IDF()$sejour.com)<=200&max(dfHHI_IDF()$sejour.com)>100){
      c(0,10,20,30,40,50,100,150,200)
    }else if(max(dfHHI_IDF()$sejour.com)<=100&max(dfHHI_IDF()$sejour.com)>50){
      c(0,5,10,15,20,30,50,75,100)
    }else if(max(dfHHI_IDF()$sejour.com)<=50&max(dfHHI_IDF()$sejour.com)>30){
      c(0,5,10,15,20,30,40,50)
    }else if (max(dfHHI_IDF()$sejour.com)<=30&max(dfHHI_IDF()$sejour.com)>20){
      c(0,5,10,15,20,25,30)
    }else if(max(dfHHI_IDF()$sejour.com)<=20&max(dfHHI_IDF()$sejour.com)>10){
      c(0,5,10,15,20)
    }else{c(0,1,2,5,10)}





    get_map1(data = dfHHI_IDF2(),data2=dfHHI_IDF2.2(),var ="sejour.com", mybins = mybins_com,
             palette =brewer.pal(name="YlOrBr",n=9)[c(2:9)] ,titre_legende="Nombre de séjours/communes",zoom=8)

    #"Hyperconcurrence niveau 2 ", 8
    #"Hyperconcurrence niveau 1", 6
    #"Concurrence moyenne",4
    #"Absence de concurrence" 2
  })


  output$HHI_IDF <- renderLeaflet({
    get_map2(data = dfHHI_IDF2(),data2=dfHHI_IDF2.2(),var ="HHI",var2 = "hhiLab" ,
             mybins = c("Hyperconcurrence niveau 2: < 0.1 ",
                        "Hyperconcurrence niveau 1: 0.1-0.15",
                        "Concurrence moyenne: 0.15-0.25",
                        "Absence de concurrence: > 0.25"),
            palette =brewer.pal(name="Blues",n=9)[c(2,4,6,8)] ,titre_legende="HHI",zoom=8)

    #"Hyperconcurrence niveau 2 ", 8
    #"Hyperconcurrence niveau 1", 6
    #"Concurrence moyenne",4
    #"Absence de concurrence" 2
  })


  ComFlow<-reactive({get_filtered_dataZone(
    data = sejourCom2,
    severe=input$ssDiag,
    speciality=input$disease,
    age=input$age,
    ZipCode = LibCom$codeGeo[LibCom$libgeo_cp==input$city],
    urgency = input$emmergency)})

  dfComFlow<-reactive({ComFlow()%>%select(codeGeo,Hospital,lngHop,latHop)%>%
      group_by(codeGeo,Hospital) %>%add_tally(name = "nb_patient")%>%# nb de patient par commune
      distinct(codeGeo,Hospital,.keep_all = T)%>%
      arrange(codeGeo, desc(nb_patient))%>%group_by(codeGeo)%>%
      mutate(pct = nb_patient/sum(nb_patient)*100,
             cum_pct = cumsum(pct),
             lngHop= as.numeric(as.character(lngHop)),
             latHop= as.numeric(as.character(latHop))) %>%top_n(n=-5)
  })

  dfComFlow2<-reactive({geoComfl%>%
    left_join(dfComFlow(),by="codeGeo")%>%filter(!is.na(nb_patient))%>%
      select(codeGeo,libgeo_cp,Com_lng,Com_lat,
      nb_patient,Hospital,lngHop,latHop,pct)%>%
      arrange(desc(nb_patient))




    })



  output$Commune2<-renderLeaflet({


    flowLine<-st_drop_geometry(dfComFlow2())
    flowLine2 <- gcIntermediate(flowLine[,3:4], flowLine[,c(7,8)],
                                sp = TRUE, addStartEnd = TRUE)
    flowLine2$nbre <- flowLine$nb_patient
    flowLine2$origines <- flowLine$libgeo_cp
    flowLine2$destinations <- flowLine$Hospital
    flowLine2$pct <- round(flowLine$pct,1)

    flowLine2Com<-flowLine2

    Label <- paste0("Vers ",
                    flowLine2Com$destinations, ': ',
                    as.character(flowLine2Com$nbre),"(",flowLine2Com$pct,"%)")



    pal <- colorFactor(palette=brewer.pal(name="YlOrBr",n=9)[c(9,7,5,3,2)],
                       flowLine2$destinations,ordered = TRUE)

    leaflet() %>%
      setView(lng = unique(dfComFlow2()$Com_lng),
              lat =unique(dfComFlow2()$Com_lat),
              zoom = 10)%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = shpDpt,color = "black",fillOpacity = 0,
                  fillColor = "white",stroke=TRUE, weight = 3)%>%

      addMarkers(data=flowLine,lng = ~ lngHop, lat = ~ latHop,
                 popup = ~ paste(Hospital))%>%
      addCircles(data=flowLine,lng = ~ Com_lng, lat = ~ Com_lat, weight = 0,
                 radius = ~1500,
                 popup = ~ paste(libgeo_cp),
                 color = "green", fillOpacity = 0.5)%>%
      addPolylines(data = flowLine2Com, weight = ~ (pct^(1/1.5))*2,
                   label = Label,
                   group = ~origines, color = ~pal(destinations),
                   opacity = 1,
                   stroke = T,
                   fill = TRUE)


  } )

  output$Commune3<-renderDataTable({
    data2=dfHHI_IDF2.2()
    })

}



shinyApp(ui, server)

