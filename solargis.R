## RegioENERGIE Solar-GIS
## Webgis für Veranstaltungen


## Entwickelt durch den Klimaschutzmanager Géza Winzig des Digitalisierungs- und Klimaschutzprojekt RegioENERGIE
## Kontakt: geza.winzig@bietigheim.de oder Tel: 07245 80850 oder www.regioenergie-netzwerk.de

# Pakete installieren (nur bei erstem deploy)
# pkg <- c('leaflet', 'httpuv', 'dplyr','rgdal', 'sp', 'foreign', 'maptools', 'htmlwidgets', 'htmltools','leaflet.extras', 'rvest')
# install.packages(pkg)
library(leaflet)
library(leaflet.extras)
library(metathis)
library(httpuv)
library(dplyr)
library(rgdal)
library(sp)
library(foreign)
library(maptools)
library(htmlwidgets)
library(htmltools)
library(raster)
library(rvest)

# loop durch die ordner
folder <- c("C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Bietigheim_/",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Bischweier_/",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Durmersheim_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Elchesheim-Illingen_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Kuppenheim_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Loffenau_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Malsch2_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Voelkersbach_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Sulzbach_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Waldprechtsweier_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Muggensturm_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Oetigheim_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Steinmauern_",
            "C:/Users/Bietigheim50/Documents/GIS/solargis/data/Ermitteltes_Solarpotenzial_auf_Dachflaechen__Weisenbach_")
# lade Kontaktdaten
setwd("C://Users/Bietigheim50/Documents/GIS/csv/")
ansprechpartner <- read.csv("ansprechpartner.csv", sep = ";", dec = ".", header = TRUE, encoding = "ANSI")

#define responsiveness
# mobile <- meta("<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no' />")

# i<- 1
for (i in 1:length(folder)) {
  print(paste("Run", i, "of", length(folder), "started.", sep=" "))
  setwd(folder[i])
  # load shp
  solarpotenzial <- readOGR(dsn =".", layer="ermitteltes_solarpotenzial_auf_dachflaechen__abfrage_", encoding = "UTF-8", use_iconv = T)
  # solarpotenzial <- readOGR(dsn =".", layer="solarpotenzial_auf_dachflaechen__abfrage_", encoding = "UTF-8", use_iconv = T)
  # lade shape files
  setwd("C://Users/Bietigheim50/Documents/GIS/shp/")
  netzwerk_kommunen <- readOGR(dsn =".", layer="netzwerk_kommunen", encoding = "UTF-8", use_iconv = T)
  
  # lat/lng re-projection
  solarpotenzial <- spTransform(solarpotenzial, CRS("+proj=longlat +datum=WGS84"))
  
  # Berechnung kWp und kWh
  kwp <- solarpotenzial$M_GLICHE_G*300/1000
  kwh <- solarpotenzial$M_GLICHE_G*300/1000*1050
  print("kW Peak und kWh berechnet.")
  
  #Berechnung anfügen
  solarpotenzial$kwp <- kwp
  solarpotenzial$kwh <- kwh
  print("kW Peak und kWh angefügt.")
  # nicht benötigte Spalten droppen
  drop.col <- c("QUALIT_T_D", "STAND_DER_", "HERKUNFT_D")
  solarpotenzial <- solarpotenzial[ , !(names(solarpotenzial) %in% drop.col)]

  # definiere PopUp Inhalt
  content <- paste0(sep = " ",
                    "<h3>", solarpotenzial$GEB_UDENUT," ", solarpotenzial$GEMEINDE,"</h3>","<br>",
                    "<b> Eignungsklasse: </b>", solarpotenzial$EIGNUNGSKL,"<br>",
                    "<b> Mögliche Bedeckung: </b>", solarpotenzial$M_GLICHE_G," ", "Module.","<br>",
                    "<b> Dachform: </b>", solarpotenzial$DACHFORM,"<br>",
                    "<b>Geschätzte Leistung: </b>",solarpotenzial$kwp ," kW Peak","<br>",
                    "<b>Erzeugung: </b>",solarpotenzial$kwh ," kWh","<br>",
                    "<b>Ihr Ansprechpartner: </b>",ansprechpartner$ap_name[[i]], "<br>",
                    "<b>E-Mail Adresse: </b>","<a href='mailto:", ansprechpartner$ap_email[[i]],"?subject=SolarGIS%20RegioENERGIE%20",solarpotenzial$GEMEINDE,"&body=Sehr%20geehrte%20Damen%20und%20Herren,%0D%0A%0D%0Abitte%20lassen%20sie%20mir%20mehr%20Informationen%20zukommen.
                    %0D%0A%0D%0AEs%20handelt%20sich%20um%20folgendes%20Objekt:%0D%0A%0D%0A",
                    solarpotenzial$GEB_UDENUT,"%20",solarpotenzial$GEMEINDE,"%0D%0A%0D%0A",
                    "Eignungsklasse:%20", solarpotenzial$EIGNUNGSKL, "%0D%0A%0D%0A",
                    "Mögliche%20Bedeckung:%20", solarpotenzial$M_GLICHE_G, "%20Module","%0D%0A%0D%0A",
                    "Dachform:%20", solarpotenzial$DACHFORM, "%0D%0A%0D%0A",
                    "Geschätzte%20Leistung:%20", solarpotenzial$kwp,"%20kWp", "%0D%0A%0D%0A",
                    "Erzeugung:%20", solarpotenzial$kwh, "%20kWH","%0D%0A%0D%0A",
                    "Die%20Adresse%20des%20Gebäudes%20lautet:",
                    
                    "'>",ansprechpartner$ap_email[[i]],"</a>", "<br>",
                    "<b>Telefon: </b>",ansprechpartner$ap_phone[[i]], "<br>",
                    "<hr />", 
                    "<i>", ansprechpartner$ap_text[[i]], "</i>",
                    "<hr />",
                    "<i>Bitte beachten Sie dass diese Angaben nicht den tatsächlichen Begebenheiten entsprechen müssen. Wir übernehmen keine Haftung für die Richtigkeit der Daten.</i>"
                    
  )
  # define color palette
  
  # (bedingt, gut, sehr gut, vor Ort)
  pal <- colorFactor(c("#F18F01","#1B512D", "#63C132", "#FBFFFE"), solarpotenzial$EIGNUNGSKL)
  # pal <- colorFactor(c("#FFB400","#00A6ED", "#63C132", "#0D2C54"), solarpotenzial$EIGNUNGSKL)
  
  
  # Test
 
  
  ## leaflet map
  
  m <- leaflet(netzwerk_kommunen) %>%
    setView(lng=8.313327, lat=48.834028, zoom = 12) %>%
    clearBounds() %>%
    addTiles(urlTemplate = "",attribution = paste("Entwickelt durch das Projekt ","<a href='https://regioenergie-netzwerk.de' 
    title='Weiterleitung zur Projektseite RegioENERGIE' target='blank'>RegioENERGIE</a>","| Grundlage: Daten aus dem Umweltinformationssystem (UIS) der LUBW Landesanstalt für Umwelt Baden-Württemberg")) %>%
    addProviderTiles(providers$OpenStreetMap.HOT, group= "HOT") %>%
    
    addPolygons(data= solarpotenzial, group = "Solarpotenzial",
                popup = content,
                color = ~pal(EIGNUNGSKL)
                ) %>%
    
    addLayersControl(
      baseGroups = c("HOT"),
      overlayGroups = c("Solarpotenzial"),
      options = layersControlOptions(collapsed = T)
    ) %>%
  # Geocoder hinzufügen
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)   
              ) %>%
  # Legende hinzufügen
    addLegend("bottomright", pal = pal, values = solarpotenzial$EIGNUNGSKL,
              title = paste("Eignungsklasse der Dachfläche für Photovoltaik in", as.character(solarpotenzial$GEMEINDE[1]), sep = " ")
              )
  # responsive machen
    # meta() %>%
      # write_meta("<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no' />")
      
  
  # m
  # Webkarte in ein html-file exportieren
  setwd("C://Users/Bietigheim50/Documents/GIS/leaflet_export/")
  
  # saveWidget(m, file = "solargis.html", title="SolarGIS RegioENERGIE")
  saveWidget(m, file = paste("solargis","_", as.character(solarpotenzial$GEMEINDE[1]),".html",
                             sep = ""), 
             title="SolarGIS RegioENERGIE",
             selfcontained = T)
                               sep = ""))
  print(paste("Karte von",solarpotenzial$GEMEINDE[1], "erfolgreich exportiert.", sep = " "))
}
print("Komplett abgeschlossen.")



















              
