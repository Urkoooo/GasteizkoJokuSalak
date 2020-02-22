# Helburua: gasteizko joku salak eta jokatzaileen bizigunearen artean erlazioa dagoen ezagutzea. 
# Urko Alonso Azkoaga 2020 CC-BY-NC.


# Beharrezko baliabideak:
  # Liburutegiak

library(ggmap)
library(data.table)
library(OpenStreetMap)
library(RColorBrewer)
library(raster)  
library(spatstat)
library(tidyverse)
library(geojsonio)
library(readxl)
library(sp)
library(rgeos)
library(sf)
library(maptools)
library(spatstat)
library(RColorBrewer)
library(tigris)
library(GISTools)
library(RColorBrewer)
library(ggalt)
library(ggthemes)
library(tmap)
library(plyr)


  # Datuak: 
    # Joku salen helbideak (Eusko Jaurlaritza, egile-eskubideak)
    # Jokalarien helbideak (COTA)
    # Gasteizko auzo bakoitzeko inmigrante kopurua (GeoEuskadi, egile-eskubideak)
    # Gabezia sozioekonomikoa (GeoEuskadi, egile-eskubideak)
    # Bizi itxaropena (GeoEuskadi, egile-eskubideak)
    # Hilkortasuna (GeoEuskadi, egile-eskubideak)

 # Mapak: 
    # Gasteizko auzoen eta sekzio zenzaleen mapak (Gasteizko Udaletxea, egile-eskubideak)
    # Google Maps (Egile-eskubideak)


# AURKIBIDEA
  # A- MAPAK SORTU
  # B- DATUAK SORTU
  # C- EGOERA ESPAZIALA AZTERTU
  # D- MAPAK SORTU

  
# A- MAPAK SORTU

# A1: Google-eko mapak
# Google-en API kodea definitu
register_google(key = "KODEA")

# Google-tik Gasteizko Mapak lortu
kokapena=c(lon= -2.6724680, lat= 42.849017)

KoloreGasteiz<- get_map(location=kokapena, zoom= 15, maptype = c("roadmap"))
ggmap(KoloreGasteiz)

KoloreGasteiz2 <- get_map(location=kokapena, zoom= 15, maptype = c("roadmap") )
ggmap(KoloreGasteiz2)

TBGasteiz <- get_map(location=kokapena, zoom= 13, source="stamen", maptype="toner", crop=TRUE)
ggmap(TBGasteiz)

TBGasteiz2 <- get_map(location=kokapena, zoom= 15, source="stamen", maptype="toner", crop=TRUE)
ggmap(TBGasteiz2)



#A2: Gasteizko Udaletxeak utzitako mapak importatu
# A2.1 Gasteizko auzoen mapa kargatu
Auzoak <- geojson_read("/Volumes/A/Trabajo fin de Residencia/Datuak/Gasteizko joku salak/Kartografia/auzoak.geojson",  what = "sp"  )

#Gasteizko auzoen mapa ikusi.
class(Auzoak)
plot(Auzoak)

# A2.2 Gasteizko sekzioen mapa kargatu
Sekzioak <- geojson_read("/Volumes/A/Trabajo fin de Residencia/Datuak/Gasteizko joku salak/Kartografia/sekzioak.geojson",  what = "sp"  )

#Gasteizko sekzioen mapa ikusi.
class(Sekzioak)
plot(Sekzioak)




# B- HELBIDEAK DATU BIHURTU
# B1: Joku salen helbideak geolokalizatu

# Joku salen helbideak importatu
SalenHelbidea <- read_excel("/Volumes/A/Trabajo fin de Residencia/Datuak/Gasteizko joku salak/Helbideak/Joku salak.xlsx")
NROW(SalenHelbidea)
View(SalenHelbidea)

# Helbideak geolokalizatu
jokuSalak <- geocode(location = SalenHelbidea$Nombre, output="latlon", source="google")
NROW(jokuSalak)
View(jokuSalak)


# B1: jokatzaileen helbideak geolokalizatu

# Jokatzaileen helbideak importatu
JokatzaileenHelbidea <- read_excel("")
NROW(JokatzaileenHelbidea)
View(JokatzaileenHelbidea)

# Helbideak geolokalizatu
jokatzaileak <- geocode(location = JokatzaileenHelbidea$Nombre, output="latlon", source="google")
NROW(jokatzaileak)
View(jokatzaileak)


# C- EGOERA ESPAZIALA AZTERTU
# Ripley K-ren bitartez datuen distribuzioa aztertu

# C1- Joku salen kasua:

p.sf1 <- st_as_sf(jokuSalak, coords = c("lon", "lat"), crs = 4326) 
p.sf1 

s.sp1 <- as(p.sf1, "Spatial")
class(s.sp1)

s.owin1 <- as(s.sp1, "owin")

p.sp1  <- as(p.sf1, "Spatial")  # Objetu espaziala sortu
p.ppp1 <- as(p.sp1, "ppp")      # ppp objetua sortu
class(p.ppp1)

# Testa egin
Joku_salak_K <- Kest(p.ppp1)
print(Joku_salak_K)

# Testa imprimatu
plot(Joku_salak_K, main = "Ripley-ren K - Joku salak")


# C2- Jokalarien kasua:

p.sf2 <- st_as_sf(jokatzaileak, coords = c("lon", "lat"), crs = 4326) 
p.sf2 

s.sp2 <- as(p.sf2, "Spatial")
class(s.sp2)

s.owin2 <- as(s.sp2, "owin")

p.sp2  <- as(p.sf2, "Spatial")  # Objetu espaziala sortu
p.ppp2 <- as(p.sp2, "ppp")      # ppp objetua sortu
class(p.ppp2)

# Testa egin
jokatzaileak_K<- Kest(p.ppp2)
print(jokatzaileak_K)

# Testa imprimatu
plot(jokatzaileak_K, main = "Ripley-ren K jokalariak")


# D- MAPAK SORTU


# D1- Gasteizko joku salak mapetan ezarri:

# Joku salak gasteizko mapan irudikatu.

ggmap(KoloreGasteiz) + geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  ggtitle("Gasteizko joku salak")

ggmap(KoloreGasteiz2) + geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  ggtitle("Gasteizko joku salak")

ggmap(TBGasteiz) + geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  ggtitle("Gasteizko joku salak")

ggmap(TBGasteiz2) + geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  ggtitle("Gasteizko joku salak") 

ggmap(TBGasteiz2) + geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  ggtitle("Gasteizko joku salak") +
  geom_polygon(data = AuzoakFF, aes( x = long, y = lat, group = group) ,color="black")

# Joku salen bero mapa

ggmap(KoloreGasteiz) + stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha=..level..), bins = 5, geom = "polygon", data = jokuSalak) + 
  scale_fill_gradient(low = "black", high = "red") +
  ggtitle("Joko salen mapa")

ggmap(TBGasteiz2) + stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha=..level..), bins = 5, geom = "polygon", data = jokuSalak) + 
  scale_fill_gradient(low = "black", high = "red") +
  ggtitle("Joko salen mapa")


ggmap(TBGasteiz2) + stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha=..level..), bins = 4, geom = "polygon", data = jokuSalak) +
  geom_point(data = jokuSalak, aes(x =lon, y =lat)) +
  scale_fill_gradient(low = "black", high = "red") + 
  ggtitle("Joko salen mapa")


#Mapa baterautuak
ggmap(KoloreGasteiz) +geom_polygon(data = AuzoakFF, aes( x = long, y = lat, group = group) ,alpha=0.5,color="black") +
  geom_point(data = jokuSalak, aes(x =lon, y =lat), color="red" )

ggmap(KoloreGasteiz) +geom_polygon(data = SekzioakFF, aes( x = long, y = lat, group = group) ,alpha=0.5,color="black") +
  geom_point(data = jokuSalak, aes(x =lon, y =lat), color="red" )



ggmap(KoloreGasteiz2) + geom_point(data = jokuSalak, aes(x =lon, y =lat) )+
  ggtitle("Mapa de Salas de juego en Vitoria- Gasteiz")



# D2- Gasteizko joku salak auzo eta sekzio mapetan ezarri:
#Gasteizkomapa ggmap irakurri dezakeen formatu batera bihurtu (Auzoak)
AuzoakFF <- fortify(Auzoak)
plot(AuzoakFF)


ggplot() + 
  geom_polygon(data = AuzoakFF, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_point(data = jokuSalak, aes(x =lon, y =lat) ) +
  ggtitle("Gasteizko joku espazioen kokagunea, auzoka")



#Gasteizkomapa ggmap irakurri dezakeen formatu batera bihurtu (Sekzioak)
SekzioakFF<- fortify(Sekzioak)
plot(SekzioakFF)


ggplot() + 
  geom_polygon(data = SekzioakFF, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  geom_point(data = jokuSalak, aes(x =lon, y =lat) ) +
  ggtitle("Gasteizko joku espazioen kokagunea, sekzioka")



# D2- Puntuak kontatu


# D2.1- Joku salak

# Honetarako lehenengo "jokuSalak"  data frame  bihurtu behar dugu.
# 3 elementuak prestatu; koordenadak, datuak, and proj4string (proiekzio geografikoa)
coords <- jokuSalak[ , c("lon", "lat")]   # koordenadak
data   <- jokuSalak      # data
myCRS=CRS("+proj=longlat +datum=WGS84 +no_defs") # proiekzoa

# data frame a bihurut puntu espazialak
jokuSalakGSPT<- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = myCRS)

#Klaseak komprobatu 
class(Auzoak)
class(Sekzioak)
class(jokuSalakGSPT)

# Bi planoen proiekzioa konprobatu
proj4string(Auzoak)
proj4string(Sekzioak)
proj4string(jokuSalakGSPT)

# Bi planoak aldi berean marraztu (Auzoak)
plot(jokuSalakGSPT)
plot(Auzoak, add=TRUE)

# Bi planoak aldi berean marraztu (Sekzioak)
plot(jokuSalakGSPT)
plot(Sekzioak, add=TRUE)



# Auzo bakoitzean zenbat joku salak dauden kontatu:

zenbakia_auzoka<- poly.counts(jokuSalakGSPT, Auzoak)
zenbakia_auzoka <- stack(zenbakia_auzoka)
view(zenbakia_auzoka)

zenbakia_sekzioka <- poly.counts(jokuSalakGSPT, Sekzioak)
zenbakia_sekzioka<- stack(zenbakia_sekzioka)
view(zenbakia_sekzioka)


# D2.2- Jokakalariak

# Honetarako lehenengo "jokalariak"  data frame  bihurtu behar dugu.
# 3 elementuak prestatu; koordenadak, datuak, and proj4string (proiekzio geografikoa)
coords2 <- jokatzaileak[ , c("lon", "lat")]   # koordenadak
data2   <- jokatzaileak     # data
myCRS=CRS("+proj=longlat +datum=WGS84 +no_defs") # proiekzoa

# data frame a bihurut puntu espazialak
jokatzaileakGSPT<- SpatialPointsDataFrame(coords = coords2,
                                       data = data2, 
                                       proj4string = myCRS)

#Klaseak komprobatu 
class(Auzoak)
class(Sekzioak)
class(jokatzaileakGSPT)

# Bi planoen proiekzioa konprobatu
proj4string(Auzoak)
proj4string(Sekzioak)
proj4string(jokatzaileakGSPT)

# Bi planoak aldi berean marraztu (Auzoak)
plot(jokatzaileakGSPT)
plot(Auzoak, add=TRUE)

# Bi planoak aldi berean marraztu (Sekzioak)
plot(jokatzaileakGSPT)
plot(Sekzioak, add=TRUE)



# Auzo bakoitzean zenbat joku salak dauden kontatu:

zenbakia_auzokaJ <- poly.counts(jokatzaileakSPT, Auzoak)
zenbakia_auzokaJ <- stack(zenbakia_auzokaJ)
view(zenbakia_auzokaJ)

zenbakia_sekziokaJ <- poly.counts(jokatzaileakGSPT, Sekzioak)
zenbakia_sekziokaJ<- stack(zenbakia_sekziokaJ)
view(zenbakia_sekziokaJ)




#Mapak margotu, mapa klopletikoa, biztanleria ez dago gehituta

# Auzoka (Joku salak)

ggplot() +
  geom_cartogram(data = AuzoakFF, aes(x = long, y = lat, map_id = id), 
                 map = AuzoakFF) +
  geom_cartogram(data = zenbakia_auzoka, aes(fill = values, map_id = ind),
                 map = AuzoakFF, color = "black", size = 0.3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()



# Sekzio bakoitzeko
# TODO: poligonoak konpondu (Joku salak)

ggplot() +
  geom_cartogram(data = SekzioakFF, aes(x = long, y = lat, map_id = id), 
                 map = SekzioakFF) +
  geom_cartogram(data = zenbakia_sekzioka, aes(fill = values, map_id = ind),
                 map = SekzioakFF, color = "black", size = 0.3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()


# Auzoka (Jokatzaileak)

ggplot() +
  geom_cartogram(data = AuzoakFF, aes(x = long, y = lat, map_id = id), 
                 map = AuzoakFF) +
  geom_cartogram(data = zenbakia_auzokaJ, aes(fill = values, map_id = ind),
                 map = AuzoakFF, color = "black", size = 0.3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()



# Sekzio bakoitzeko
# TODO: poligonoak konpondu (Jokatzaileak

ggplot() +
  geom_cartogram(data = SekzioakFF, aes(x = long, y = lat, map_id = id), 
                 map = SekzioakFF) +
  geom_cartogram(data = zenbakia_sekziokaJ, aes(fill = values, map_id = ind),
                 map = SekzioakFF, color = "black", size = 0.3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()




zenbakia_auzoka <- rename(zenbakia_auzoka,c("ind" = "BARRIO", "values" = "joku_salak"))

Auzoak2 <- merge(Auzoak, zenbakia_auzoka, by="BARRIO")


tm_shape(Auzoak2) +
  tm_borders() +
  tm_fill(col = "joku_salak")


qtm(Auzoak)
