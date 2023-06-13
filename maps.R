library(ggplot2)
library(sf) # Spatial data format
library(rnaturalearth) # Provides a map of countries of the entire world
library(ggspatial) # Scale bar and North arrow
library(maps) # USA
library(googleway) # Access to Google Maps APIs,
library(cowplot) # Grid plots
library(ggmap) # Google maps
library(rgdal)
library(dplyr)
library(plotly)
library(shiny)
library(leaflet)

getwd()
theme_set(theme_bw()) # Theme appropriate for maps

powiaty <- st_as_sf(readOGR(dsn=file.path(getwd(),"powiaty")))
entire_data <- read.csv("dataset.csv", encoding = "UTF-8")

powiaty <- transform(powiaty, powiat_name = sapply(strsplit(x=powiaty$JPT_NAZWA_, split = ' '), function(x) paste(x[-1], collapse = ' ')))  

data_powiaty_agg <- entire_data %>% group_by(Powiat) %>% summarise(pis=sum(Prawo.i.Sprawiedliwość),
                                                                   po = sum(Koalicja.Europejska),
                                                                   .groups='drop') %>%  as.data.frame()

powiaty_agg <- merge(x=data_powiaty_agg, y=powiaty[c('powiat_name', 'geometry')], by.x="Powiat", by.y="powiat_name", all.x=TRUE)
powiaty_agg <- transform(powiaty_agg, zwyciezca =  ifelse(pis > po, TRUE, FALSE)) #binary fill 

powiaty_shiny <- entire_data %>% group_by(Powiat) %>% summarise(pis=sum(Prawo.i.Sprawiedliwość)/sum(Liczba.kart.wyjętych.z.urny)*100,
                      po = sum(Koalicja.Europejska)/sum(Liczba.kart.wyjętych.z.urny)*100,
                      konf = sum(Konfederacja)/sum(Liczba.kart.wyjętych.z.urny)*100,
                      wiosna = sum(Wiosna)/sum(Liczba.kart.wyjętych.z.urny)*100,
                      lew = sum(Lewica.Razem)/sum(Liczba.kart.wyjętych.z.urny)*100,
                      kukiz = sum(Kukiz15)/sum(Liczba.kart.wyjętych.z.urny)*100, #in percents
                      .groups='drop') %>% as.data.frame() %>%
                      merge(x=., y=powiaty[c('powiat_name', 'geometry')], by.x="Powiat", by.y="powiat_name", all.x=TRUE)


wojewodztwa <- st_as_sf(readOGR(dsn=file.path(getwd(),"wojewodztwa")))
wojewodztwa <- cbind(wojewodztwa, st_coordinates(st_centroid(wojewodztwa$geometry)))

data_wojewodztwa_agg <- entire_data %>% group_by(Województwo) %>%
  summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
            uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
            .groups='drop') %>% as.data.frame()

wojewodztwa_agg <- merge(x=data_wojewodztwa_agg, y=wojewodztwa[c('JPT_NAZWA_', 'geometry','X','Y')], by.x="Województwo", by.y="JPT_NAZWA_")
wojewodztwa_agg <- transform(wojewodztwa_agg, frekwencja=round(oddane_glosy/uprawnieni_do_glosowania, digits = 3)*100)


wojewodztwa_agg <- transform(wojewodztwa_agg, Y_woj=Y-20000)

############do tego momentu trzeba załadować, żeby działały wykresy z maps_app.R



mapa_partia <- ggplot(data = powiaty_shiny, aes(geometry = geometry, fill = po)) + 
  geom_sf() +
  scale_fill_viridis_c(option = "G", alpha = .6, direction = -1) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

mapa_partia

mapa_interaktywna <- ggplot(data = powiaty_agg, aes(geometry = geometry)) + 
  geom_sf()+
  geom_sf(data = powiaty_agg, aes(fill = zwyciezca)) +
  scale_fill_discrete(name='Zwycięzca w okręgu',
                      labels=c('Koalicja Europejska', 'Prawo i Sprawiedliwość')) +
  geom_sf(data = wojewodztwa, fill = NA, color = "black", linetype = "solid", size = 10.0) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

powiaty_agg_sf <- st_as_sf(powiaty_agg)
powiaty_agg_sf <- st_transform(powiaty_agg_sf, crs = st_crs("+proj=longlat +datum=WGS84"))

wojewodztwa_agg_sf <- st_as_sf(wojewodztwa_agg)
wojewodztwa_agg_sf <- st_transform(wojewodztwa_agg_sf, crs = st_crs("+proj=longlat +datum=WGS84"))

mymap <- leaflet() %>%
  setView(lng = 52.25, lat = 19.25, zoom = 2.0) %>%
  addPolygons(data = powiaty_agg_sf, fillColor = ~ifelse(zwyciezca, "blue", "red"), color = 'black', weight = 2)

powiaty_agg_sf$name <- powiaty_agg_sf$Powiat

legend_colors <- c("blue", "red")  # Define colors for the legend
legend_labels <- c("PiS", "KE")  # Define labels for the legend
line_thickness = 5

mymap <- mymap %>% addPolygons(data = wojewodztwa_agg_sf, 
                               color = "black", 
                               weight = line_thickness)  # Second layer with black lines

mymap <- mymap %>% addPolygons(data = powiaty_agg_sf, 
                               fillColor = ~ifelse(zwyciezca, "blue", "red"),
                               color = 'black',
                               weight = 2,
                               popup = ~name) %>%
  addLegend("bottomright", colors = legend_colors, labels = legend_labels)

mymap



mapa_interaktywna <- ggplotly(mapa_interaktywna) %>%
  add_markers(data = powiaty_agg, x = ~geometry$x, y = ~geometry$y, hovertext = ~powiaty_agg$Powiat)

mapa_1 <- ggplotly(mapa_interaktywna)

mapa_interaktywna


frekwencja_ogolna <- ggplot(data = wojewodztwa_agg, aes(geometry = geometry)) +
  geom_sf(aes(fill = frekwencja)) +
  geom_text(aes(x=X, y=Y, label=paste(frekwencja, "%", sep='')), position = position_dodge(0.5), size=4.2) +
  geom_text(aes(x=X, y=Y_woj, label=Województwo), position= position_dodge(0.9), size=2.5) +
  scale_fill_viridis_c(option = "D",trans = "sqrt", alpha = .4) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

data_wojewodztwa_agg_village <- entire_data %>% filter(Typ.obszaru == 'wieś') %>% group_by(Województwo) %>%
  summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
            uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
            .groups='drop') %>% as.data.frame()  

data_wojewodztwa_agg_village
data_wojewodztwa_agg

wojewodztwa_agg_village <- merge(x=data_wojewodztwa_agg_village, y=wojewodztwa[c('JPT_NAZWA_', 'geometry','X','Y')], by.x="Województwo", by.y="JPT_NAZWA_")
wojewodztwa_agg_village <- transform(wojewodztwa_agg_village, frekwencja=round(oddane_glosy/uprawnieni_do_glosowania, digits = 3)*100)

wojewodztwa_agg_village <- transform(wojewodztwa_agg_village, Y_woj=Y-20000)

frekwencja_wies <- ggplot(data = wojewodztwa_agg_village, aes(geometry = geometry)) +
  geom_sf(aes(fill = frekwencja)) +
  geom_text(aes(x=X, y=Y, label=paste(frekwencja, "%", sep='')), position = position_dodge(0.5), size=4.2) +
  geom_text(aes(x=X, y=Y_woj, label=Województwo), position= position_dodge(0.9), size=2.5) +
  scale_fill_viridis_c(option = "D",trans = "sqrt", alpha = .4) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

data_wojewodztwa_agg_city <- entire_data %>% filter(Typ.obszaru == 'miasto') %>% group_by(Województwo) %>%
  summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
            uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
            .groups='drop') %>% as.data.frame()  

data_wojewodztwa_agg_city
data_wojewodztwa_agg

wojewodztwa_agg_city <- merge(x=data_wojewodztwa_agg_city, y=wojewodztwa[c('JPT_NAZWA_', 'geometry','X','Y')], by.x="Województwo", by.y="JPT_NAZWA_")
wojewodztwa_agg_city <- transform(wojewodztwa_agg_city, frekwencja=round(oddane_glosy/uprawnieni_do_glosowania, digits = 3)*100)

wojewodztwa_agg_city <- transform(wojewodztwa_agg_city, Y_woj=Y-20000)

frekwencja_miasto <- ggplot(data = wojewodztwa_agg_city, aes(geometry = geometry)) +
  geom_sf(aes(fill = frekwencja)) +
  geom_text(aes(x=X, y=Y, label=paste(frekwencja, "%", sep='')), position = position_dodge(0.5), size=4.2) +
  geom_text(aes(x=X, y=Y_woj, label=Województwo), position= position_dodge(0.9), size=2.5) +
  scale_fill_viridis_c(option = "D",trans = "sqrt", alpha = .4) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

entire_data$Typ
input <- "razem"
entire_data %>%
  {if(input != "razem") filter(., Typ.obszaru == input) else .} %>%
  group_by(Województwo) %>%
  summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
            uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
            .groups='drop') %>%
  as.data.frame() 
