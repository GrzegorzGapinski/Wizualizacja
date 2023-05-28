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

getwd()
theme_set(theme_bw()) # Theme appropriate for maps

powiaty <- st_as_sf(readOGR(dsn=file.path(getwd(),"powiaty")))
entire_data <- read.csv("dataset.csv")

powiaty <- transform(powiaty, powiat_name = sapply(strsplit(x=powiaty$JPT_NAZWA_, split = ' '), function(x) paste(x[-1], collapse = ' ')))  
##my god why string concatenation is so hard in R xD
data_powiaty_agg <- entire_data %>% group_by(Powiat) %>% summarise(pis=sum(Prawo.i.Sprawiedliwość),
                                       po = sum(Koalicja.Europejska),
                                       .groups='drop') %>%  as.data.frame()

powiaty_agg <- merge(x=data_powiaty_agg, y=powiaty[c('powiat_name', 'geometry')], by.x="Powiat", by.y="powiat_name", all.x=TRUE)
powiaty_agg <- transform(powiaty_agg, zwyciezca =  ifelse(pis > po, TRUE, FALSE)) #binary fill 


ggplot(data = powiaty_agg, aes(geometry = geometry)) + 
  geom_sf(aes(fill = zwyciezca)) +
  scale_fill_discrete(name='Zwycięzca w okręgu',
                      labels=c('Koalicja Europejska', 'Prawo i Sprawiedliwość')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

wojewodztwa <- st_as_sf(readOGR(dsn=file.path(getwd(),"wojewodztwa")))

data_wojewodztwa_agg <- entire_data %>% group_by(Województwo) %>%
  summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
            uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
            .groups='drop') %>% as.data.frame()

wojewodztwa <- cbind(wojewodztwa, st_coordinates(st_centroid(wojewodztwa$geometry)))
wojewodztwa_agg <- merge(x=data_wojewodztwa_agg, y=wojewodztwa[c('JPT_NAZWA_', 'geometry','X','Y')], by.x="Województwo", by.y="JPT_NAZWA_")
wojewodztwa_agg <- transform(wojewodztwa_agg, frekwencja=round(oddane_glosy/uprawnieni_do_glosowania, digits = 3)*100)

############napisy##############
wojewodztwa_agg <- transform(wojewodztwa_agg, Y_woj=Y-20000)


ggplot(data = wojewodztwa_agg, aes(geometry = geometry)) +
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
  
  
  
