library(readxl)
extrafont::loadfonts("win")
library(ggplot2)

data <- read_excel("data poverty.xlsx")

library(tidyverse)
library(ggfittext)
library(treemapify)
library(viridis)
library(showtext)
library(geojsonio)
library(broom)
library(dplyr)

data = as.data.frame(data)
str(data)
colnames(data) = c('stato', 'poverty')


#### PLOT RETTANGOLI ####

p = ggplot(data, aes(area = poverty, fill = poverty, label = stato)) +
  geom_treemap() +
  scale_fill_viridis(begin=0.7, end =0, option = "D", name="") +
  theme(legend.position = 'none')
p

font_add_google('Montserrat', family = 'Montserrat')
showtext_auto()

p + geom_treemap_text(
  family = 'Montserrat',
  fontface = "bold",
  colour = "white",
  place = "centre",
  min.size = 4,
  grow = FALSE,
  reflow = TRUE
)

#### MAPPA ####

state_name = data$stato 
state_name[4] = 'Czech Republic'
spdf <- geojson_read("https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson",  what = "sp")
index <- spdf$NAME %in% state_name
sum(index) # 34 - UE
# index_sbagliato = state_name %in% spdf$NAME  # Per controllare quali stati ho escluso

spdf_fortified <- tidy(spdf[index,], region = "NAME")


ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()


spdf_fortified = spdf_fortified %>%
  left_join(. , data, by=c("id"="stato"))


library(viridis)


q <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = poverty, x = long, y = lat, group = group)) +
  theme_void() +
  scale_fill_viridis(na.value="grey85", begin= 0.1, end = 0.9, option = "F", name="",guide= guide_colourbar(label = F, direction = "horizontal", barwidth=5, ticks=F) ) +
  theme(  text = element_text(color = "#22211d"),
    legend.position = c(0.2,0.73)
  ) +
  coord_map()
q

#### SCATTERPLOT ####
