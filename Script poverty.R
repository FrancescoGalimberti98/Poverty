library(readxl)
library(ggplot2)
library(tidyverse)
library(ggfittext)
library(treemapify)
library(viridis)
library(showtext)
library(geojsonio)
library(broom)
library(dplyr)
library(hrbrthemes)
library(grid)
library(ggtext)
library(ggrepel)

font_add_google('Montserrat', family = 'Montserrat')
showtext_auto()

data <- read_excel("data poverty.xlsx")
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

# SCEGLIERE QUALI STATI TOGLIERE DALLA MAPPA (MAGARI RUSSIA E ISLANDA)

# state_name = data$stato 
# state_name[4] = 'Czech Republic'
# spdf <- geojson_read("https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson",  what = "sp")
# index <- spdf$NAME %in% state_name
# sum(index) # 34 - UE


# index_sbagliato = state_name %in% spdf$NAME  # Per controllare quali stati ho escluso


# spdf_fortified <- tidy(spdf[index,], region = "NAME")
spdf_fortified <- tidy(spdf, region = "NAME")

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()


spdf_fortified = spdf_fortified %>%
  left_join(. , data, by=c("id"="stato"))

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

# Serve dataset con 2 info, uso dataset giovani_VS_vecchi

data2 <- read_excel("Poverty giovani_vecchi.xlsx")

ggplot(data2, aes(y=Vecchi, x = Giovani, color = stato)) +
  geom_point(size=3,show.legend = FALSE) + 
  # ylim(66.5,85) +
  geom_text_repel(aes(label = stato),show.legend = F,
                  family = 'Montserrat',fontface = "bold",size = 3.8, min.segment.length = 3,point.padding = unit(1,"lines")) +
  labs(y = "Vecchi", x = "Giovani",family = 'Montserrat',fontface = "bold") +
  scale_color_viridis(begin = 0.15,end = 0.65,direction = 1,discrete = TRUE, option = "F") +
  theme_minimal(base_size=15, base_family = 'Montserrat') +
  theme(axis.line = element_line(color='grey85'), panel.grid.minor = element_blank(),
        axis.title=element_text(face="bold"), legend.text = element_text(face="bold"),
        axis.text = element_text(face="bold")) +
  geom_hline(yintercept = mean(data2$Vecchi), col = 'grey40',lty = 2,lwd = 1) +
  geom_vline(xintercept = mean(data2$Giovani), col = 'grey40', lwd = 1, lty = 2)

#### BARPLOT ####

# NON FUNZIONA

# index = order(data$poverty)
# ord.data = data[index,]

# riordino così

data$stato <- factor(data$stato,levels = data$stato[order(data$poverty, decreasing = TRUE)])
data1 = data

p = ggplot(data = data1, aes(x = poverty, y = stato, fill = poverty)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(y = "Stato", x = "Poverty",family = 'Montserrat',fontface = "bold") +
  scale_fill_viridis(begin = 0.1,end = 0.9,direction = -1,discrete = FALSE, option = "F") +
  theme_minimal(base_size=15, base_family = 'Montserrat') +
  theme(panel.grid = element_blank(),panel.grid.major.x =element_line(color='grey85'),axis.title=element_text(face="bold"), 
        axis.text = element_text(face="bold"))

p

gdp_pc <- read_excel("GDP pro capite.xlsx")
gdp_pc = as.data.frame(gdp_pc)
stati_ricchi = gdp_pc[1:15,1]

indice = data$stato %in% stati_ricchi
# ind.err = stati_ricchi %in% data$stato # non ho liechtenstein e islanda
sum(indice)

q = ggplot(data = data[indice,], aes(x = poverty, y = stato, fill = poverty)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(y = "Stato", x = "Poverty",family = 'Montserrat',fontface = "bold") +
  scale_fill_viridis(begin = 0.1,end = 0.9,direction = -1,discrete = FALSE, option = "F") +
  theme_minimal(base_size=15, base_family = 'Montserrat') +
  theme(panel.grid = element_blank(),panel.grid.major.x =element_line(color='grey85'),axis.title=element_text(face="bold"), 
        axis.text = element_text(face="bold"))

q


#### ALTRI GRAFICI ??? ####
