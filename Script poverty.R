library(readxl)
extrafont::loadfonts("win")
library(ggplot2)

data <- read_excel("data poverty.xlsx")

library(tidyverse)
library(ggfittext)
library(treemapify)
library(viridis)
library(showtext)

data[order(data$Plastica)[1:5],]$Stato = ''

p = ggplot(data, aes(area = Plastica, fill = Plastica, label = Stato)) +
  geom_treemap() +
  scale_fill_viridis(begin=0.7, end =0, option = "D", name="") +
  theme(legend.position = 'none')
#,guide= guide_colourbar(label = F, direction = "vertical", barwidth=7, ticks=F) )
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
