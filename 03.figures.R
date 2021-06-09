# Mon May 31 20:13:50 2021 ------------------------------
#Maps for book chapter wef-Brasil

source("02.data_analysis.R")

#library####
library(sf)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(geobr)
library(ggpubr)

## data####
st_as_sf(map_caat)->sf.map_caat
uf<-read_state()
uf %>% 
  filter(abbrev_state %in% c("MG","BA","SE","PE", "AL","PB","RN","CE","PI"))->uf_caat

#maps####
##nvc_2018####
ggplot() +
  geom_sf(data = sf.map_caat$geometry,
          aes(fill = sf.map_caat$nvcPerc_18),
          lwd = 0) +
  scale_fill_distiller(
    type = "seq",
    palette = "Greens",
    direction = 1,
    name = "Native vegetation\ncover (%)",
    na.value = "Grey70"
  ) +
  geom_sf(
    data = uf_caat,
    fill = "transparent",
    color = "black",
    lwd = 0.2
  ) +
  coord_sf(xlim = c(-48,-34), ylim = c(-17.1,-3)) +
  geom_text(data = uf_caat, aes(
    x = c(-42,-39.5, -36.5, -35.5,-34.5,-34.4,-36,-39, -42.4),
    y = c(-16.8,-15,-11,-10,-8.5,-7,-4.7,-2.9,-5),
    label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")
  ),
  size = 2.5) +
  theme(
    text = element_text(family = '', size = 6),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.position = c(0.95, 0.2),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) -> nvc.caat_18

##cattle 2018####
ggplot() +
  geom_sf(data = sf.map_caat$geometry,
          aes(fill = sf.map_caat$bovMed_18),
          lwd = 0) +
  scale_fill_distiller(
    type = "seq",
    palette = "Reds",
    direction = 1,
    name = "Mean cattle\nherd",
    na.value = "Grey70"
  ) +
  geom_sf(
    data = uf_caat,
    fill = "transparent",
    color = "black",
    lwd = 0.2
  ) +
  coord_sf(xlim = c(-48,-34), ylim = c(-17.1,-3)) +
  geom_text(data = uf_caat, aes(
    x = c(-42,-39.5, -36.5, -35.5,-34.5,-34.4,-36,-39, -42.4),
    y = c(-16.8,-15,-11,-10,-8.5,-7,-4.7,-2.9,-5),
    label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")
  ),
  size = 2.5) +
  theme(
    text = element_text(family = '', size = 6),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.position = c(0.9, 0.2),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) -> bov.caat_18

##goat 2018####
ggplot() +
  geom_sf(data = sf.map_caat$geometry,
          aes(fill = sf.map_caat$capMed_18),
          lwd = 0) +
  scale_fill_distiller(
    type = "seq",
    palette = "Reds",
    direction = 1,
    name = "Mean goat\nherd",
  ) +
  geom_sf(
    data = uf_caat,
    fill = "transparent",
    color = "black",
    lwd = 0.2
  ) +
  coord_sf(xlim = c(-48,-34), ylim = c(-17.1,-3)) +
  geom_text(data = uf_caat, aes(
    x = c(-42,-39.5, -36.5, -35.5,-34.5,-34.4,-36,-39, -42.4),
    y = c(-16.8,-15,-11,-10,-8.5,-7,-4.7,-2.9,-5),
    label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")
  ),
  size = 2.5) +
  theme(
    text = element_text(family = '', size = 6),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.position = c(0.88, 0.2),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, vjust = 1.5)
  )-> cap.caat_18

## Drought exposure ####
ggplot() +
  geom_sf(data = sf.map_caat$geometry,
          aes(fill = sf.map_caat$drought_exposure_com),
          lwd = 0) +
  scale_fill_distiller(
    type = "seq",
    palette = "Blues",
    direction = -1,
    name = "Drought\nexposure",
    na.value = "Grey70"
  ) +
  geom_sf(
    data = uf_caat,
    fill = "transparent",
    color = "black",
    lwd = 0.2
  ) +
  coord_sf(xlim = c(-48,-34), ylim = c(-17.1,-3)) +
  geom_text(data = uf_caat, aes(
    x = c(-42,-39.5, -36.5, -35.5,-34.5,-34.4,-36,-39, -42.4),
    y = c(-16.8,-15,-11,-10,-8.5,-7,-4.7,-2.9,-5),
    label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")
  ),
  size = 2.5) +
  theme(
    text = element_text(family = '', size = 6),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key.size = unit(5, "mm"),
    legend.position = c(0.87, 0.2),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, vjust = 1.5)
  )-> seca.caat_18

ggarrange(nvc.caat_18, seca.caat_18, bov.caat_18, cap.caat_18)->fig.food.map
ggsave(plot = fig.food.map, filename = "fig.food.map.png", units = "in" , height = 8, width = 8.8)
