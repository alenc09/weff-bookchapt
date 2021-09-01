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
library(cowplot)

## data####
st_as_sf(map_caat)->sf.map_caat
uf<-read_state()
uf %>% 
  filter(abbrev_state %in% c("MG","BA","SE","PE", "AL","PB","RN","CE","PI"))->uf_caat

read_country()->brazil_map

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

##inset map####
ggplot()+
  geom_sf(data = brazil_map,
        fill = "transparent",
        #colour = "black",
        lwd = 0.2)+
  geom_sf(data = sf.map_caat,
          colour = "transparent",
          fill="grey50")+
  geom_sf(data=uf$geom,
          fill = "transparent",
          colour = "black",
          lwd = 0.1)+
  theme(
    text = element_text(family = '', size = 6),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA))->inset_map

##fig 1a####
ggdraw()+
  draw_plot(nvc.caat_18)+
  draw_plot(inset_map,
            x = 0.04, y = 0.65, width = 0.35, height = 0.35)->fig.1a

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

plot_grid(fig.1a, seca.caat_18, bov.caat_18, cap.caat_18, 
          labels = c("a", "b", "c", "d"))->fig.food.map
ggsave(plot = fig.food.map, filename = "fig.food.map.png", units = "in" , height = 8, width = 8.8)
ggsave(plot = fig.food.map, filename = "figure3.jpg", units = "in" , height = 8, width = 8.8, dpi = 300)

#Figures####
## Energy####
map_caat@data%>%
  filter(carvKgHa_18 != 0)%>%
  ggplot()+
  geom_point(aes(x=nvcPerc_18, y=log(carvKgHa_18)))+
  geom_smooth(aes(x=nvcPerc_18, y=carvKgHa_18), formula = log(y) ~ x, method = "lm")+
  labs(x = "NVC 2018 (%)", y = "Charcoal")+
  theme_classic()+
  theme(axis.title = element_text(size = 12))-> nvc.carv

map_caat@data%>%
  filter(lenKgHa_18 != 0)%>%
  ggplot()+
  geom_point(aes(x=nvcPerc_18, y=log(lenKgHa_18)))+
  geom_smooth(aes(x=nvcPerc_18, y=lenKgHa_18), formula = log(y) ~ x + I(x^2), method = "lm")+
  labs(x = "NVC 2018 (%)", y = "Firewood")+
  theme_classic()+
  theme(axis.title = element_text(size = 12))-> nvc.len

map_caat@data%>%
  filter(carvKgHa_18 != 0, legal_tMS_a!=0)%>%
  ggplot()+
  geom_point(aes(x=log(legal_tMS_a), y=log(carvKgHa_18)))+
  geom_smooth(aes(x=log(legal_tMS_a), y=carvKgHa_18), formula = log(y) ~ x, method = "lm")+
  labs(x = "Legal offer of PDM", y = "Charcoal")+
  theme_classic()+
  theme(axis.title = element_text(size = 12))->legal.carv

map_caat@data%>%
  filter(lenKgHa_18 != 0, legal_tMS_a!=0)%>%
  ggplot()+
  geom_point(aes(x=log(legal_tMS_a), y=log(lenKgHa_18)))+
  geom_smooth(aes(x=log(legal_tMS_a), y=lenKgHa_18), formula = log(y) ~ x, method = "lm")+
  labs(x = "Legal offer of PDM", y = "Firewood")+
  theme_classic()+
  theme(axis.title = element_text(size = 12))-> legal.len

plot_grid(nvc.carv, legal.carv, nvc.len, legal.len, labels = c("A", "B", "C", "D") )-> panel.energy
ggsave("panel.energy.png", dpi=300)
ggsave(plot = panel.energy, filename = "figure4.jpg", dpi=300)
