# Mon May 31 20:13:50 2021 ------------------------------
#Maps for book chapter wef-Brasil

source("02.data_analysis.R")

#library####
library(sf)

#maps####
##nvc_2018####
st_as_sf(map_caat)->sf.map_caat


ggplot()+  
  geom_sf(data=sf.map_caat$geometry, aes(fill = sf.map_caat$nvcPerc_18), lwd = 0)+
  scale_fill_continuous()#+
  #geom_sf(data=uf_caat, fill="transparent")+
  #coord_sf(expand = FALSE)+
  #labs(subtitle="Human capital", size=8, fill = "Vulnerability") +
  #theme_map()+
  #theme(legend.position = "none")
