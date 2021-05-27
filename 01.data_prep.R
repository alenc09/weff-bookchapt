# Thu May 27 17:13:39 2021 ------------------------------
#Data preparation for book (wef-brasil) chapter (weff-caatinga)

#library####
library(readxl)
library(dplyr)
library(foreign)
library(stringr)
library(rgdal)

#data ####
read_xlsx("mapb5_cobertura_selec.xlsx")->mapb5_cob
read.dbf("/home/lucas/Documentos/Doutorado/Dados/shapes/BR_Municipios_2020/BR_Municipios_2020.dbf")-> muns_br
read_xlsx("ppm_85-19.xlsx") ->ppm_85_19
read_xlsx("wivi_dim.xlsx")->wivi_dim
readOGR(dsn = "/home/lucas/Documentos/Doutorado/Dados/shapes/", layer = "muncat_2020") ->map_caat


muns_br%>%
  select(CD_MUN, AREA_KM2)%>%
  mutate(CD_MUN= as.double(as.character(CD_MUN)),
         area_ha = AREA_KM2*100)%>%
  glimpse->muns_br

ppm_85_19%>%
  mutate(code_muni = as.double(code_muni))%>%
  glimpse->ppm_85_19
       
mapb5_cob %>%
  group_by(territory_id) %>%
  summarise(across(.cols = c(8:42), .fns = sum)) %>%
  mutate(code_short = as.double(str_sub(as.character(territory_id), end = 6), .keep="all"))%>%
  left_join(x = ., y = select(muns_br, CD_MUN, area_ha), by = c("territory_id" = "CD_MUN"))%>%
  left_join(y=ppm_85_19, by = c("territory_id" = "code_muni"))%>%
  left_join(y=select(wivi_dim, new_database_code_short, drought_exposure_com), by=c("code_short" = "new_database_code_short"))%>%
  glimpse ->db_food

map_caat@data%>%
  left_join(y= db_food, by = c("code_mn" = "territory_id"))%>%
  glimpse->map_caat@data

