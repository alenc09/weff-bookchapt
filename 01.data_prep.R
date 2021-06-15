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
read_xlsx("area_num_prop.xlsx", sheet = 1)->num_prop
readOGR(dsn = "/home/lucas/Documentos/Doutorado/Dados/shapes/", layer = "muncat_2020") ->map_caat
read_xlsx("pevs_18.xlsx")->pevs_18
read_xlsx("/home/lucas/Documentos/Doutorado/projetos_paralelos/cap_wef-brasil/NEXUS_database.xlsx")->nexus_db

## db_food####
muns_br%>%
  select(CD_MUN, AREA_KM2)%>%
  mutate(CD_MUN= as.double(as.character(CD_MUN)),
         area_ha = AREA_KM2*100)%>%
  glimpse->muns_br

ppm_85_19%>%
  mutate(code_muni = as.double(code_muni))%>%
  glimpse->ppm_85_19

num_prop%>%
  mutate(code_muni = as.double(num_prop$code_muni))%>%
  glimpse->num_prop

mapb5_cob %>%
  group_by(territory_id) %>%
  summarise(across(.cols = c(8:42), .fns = sum)) %>%
  mutate(code_short = as.double(str_sub(as.character(territory_id), end = 6), .keep="all"))%>%
  left_join(x = ., y = select(muns_br, CD_MUN, area_ha), by = c("territory_id" = "CD_MUN"))%>%
  left_join(y=ppm_85_19, by = c("territory_id" = "code_muni"))%>%
  left_join(y=select(wivi_dim, new_database_code_short, drought_exposure_com), by=c("code_short" = "new_database_code_short"))%>%
  left_join(y=select(num_prop, code_muni, Total), by = c("territory_id" = "code_muni"))%>%
  rename(num_prop = Total)%>%
  mutate(nvcPerc_18 = (`2018`)*100/area_ha,
         bovMed_18 = bov_18/num_prop,
         capMed_18 = cap_18/num_prop)%>%
  filter(!nvcPerc_18 > 100)%>%
  distinct()%>%
  glimpse ->db_food

length(unique(db_food$territory_id))


map_caat@data%>%
  left_join(y= db_food, by = c("code_mn" = "territory_id"))%>%
  glimpse ->map_caat@data

## db_energy####
pevs_18%>%
  mutate(code_muni = as.double(code_muni))%>%
  left_join(y = select(muns_br, CD_MUN, area_ha), by = c("code_muni" = "CD_MUN"))%>%
  left_join(y= select(nexus_db, cód.mun.,legal_tMS_a), by = c("code_muni" = "cód.mun."))%>%
  mutate(carvKgHa_18 = carv_18*1000/area_ha,
         lenKgHa_18 = len_18*1000/area_ha)%>%
  glimpse->db_energy

map_caat@data%>%
  left_join(y=select(db_energy,code_muni, carvKgHa_18, lenKgHa_18, legal_tMS_a),
            by = c("code_mn" = "code_muni"))%>%
  glimpse->map_caat@data
  