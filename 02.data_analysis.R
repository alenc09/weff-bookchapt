# Mon May 31 17:57:33 2021 ------------------------------
#Data analysis script

source("01.data_prep.R")

#library####
library(GGally)

#analysis####
## forest-food ####
map_caat@data%>%
  select(nvcPerc_18, drought_exposure_com, bovMed_18, capMed_18)%>%
  rename("Native vegetation cover" = nvcPerc_18,
         "Drought exposure" = drought_exposure_com,
        "Bovine herd" = bovMed_18,
         "Goat herd" = capMed_18)%>%
ggcorr(method = c("pairwise","spearman"), label = T,
                   hjust = 0.8, vjust = 3,layout.exp = 3, label_round = 4)-> corr.food
ggsave(plot = corr.food, filename = "corr.food.png")
ggsave(plot = corr.food, filename = "figure2.jpg", dpi = 300)

lm(data = map_caat@data, nvcPerc_18 ~ capMed_18 + bovMed_18 + drought_exposure_com)->mod.nvc_food
par(mfrow = c(2,2))
plot(mod.nvc_food)
summary(mod.nvc_food)

## forest-energy####
map_caat@data%>%
  filter(carvKgHa_18 != 0, lenKgHa_18 != 0)->db_test

lm(data = db_test, log(carvKgHa_18) ~ nvcPerc_18)->mod.carv
plot(mod.carv)
summary(mod.carv)

lm(data = db_test, log(lenKgHa_18) ~ nvcPerc_18)->mod.len1
plot(mod.len1)
summary(mod.len1)
lm(data = db_test, log(lenKgHa_18) ~ log(nvcPerc_18))->mod.len2
plot(mod.len2)
summary(mod.len2)
lm(data = db_test, log(lenKgHa_18) ~nvcPerc_18 + I(nvcPerc_18^2) )->mod.len3
plot(mod.len3)
summary(mod.len3)

AIC(mod.len1, mod.len2, mod.len3)

## legalForest-energy ####
 map_caat@data%>%
  filter(carvKgHa_18 != 0, lenKgHa_18 != 0, legal_tMS_a != 0)->db_test2

lm(data = db_test2, log(carvKgHa_18) ~ log(legal_tMS_a))->mod.carv.legal
plot(mod.carv.legal)
summary(mod.carv.legal)

lm(data = db_test2, log(lenKgHa_18) ~ legal_tMS_a)->mod.len.legal1
plot(mod.len.legal1)
summary(mod.len.legal1)
lm(data = db_test2, log(lenKgHa_18) ~ log(legal_tMS_a))->mod.len.legal2
plot(mod.len.legal2)
summary(mod.len.legal2)
lm(data = db_test2, log(lenKgHa_18) ~legal_tMS_a + I(legal_tMS_a^2) )->mod.len.legal3
plot(mod.len.legal3)
summary(mod.len.legal3)

AIC(mod.len.legal1, mod.len.legal2, mod.len.legal3)

