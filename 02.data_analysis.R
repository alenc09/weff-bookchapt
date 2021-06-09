# Mon May 31 17:57:33 2021 ------------------------------
#Data analysis script

source("01.data_prep.R")

#library####
library(GGally)

#analysis####

map_caat@data%>%
  select(nvcPerc_18, drought_exposure_com, bovMed_18, capMed_18)%>%
  rename("Native vegetation cover" = nvcPerc_18,
         "Drought exposure" = drought_exposure_com,
        "Bovine herd" = bovMed_18,
         "Goat herd" = capMed_18)%>%
ggcorr(method = c("pairwise","spearman"), label = T,
                   hjust = 0.8, vjust = 3,layout.exp = 3, label_round = 4)-> corr.food
ggsave(plot = corr.food, filename = "corr.food.png")


lm(data = map_caat@data, nvcPerc_18 ~ capMed_18 + bovMed_18 + drought_exposure_com)->mod.nvc_food
par(mfrow = c(2,2))
plot(mod.nvc_food)
summary(mod.nvc_food)
