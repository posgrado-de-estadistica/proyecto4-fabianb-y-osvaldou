
#Libs===============================================

library(sp)
library(sf)
library(tidyverse)
library(gstat)
library(viridis)
library(RColorBrewer)
library(classInt)
library(spdep)
library(gridExtra)
library(car)
library(formattable)
library(corrplot)

#Mapa por distritos==================================

#mapa_distritos <- st_read("CR_Distritos.shp") %>% filter (nom_distr != "ISLA DEL COCO")
#st_bbox(mapa_distritos) # Para no graficar Isla del Coco en mapa

#Cargar datos========================================

rm(list = ls())
setwd("C:/Users/fabia/OneDrive/Documentos/Fabian/Estadística/2019/II Ciclo/Estadística Espacial/Proyectos/4")

# names(ips_df)
# ips <- read.csv("IPS R.csv", sep = ",", dec = ".")
# 
# mapa_sf <- st_read("Cantones_de_Costa_Rica.shp")
#
# ccss <- read.csv("CCSS.csv", sep = ",", dec = ".")
# 
# ips_sf <- mapa_sf %>% as.data.frame() %>%
#   left_join(ips, by = c("NOM_CANT_1" = "Cantón")) %>%
#   left_join(ccss, by = c("NOM_CANT_1" = "Cantón")) %>% 
#   st_as_sf(crs = crs(mapa_sf))
# 
#   
# ips_sp <- as(ips_sf, "Spatial")
# 
# ips_df <- ips_sf %>% as_tibble()
# 
# save(ips, ips_sf, ips_sp, ips_df, mapa_sf, file = "Proyecto 4 datos.RData")

load("Proyecto 4 datos.Rdata")

#Mapas===============================================

brks_ips <- quantile(round(ips_sf$IPS, 10), seq(0, 1, 0.20), type = 9)
brks_ips[1] <- min(ips_sf$IPS - 0.001)

ips_sf <- mutate(ips_sf, IPS_Quintil = factor(cut(IPS, brks_ips), order = T))
  
ips_mapa <- ips_sf %>%
  ggplot() +
  geom_sf(aes(fill = IPS_Quintil)) + 
  labs(fill = "IPS Quintil") +
  xlim(286803, 658879) + ylim(889158, 1241118) +
  scale_fill_brewer(palette = "YlGnBu")

#Vecinos============================================
nb_queen <- poly2nb(ips_sf, queen = TRUE)
nb_queen_sf <- as(nb2lines(nb_queen, coords = coordinates(ips_sp)), 'sf')
nb_queen_sf <- st_set_crs(nb_queen_sf, st_crs(ips_sf))
nb_reina_graf <- ggplot(ips_sf) +
  geom_sf(fill = "slategray2", colour = "white") +
  geom_sf(data = nb_queen_sf) + 
  xlim(286803, 658879) + ylim(889158, 1241118) + 
  ggtitle("Movimiento de la Reina")

nb_torre <- poly2nb(ips_sf, queen = F)
nb_torre_sf <- as(nb2lines(nb_torre, coords = coordinates(ips_sp)), 'sf')
nb_torre_sf <- st_set_crs(nb_torre_sf, st_crs(ips_sf))
nb_torre_graf <- ggplot(ips_sf) +
  geom_sf(fill = "slategray2", colour = "white") +
  geom_sf(data = nb_torre_sf) + 
  xlim(286803, 658879) + ylim(889158, 1241118) +
  ggtitle("Movimiento de la Torre")

grid.arrange(nb_reina_graf, nb_torre_graf, ncol = 2)

#Test de Moran======================================

W_reina <- nb2listw(nb_queen)
W_torre <- nb2listw(nb_torre)
mt_wrn <- moran.mc(ips_sf$IPS, W_reina, 999)
mt_wtr <- moran.mc(ips_sf$IPS, W_torre, 999)

#Modelo lineal======================================

ips_df$tasa_empleo_formal <- ips_df$Trabajadores_Seguro_Salud / ips_df$P.E.A

X <- c("Tasa_de_robos._asaltos_y_hurtos","Habitantes_por_EBAIS", 
       "Cobertura_del_servicio_de_recolección_de_residuos", "Acceso_a_energía_eléctrica", 
        "tasa_empleo_formal", "Acceso_a_Conocimientos_Básicos")

YX <- scale(ips_df %>% dplyr::select(IPS, X)) %>% as.data.frame()
cormat <- YX %>% cor()



fn <- as.formula(paste("IPS", paste(X, collapse = " + "), sep = " ~ "))

mod_lm <- lm(fn, data = YX)
summary(mod_lm)
AIC(mod_lm)

ggplot(data = data.frame(res = mod_lm$residuals), aes(sample = res)) + 
  stat_qq() + stat_qq_line()

#Pruebas de dependencia espacial a los residuos del modelo lineal
lm.morantest(mod_lm, W_reina, alternative = "two.sided")

ips_sf$lmres <- mod_lm$residuals
moran.mc(ips_sf$lmres, W_reina, 999)

lmres_mapa <- ips_sf %>%
  ggplot() +
  geom_sf(aes(fill = lmres)) + 
  labs(fill = "Residuos del lm") +
  xlim(286803, 658879) + ylim(889158, 1241118) 

moran.plot(ips_sf$lmres, W_reina)

#Modelos Espaciales==================================
mod_sar_wreina <- spautolm(fn, data = YX, listw = W_reina)
mod_sar_wtorre <- spautolm(fn, data = YX, listw = W_torre)
mod_car_wreina <- spautolm(fn, data = YX, listw = W_reina, family = "CAR")
mod_car_wtorre <- spautolm(fn, data = YX, listw = W_torre, family = "CAR")

summary(mod_sar_wreina)
summary(mod_sar_wtorre)
summary(mod_car_wreina)
summary(mod_car_wtorre)

AIC(mod_lm)


