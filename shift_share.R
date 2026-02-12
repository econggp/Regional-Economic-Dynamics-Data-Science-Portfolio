library(car)
library(glmnet)
library(spatialreg)
library(spdep)
library(sf)
library(plm)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(RColorBrewer)
library(classInt)
library(plotrix)
library(circlize)
library(tmap)
library(dplyr)
library(datasets)
library(tidyverse)
library(splm)
library(spldv)
library(pspatreg)
library(SDPDmod)
library(terra) 
library(spDataLarge)
library(readr)
library(kableExtra)
library(vegan)
library(fastDummies)
library(writexl)


dsse <- bin_02


csse03 <- dsse %>% filter(tcode == "2003")
csse08 <- dsse %>% filter(tcode == "2008")
csse13 <- dsse %>% filter(tcode == "2013")
csse18 <- dsse %>% filter(tcode == "2018")


mxsse <- read_sf("C:/Users/gezum/OneDrive/Escritorio/Entidades_Federativas/Entidades_Federativas.shp")

encodings <- guess_encoding("C:/Users/gezum/OneDrive/Escritorio/Entidades_Federativas/Entidades_Federativas.dbf")
print(encodings)

# Identificar las columnas de texto
text_columns <- sapply(mxsse, is.character)

# Convertir solo las columnas de texto a UTF-8
for (col in names(mxsse)[text_columns]) {
  mxsse[[col]] <- iconv(mxsse[[col]], from = "windows-1252", to = "UTF-8")
}

st_geometry_type(mxsse)



dcor <- coordenadas 
coord_sf1 <- cbind(dcor$long, dcor$lat)
vecinos <- knearneigh(coord_sf1, k = 3)
pesos <- nb2listw(knn2nb(vecinos))
W <- kronecker(diag(4), as.matrix(listw2mat(pesos)))
dim(W)
is.matrix(W)
W <- mat2listw(W, style = "W")

csse03_sf <- left_join(csse03, mxsse, by = c("NOMGEO" = "NOMGEO", "CVEGEO"="CVEGEO"))

csse03_sf <- csse03_sf %>%
  mutate(
    espacialedjpacd = lag.listw(pesos, edjpacd)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialeejpacd = lag.listw(pesos, eejpacd)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialetjpacd = lag.listw(pesos, etjpacd)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialeijpacd = lag.listw(pesos, eijpacd)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialemjpacd = lag.listw(pesos, emjpacd)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialermjpacd = lag.listw(pesos, ermjpacd)
  )


ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialedjpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EDj PACD 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialeejpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EEj PACD 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialetjpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "ETj PACD 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialeijpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EIj PACD 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialemjpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EMj PACD 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialermjpacd)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "ERMj PACD 2018 vs 2003")

csse03_sf <- csse03_sf %>%
  mutate(
    espacialedjppvs = lag.listw(pesos, edjppvs)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialeejppvs = lag.listw(pesos, eejppvs)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialetjppvs = lag.listw(pesos, etjppvs)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialeijppvs = lag.listw(pesos, eijppvs)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialemjppvs = lag.listw(pesos, emjppvs)
  )

csse03_sf <- csse03_sf %>%
  mutate(
    espacialermjppvs = lag.listw(pesos, ermjppvs)
  )


ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialedjppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EDj PPVS 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialeejppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EEj PPVS 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialetjppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "ETj PPVS 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialeijppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EIj PPVS 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialemjppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "EMj PPVS 2018 vs 2003")

ggplot(data = csse03_sf) +  
  geom_sf(aes(geometry = geometry, fill = espacialermjppvs)) +  
  scale_fill_viridis_c(direction = -1) +  
  theme_minimal() +  
  labs(title = "ERMj PPVS 2018 vs 2003")


