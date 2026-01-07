library(car)
library(glmnet)
library(sf)
library(spdep)
library(spatialreg)
library(splm)
library(spldv)
library(pspatreg)
library(SDPDmod)
library(terra) 
library(spData)
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
library(spDataLarge)
library(plm)
library(ggplot2)
library(GGally)
library(ggrepel)
library(cowplot)
library(patchwork)
library(magick)
library(RColorBrewer)
library(classInt)
library(plotrix)
library(circlize)
library(tmap)
library(dplyr)
library(purrr)
library(datasets)
library(tidyverse)
library(readr)
library(kableExtra)
library(xtable)
library(vegan)
library(fastDummies)
library(deaR)
library(writexl)




d01 <- coordenadas 
coord_sf1 <- cbind(d01$long, d01$lat)
vecinos <- knearneigh(coord_sf1, k = 4)
pesos <- nb2listw(knn2nb(vecinos))

W <- kronecker(diag(4), as.matrix(listw2mat(pesos)))
dim(W)
is.matrix(W)
W <- mat2listw(W, style = "W")


### Maping

mx <- read_sf("C:/Users/gezum/OneDrive/Escritorio/Entidades_Federativas/Entidades_Federativas.shp")

encodings <- guess_encoding("C:/Users/gezum/OneDrive/Escritorio/Entidades_Federativas/Entidades_Federativas.dbf")
print(encodings)

# Identificar las columnas de texto
text_columns <- sapply(mx, is.character)

# Convertir solo las columnas de texto a UTF-8
for (col in names(mx)[text_columns]) {
  mx[[col]] <- iconv(mx[[col]], from = "windows-1252", to = "UTF-8")
}

st_geometry_type(mx)

spd <- baseind
spd[is.na(spd)] <- 0
str(spd)

ggpairs(spd, columns = c("qrppvs","qrpacd","prppvs","prpacd","mbi","dca","rat","iaf","roic","roa","eeppvs", "marppvs", "marpacd", "divppvs", "divpacd", "comppvs", "compacd", "eepacd", "srppvs", "srpacd"),
        upper = list(continuous = wrap("cor", size = 4, method = "pearson", stars = TRUE)))


scaled_data <- spd %>% 
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector))

scaled_data$tcode <- spd$tcode
scaled_data$NOMGEO <- spd$NOMGEO
scaled_data$CVE_GEO <- spd$CVE_GEO
scaled_data$CVE_ENT <- spd$CVE_ENT

joined_df <- left_join(scaled_data, mx, by = c("NOMGEO" = "NOMGEO", "CVE_GEO"="CVEGEO"))
dpsf <- st_sf(joined_df)
dpsf01 <- merge(dpsf, d01, by = "NOMGEO")

str(dpsf01)

######## Spatial panel data model specification

ecu <- ptfr ~ ptf+marppvs+marpacd+marppvsr+marpacdr+divppvsr+
  divpacdr+divppvs+divpacd+comppvsr+compacdr+comppvs+compacd+
  roar+roicr+iafr+ratr+dcar+mbir+roa+roic+iaf+rat+dca+mbi+eeppvs+
  eepacd+eeppvsr+eepacdr+srppvs+srpacd+srppvsr+srpacdr+
  qrppvs+qrpacd+qrppvsr+qrpacdr+crrppvs+crrpacd


alias(lm(ecu, data = dpsf01))

vif(lm( ptfr ~ ptf+marppvs+marpacd+marppvsr+marpacdr+divppvsr+
          divpacdr+divppvs+divpacd+comppvsr+compacdr+comppvs+compacd+
          roar+roicr+iafr+ratr+dcar+mbir+roa+roic+iaf+rat+dca+mbi+eeppvs+
          eepacd+eeppvsr+eepacdr+srppvs+srpacd+srppvsr+srpacdr+
          qrppvs+qrpacd+qrppvsr+qrpacdr+crrppvs+crrpacd, data = dpsf01))


# workout model

ecu1 <- ptfr ~ comppvsr+marpacdr+prppvsr+
  roar + roic + iafr + mbir+ qrpacdr + qrppvsr +
  pspl(eepacd, nknots = 20) + pspl(divpacdr, nknots = 20) +
  pspl(eejpacd, nknots = 20) + pspl(crrppvs, nknots = 20) 
 
### GAM + SAR Model
mod1 <- pspatfit(ecu1, data = dpsf01, na.action = T, 
                 type = "sar", eff_demean = "twoways",
                 listw = W, demean = TRUE)

summary(mod1)

### With a spatial error term
mod2 <- pspatfit(ecu1, data = dpsf01, na.action = T,
                 listw = W, eff_demean = "twoways", 
                 method = "eigen", type = "sem",
                 demean = TRUE)

summary(mod2)

### GAM pure
mod3 <- pspatfit(ecu1, data = dpsf01, 
                eff_demean = "twoways", na.action = T)
summary(mod3)


ecu2 <-  ptfr ~ comppvsr+marpacdr+prppvsr+
  roar+roic+iafr+ mbir+qrpacdr+qrppvsr+
  pspl(eepacd, nknots = 20) + pspl(divpacdr, nknots = 20) +
  pspl(eejpacd, nknots = 20) + pspl(crrppvs, nknots = 20) +
  pspt(lat, long, tcode, nknots = c(10, 10,10), psanova = F)

###  GAM + GEO Model
mod4 <- pspatfit(ecu2, data = dpsf01, demean = TRUE,
                     eff_demean = "twoways", na.action = T)
summary(mod4)

### Add a spatial lag (sar) and temporal correlation in the noise of PSANOVA 3d model
mod5 <- pspatfit(ecu2, data = dpsf01, eff_demean = "twoways", na.action = T,
                        type = "sar", 
                        listw = pesos)
summary(mod5)

ecu3 <-  ptfr ~ comppvsr+marpacdr+prppvsr+
  roar+roic+iafr+ mbir+qrpacdr+qrppvsr+
  pspl(eepacd, nknots = 20) + pspl(divpacdr, nknots = 20) +
  pspl(eejpacd, nknots = 20) + pspl(crrppvs, nknots = 20) +
  pspt(lat, long, tcode, nknots = c(10, 10,10), psanova = T)


### Models with psanova 3d spatial trend
mod6 <- pspatfit(ecu3, data = dpsf01, eff_demean = "twoways", 
                             na.action = T, type = "sar", 
                             listw = pesos)
summary(mod6)

## Comparison between models
anova(mod1, mod2, mod3, mod4, mod5, mod6,lrtest = FALSE)

### plot spatial trend for spatial point coordinate
plot_sp3d(mod4, data = dpsf01, 
          time_var = "tcode", time_index = c(2003, 2008, 2013, 2018),
          addmain = FALSE, addint = FALSE)

### Non-Parametric Total, Direct and Indirect impacts
nparimpacts <- impactsnopar(mod1, listw = W, viewplot = FALSE)
plot_impactsnopar(nparimpacts, data = dpsf01, smooth = TRUE) 

list_varnopar <- c("eepacd","divpacdr", "eejpacd", "crrppvs")
terms_nopar <- fit_terms(mod1, list_varnopar)
plot_terms(terms_nopar, dpsf01, alpha = 0.10)

img1 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eepacdd.png") 
img2 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eepacdi.png") 
img3 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eepacdt.png") 
img4 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/divpacdrd.png")
img5 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/divpacdri.png")
img6 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/divpacdrt.png")
img7 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eejpacdd.png")
img8 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eejpacdi.png")
img9 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/eejpacdt.png")
img10 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/crrppvsd.png")
img11 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/crrppvsi.png")
img12 <- image_read("D:/OneDrive - correo.xoc.uam.mx/Documentos/Investigación/UAM-UNAM/crrppvst.png")

images <- image_scale(c(img1, img2, img3, img4, img5, img6, img7, img8, img9, img10, img11, img12), "450x450")
combined_image <- image_montage(images, geometry = "450x450+3+3", tile = "3x4")
print(combined_image)



### Parametric Total, Direct and Indirect impacts
imp_parvar_sar <- impactspar(mod1, W)
summary(imp_parvar_sar)



fitted(mod1)

residuals(mod1)

vcov(mod1, bayesian = T)

summary(mod1)



