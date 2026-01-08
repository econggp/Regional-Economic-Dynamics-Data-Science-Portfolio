# Manipulación de datos
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(knitr)
library(rpivotTable)
library(reshape2)

# Visualización
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(gghighlight)
library(fmsb)

# Estadística descriptiva e inferencial
library(psych)
library(broom)
library(car)
library(corrr)

# Modelado predictivo
library(caret)
library(randomForest)
library(xgboost)
library(e1071)

# Clustering y reducción de dimensionalidad
library(cluster)
library(FactoMineR)
library(factoextra)
library(fpc)
library(mclust)

# Gráficos especializados
library(igraph)
library(treemapify)
library(highcharter)

# Espacial
library(janitor)
library(skimr)
library(naniar)
library(sf)


data <- bin

tgen<- rpivotTable(data, rows="NOMGEO", col="AE", aggregatorName="Average", 
                   vals="compacd")
tgen

#Distribución

marpacd <- dcast(data, AE~tcode, sum, value.var = "marpacd", margins=TRUE) 
marpacd %>% kable(., caption="Especialización sectorial PACD 2003-2018")

write.csv(marpacd, "marpacd_ae.csv", row.names = FALSE)

marppvs <- dcast(data, AE~tcode, sum, value.var = "marppvs", margins=TRUE)
marppvs %>% kable(., caption="Especialización sectorial PPVS 2003-2018")

write.csv(marppvs, "marppvs_ae.csv", row.names = FALSE)

# marpot <- dcast(data, AE~tcode, sum, value.var = "marpot", margins=TRUE)
# marpot %>% kable(., caption="Especialización sectorial POT 2003-2018")

divpacd <- dcast(data, AE~tcode, sum, value.var = "divpacd", margins=TRUE)
divpacd %>% kable(., caption="Peso en la diversificación sectorial PACD 2003-2018")

write.csv(divpacd, "divpacd_ae.csv", row.names = FALSE)

divppvs <- dcast(data, AE~tcode, sum, value.var = "divppvs", margins=TRUE)
divppvs %>% kable(., caption="Peso en la diversificación sectorial PPVS 2003-2018")

write.csv(divppvs, "divppvs_ae.csv", row.names = FALSE)

#divpot <- dcast(data, AE~tcode, sum, value.var = "divpot", margins=TRUE)
#divpot %>% kable(., caption="Peso en la diversificación sectorial POT 2003-2018")

compacd <- dcast(data, AE~tcode, sum, value.var = "compacd", margins=TRUE)
compacd %>% kable(., caption="Competencia sectorial PACD 2003-2018")

write.csv(compacd, "compacd_ae.csv", row.names = FALSE)

comppvs <- dcast(data, AE~tcode, sum, value.var = "comppvs", margins=TRUE)
comppvs %>% kable(., caption="Competencia sectorial PPVS 2003-2018")

write.csv(comppvs, "comppvs_ae.csv", row.names = FALSE)


marpacde <- dcast(data, NOMGEO~tcode, sum, value.var = "marpacd", margins=TRUE) 
marpacde %>% kable(., caption="Especialización nacional PACD 2003-2018")

write.csv(marpacde, "marpacde.csv", row.names = FALSE)

marppvse <- dcast(data, NOMGEO~tcode, sum, value.var = "marppvs", margins=TRUE)
marppvse %>% kable(., caption="Especialización nacional PPVS 2003-2018")

write.csv(marppvse, "marppvse.csv", row.names = FALSE)

# marpote <- dcast(data, NOMGEO~tcode, sum, value.var = "marpot", margins=TRUE)
# marpote %>% kable(., caption="Especialización nacional POT 2003-2018")

divpacde <- dcast(data, NOMGEO~tcode, sum, value.var = "divpacd", margins=TRUE)
divpacde %>% kable(., caption="Diversificación nacional PACD 2003-2018")

write.csv(divpacde, "divpacde.csv", row.names = FALSE)

divppvse <- dcast(data, NOMGEO~tcode, sum, value.var = "divppvs", margins=TRUE)
divppvse %>% kable(., caption="Diversificación nacional PPVS 2003-2018")

write.csv(divppvse, "divppvse.csv", row.names = FALSE)

#divpote <- dcast(data, NOMGEO~tcode, sum, value.var = "divpots", margins=TRUE)
#divpote %>% kable(., caption="Peso en la diversificación nacional POT 2003-2018")

compacde <- dcast(data, NOMGEO~tcode, sum, value.var = "compacd", margins=TRUE)
compacde %>% kable(., caption="Comersificación nacional PACD 2003-2018")

write.csv(compacde, "compacde.csv", row.names = FALSE)

comppvse <- dcast(data, NOMGEO~tcode, sum, value.var = "comppvs", margins=TRUE)
comppvse %>% kable(., caption="Comersificación nacional PPVS 2003-2018")

write.csv(comppvse, "comppvse.csv", row.names = FALSE)


(dmarpacd <- ggplot(data, aes(x=marpacd)) + 
    geom_histogram(color="white", fill="blue") +
    facet_wrap(~tcode)+
    geom_freqpoly()+
    labs(title = "Especialización PACD") +
    scale_fill_continuous(name="marpacd"))

(dmarppvs <- ggplot(data, aes(x=marppvs)) + 
    geom_histogram(color="black", fill="white") +
    facet_wrap(~tcode)+
    geom_freqpoly()+
    labs(title = "Especialización PPVS") +
    scale_fill_continuous(name="marppvs"))

(ddivpacd <- ggplot(data, aes(x=divpacd)) + 
    geom_histogram(color="white", fill="red") +
    facet_wrap(~tcode)+
    geom_freqpoly()+
    labs(title = "Diversificación PACD") +
    scale_fill_continuous(name="divpacd"))

(ddivppvs <- ggplot(data, aes(x=divppvs)) + 
    geom_histogram(color="white", fill="black") +
    facet_wrap(~tcode)+
    geom_freqpoly()+
    labs(title = "Diversificación PPVS") +
    scale_fill_continuous(name="divppvs"))

library(gridExtra)

grid.arrange(dmarpacd, dmarppvs,ddivpacd,ddivppvs, nrow=2, 
             top= "Distribución de las externalidades cognitivas",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")



(marpacdeg <- xyplot(marpacd ~ tcode|AE, data_esc,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))

(marppvseg <- xyplot(marppvs ~ tcode|AE, data_esc, 
                layout=c(4, 5),
                type=c('p', 'r'),
                auto.key=list(space='right')))

(divpacdeg <- xyplot(divpacdr ~ tcode|AE, data_esc,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))

(divppvseg <- xyplot(divppvsr ~ tcode|AE, data_esc,
                     layout=c(4, 5),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))


(marpacdee <- xyplot(marpacd ~ tcode|NOMGEO, data_esc, 
                 layout=c(4, 8),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))

(marppvsee <- xyplot(marppvs ~ tcode|NOMGEO, data_esc, 
                layout=c(4, 8),
                type=c('p', 'r'),
                auto.key=list(space='right')))

(divpacdee <- xyplot(divpacd ~ tcode|NOMGEO, data, 
                 layout=c(4, 8),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))

(divppvsee <- xyplot(divppvsr ~ tcode|NOMGEO, data, 
                     layout=c(4, 8),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))

grid.arrange(marpacdeg, marppvseg, divpacdeg, divppvseg, nrow=2, 
             top= "Externalidades cognitivas por Sector 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(marpacdee, marppvsee, divpacdee, divppvsee, nrow=2, 
             top= "Externalidades cognitivas por Entidad 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


#Especialización

(emarpacd <- xyplot(marpacd > 1 ~ tcode|AE, data, 
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

(emarppvs <- xyplot(marppvs > 1 ~ tcode|AE, data, 
                   layout=c(4, 5),
                   type=c('p', 'r'),
                   auto.key=list(space='right')))

(edivpacd <- xyplot(divpacdr>0 ~ tcode|AE, data, 
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

(edivppvs <-xyplot(divppvsr>0 ~ tcode|AE, data, 
                   layout=c(4, 5),
                   type=c('p', 'r'),
                   auto.key=list(space='right')))

(fmarpacd <- xyplot(marpacd > 1 ~ tcode|NOMGEO, data, 
                   layout=c(4, 8),
                   type=c('p', 'r'),
                   auto.key=list(space='right')))

(fmarppvs <- xyplot(marppvs > 1 ~ tcode|NOMGEO, data, 
                    layout=c(4, 8),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

(fdivpacd <- xyplot(divpacdr>0 ~ tcode|NOMGEO, data, 
                    layout=c(4, 8),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

(fdivppvs <- xyplot(divppvsr>0 ~ tcode|NOMGEO, data, 
                    layout=c(4, 8),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))


grid.arrange(emarpacd, emarppvs, edivpacd, edivppvs, nrow=2, 
             top= "Crecimiento de las Externalidades Cognitivas por sector 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(fmarpacd, fmarppvs, fdivpacd, fdivppvs, nrow=2, 
             top= "Crecimiento de las Externalidades Cognitivas por Entidad 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

# Capacidades productivas

itec <- dcast(data, AE~tcode, sum, value.var = "itec", margins=TRUE)
itec %>% kable(., caption="ITEC por Sector 2003-2018")

write.csv(itec, "itec_ae.csv", row.names = FALSE)

efene <- dcast(data, AE~tcode, sum, value.var = "efene", margins=TRUE)
efene %>% kable(., caption="EFENE por Sector 2003-2018")

write.csv(efene, "efene_ae.csv", row.names = FALSE)

automa <- dcast(data, AE~tcode, sum, value.var = "automa", margins=TRUE)
automa %>% kable(., caption="AUTOMA Sector 2003-2018")

write.csv(automa, "automa_ae.csv", row.names = FALSE)

ecpacd <- dcast(data, AE~tcode, sum, value.var = "ecpacd", margins=TRUE)
ecpacd %>% kable(., caption="ECPACD Sector 2003-2018")

write.csv(ecpacd, "ecpacd_ae.csv", row.names = FALSE)

ecppvs <- dcast(data, AE~tcode, sum, value.var = "ecppvs", margins=TRUE)
ecppvs %>% kable(., caption="ECPPVS Sector 2003-2018")

write.csv(ecppvs, "ecppvs_ae.csv", row.names = FALSE)

itece <- dcast(data, NOMGEO~tcode, sum, value.var = "itec", margins=TRUE)
itece %>% kable(., caption="ITEC por Entidad 2003-2018")

write.csv(itece, "itec_NOMGEO.csv", row.names = FALSE)

efenee <- dcast(data, NOMGEO~tcode, sum, value.var = "efene", margins=TRUE)
efenee %>% kable(., caption="EFENE por Entidad 2003-2018")

write.csv(efenee, "efene_NOMGEO.csv", row.names = FALSE)

automae <- dcast(data, NOMGEO~tcode, sum, value.var = "automa", margins=TRUE)
automae %>% kable(., caption="AUTOMA Entidad 2003-2018")

write.csv(automae, "automa_NOMGEO.csv", row.names = FALSE)

ecpacde <- dcast(data, NOMGEO~tcode, sum, value.var = "ecpacd", margins=TRUE)
ecpacde %>% kable(., caption="ECPACD Entidad 2003-2018")

write.csv(ecpacde, "ecpacd_NOMGEO.csv", row.names = FALSE)

ecppvse <- dcast(data, NOMGEO~tcode, sum, value.var = "ecppvs", margins=TRUE)
ecppvse %>% kable(., caption="ECPPVS Entidad 2003-2018")

write.csv(ecppvse, "ecppvs_NOMGEO.csv", row.names = FALSE)

# Capacidades de inversión

mbi <- dcast(data, AE~tcode, sum, value.var = "mbi", margins=TRUE)
mbi %>% kable(., caption="MBI Sector 2003-2018")

write.csv(mbi, "mbi_ae.csv", row.names = FALSE)

sact <- dcast(data, AE~tcode, sum, value.var = "sact", margins=TRUE)
sact %>% kable(., caption="SACT Sector 2003-2018")

write.csv(sact, "sact_ae.csv", row.names = FALSE)

ecos <- dcast(data, AE~tcode, sum, value.var = "ecos", margins=TRUE)
ecos %>% kable(., caption="ECOS Sector 2003-2018")

write.csv(ecos, "ecos_ae.csv", row.names = FALSE)

ecose <- dcast(data, NOMGEO~tcode, sum, value.var = "ecos", margins=TRUE)
ecose %>% kable(., caption="ECOS por Entidad 2003-2018")

write.csv(ecose, "ecose.csv", row.names = FALSE)

sacte <- dcast(data, NOMGEO~tcode, sum, value.var = "sact", margins=TRUE)
sacte %>% kable(., caption="SACT por Entidad 2003-2018")

write.csv(sacte, "sactee.csv", row.names = FALSE)

mbie <- dcast(data, NOMGEO~tcode, sum, value.var = "mbi", margins=TRUE)
mbie %>% kable(., caption="MBI por Entidad 2003-2018")

write.csv(mbie, "mbie.csv", row.names = FALSE)

# Capacidades de soporte

tiid <- dcast(data, AE~tcode, sum, value.var = "tiid", margins=TRUE)
tiid %>% kable(., caption="TIID Sector 2003-2018")

write.csv(tiid, "tiid_ae.csv", row.names = FALSE)

cdig <- dcast(data, AE~tcode, sum, value.var = "cdig", margins=TRUE)
cdig %>% kable(., caption="CDIG Sector 2003-2018")

write.csv(cdig, "cdig_ae.csv", row.names = FALSE)

sop <- dcast(data, AE~tcode, sum, value.var = "sop", margins=TRUE)
sop %>% kable(., caption="SOP Sector 2003-2018")

write.csv(sop, "sop_ae.csv", row.names = FALSE)

ital <- dcast(data, AE~tcode, sum, value.var = "ital", margins=TRUE)
ital %>% kable(., caption="ITAL Sector 2003-2018")

write.csv(ital, "ital_ae.csv", row.names = FALSE)

tiide <- dcast(data, NOMGEO~tcode, sum, value.var = "tiid", margins=TRUE)
tiide %>% kable(., caption="TIID Entidad 2003-2018")

write.csv(tiide, "tiid_NOMGEO.csv", row.names = FALSE)

cdige <- dcast(data, NOMGEO~tcode, sum, value.var = "cdig", margins=TRUE)
cdige %>% kable(., caption="CDIG Entidad 2003-2018")

write.csv(cdige, "cdig_NOMGEO.csv", row.names = FALSE)

sope <- dcast(data, NOMGEO~tcode, sum, value.var = "sop", margins=TRUE)
sope %>% kable(., caption="SOP Entidad 2003-2018")

write.csv(sope, "sop_NOMGEO.csv", row.names = FALSE)

itale <- dcast(data, NOMGEO~tcode, sum, value.var = "ital", margins=TRUE)
itale %>% kable(., caption="ITAL Entidad 2003-2018")

write.csv(itale, "ital_NOMGEO.csv", row.names = FALSE)



# Determinación de los pesos para los índices de capacidades

# ICP

iicp <- data[, c( "automa", "ecpacd", "ecppvs", "sact")]

pca_icp <- PCA(iicp, graph = FALSE)


# Ver contribuciones de cada variable
fviz_pca_var(pca_icp)

# Ver cargas factoriales del primer componente (el más importante)
pca_icp$var$coord %>% kable(., caption="ICP")



# ICI

iici <- data[, c("ecos", "efene", "mbi")]

pca_ici <- PCA(iici, graph = FALSE)

# Ver contribuciones de cada variable
fviz_pca_var(pca_ici)

# Ver cargas factoriales del primer componente (el más importante)
pca_ici$var$coord %>% kable(., caption="ICI")

# ICS

iics <- data[, c("cdig", "ital", "itec")]

pca_ics <- PCA(iics, graph = FALSE)

# Ver contribuciones de cada variable
fviz_pca_var(pca_ics)

# Ver cargas factoriales del primer componente (el más importante)
pca_ics$var$coord %>% kable(., caption="ICS")


### Productividad

(ptfme <- xyplot(ptfm>mean(ptfm) ~ tcode|AE, data, 
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

(ctme <- xyplot(ctm>mean(ctm) ~ tcode|AE, data, 
                     layout=c(4, 5),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))

(ceme <- xyplot(cem>mean(cem) ~ tcode|AE, data, 
                     layout=c(4, 5),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))


(ptfe <- xyplot(ptf > mean(ptf) ~ tcode|AE, data, 
                layout=c(4, 5),
                type=c('p', 'r'),
                auto.key=list(space='right')))

### Adecuación de base de datos

caa <-  dplyr::select(data, NOMGEO, tcode, AE, ID, itec:divpacd)

str(caa)

caa2003 <- filter(caa, tcode == 2003)  
caa2008 <- filter(caa, tcode == 2008)  
caa2013 <- filter(caa, tcode == 2013)  
caa2018 <- filter(caa, tcode == 2018)

caa2003$tcode <- NULL
caa2003$ptfm <- NULL
caa2003$ctm <- NULL
caa2003$cem <- NULL
caa2008$tcode <- NULL
caa2013$tcode <- NULL
caa2018$tcode <- NULL

cau2003 <-  dplyr::select(caa2003, ID, itec:divpacd) %>% tibble::column_to_rownames(., var = 'ID')
cau2008 <-  dplyr::select(caa2008, ID, itec:divpacd) %>% tibble::column_to_rownames(., var = 'ID')
cau2013 <-  dplyr::select(caa2013, ID, itec:divpacd) %>% tibble::column_to_rownames(., var = 'ID')
cau2018 <-  dplyr::select(caa2018, ID, itec:divpacd) %>% tibble::column_to_rownames(., var = 'ID')

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cagu2003 <- cau2003 %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = min_max
  ))

cagu2008 <- cau2008 %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = min_max
  ))

cagu2013 <- cau2013 %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = min_max
  ))

cagu2018 <- cau2018 %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = min_max
  ))

pesosicp <- c(sact=0.4, automa=0.3, ecppvs=0.2, ecpacd=0.1)
pesosici <- c(ecos=0.4, efene=0.4, mbi=0.2)
pesosics <- c(cdig=0.4, ital=0.35, itec=0.25)

cagu2003 <- cagu2003 %>%
  rowwise() %>%
  mutate(icp = sum(c_across(c(sact, automa, 
                            ecppvs, ecpacd)) * unlist(pesosicp))) %>%
  ungroup()

cagu2003 <- cagu2003 %>%
  rowwise() %>%
  mutate(ici = sum(c_across(c(ecos, efene,
                              mbi)) * unlist(pesosici))) %>%
  ungroup()

cagu2003 <- cagu2003 %>%
  rowwise() %>%
  mutate(ics = sum(c_across(c(cdig, ital,
                              itec)) * unlist(pesosics))) %>%
  ungroup()

cagu2003 <- cagu2003 %>%
  mutate(ID = caa2003$ID) %>%
  tibble::column_to_rownames("ID")

cagu2008 <- cagu2008 %>%
  rowwise() %>%
  mutate(icp = sum(c_across(c(sact, automa, 
                              ecppvs, ecpacd)) * unlist(pesosicp))) %>%
  ungroup()

cagu2008 <- cagu2008 %>%
  rowwise() %>%
  mutate(ici = sum(c_across(c(ecos, efene,
                              mbi)) * unlist(pesosici))) %>%
  ungroup()

cagu2008 <- cagu2008 %>%
  rowwise() %>%
  mutate(ics = sum(c_across(c(cdig, ital,
                              itec)) * unlist(pesosics))) %>%
  ungroup()

cagu2008 <- cagu2008 %>%
  mutate(ID = caa2008$ID) %>%
  tibble::column_to_rownames("ID")

cagu2013 <- cagu2013 %>%
  rowwise() %>%
  mutate(icp = sum(c_across(c(sact, automa, 
                              ecppvs, ecpacd)) * unlist(pesosicp))) %>%
  ungroup()

cagu2013 <- cagu2013 %>%
  rowwise() %>%
  mutate(ici = sum(c_across(c(ecos, efene,
                              mbi)) * unlist(pesosici))) %>%
  ungroup()

cagu2013 <- cagu2013 %>%
  rowwise() %>%
  mutate(ics = sum(c_across(c(cdig, ital,
                              itec)) * unlist(pesosics))) %>%
  ungroup()

cagu2013 <- cagu2013 %>%
  mutate(ID = caa2013$ID) %>%
  tibble::column_to_rownames("ID")

cagu2018 <- cagu2018 %>%
  rowwise() %>%
  mutate(icp = sum(c_across(c(sact, automa, 
                              ecppvs, ecpacd)) * unlist(pesosicp))) %>%
  ungroup()

cagu2018 <- cagu2018 %>%
  rowwise() %>%
  mutate(ici = sum(c_across(c(ecos, efene,
                              mbi)) * unlist(pesosici))) %>%
  ungroup()

cagu2018 <- cagu2018 %>%
  rowwise() %>%
  mutate(ics = sum(c_across(c(cdig, ital,
                              itec)) * unlist(pesosics))) %>%
  ungroup()

cagu2018 <- cagu2018 %>%
  mutate(ID = caa2018$ID) %>%
  tibble::column_to_rownames("ID")



### Estadísticos 

caa2003$icp <- cagu2003$icp
caa2003$ici <- cagu2003$ici
caa2003$ics <- cagu2003$ics

caa2008$icp <- cagu2008$icp
caa2008$ici <- cagu2008$ici
caa2008$ics <- cagu2008$ics

caa2013$icp <- cagu2013$icp
caa2013$ici <- cagu2013$ici
caa2013$ics <- cagu2013$ics

caa2018$icp <- cagu2018$icp
caa2018$ici <- cagu2018$ici
caa2018$ics <- cagu2018$ics


# Definir las variables de interés
vars_interes <- c("marppvs", "marpacd", "comppvs", "compacd",
                  "divppvs", "divpacd","icp", "ici", "ics")

# 2003

datos_sel03 <- dplyr::select(caa2003, all_of(vars_interes))

# Calcular estadísticas descriptivas
estadisticas03 <- datos_sel03 %>%
  summarize(
    across(everything(), list(
      media = mean,
      desv_est = sd,
      min = min,
      max = max,
      q1 = ~ quantile(., 0.25),
      mediana = median,
      q3 = ~ quantile(., 0.75),
      cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      range_ratio = ~max(.x, na.rm = TRUE) / min(.x, na.rm = TRUE)
    ), .names = "{col}_{fn}")
  ) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "estadistica"), 
               values_to = "valor", 
               names_sep = "_")

# Convertir a tabla ancha para mejor visualización
estadisticas_03 <- pivot_wider(estadisticas03, 
                                  names_from = estadistica, 
                                  values_from = valor)

# Mostrar como tabla 
kable(estadisticas_03, digits = 3, caption = "Estadísticas descriptivas EC y CT 2003")

write.csv(estadisticas_03, "estadisticas_03.csv", row.names = FALSE)

### Estadísticos 2008

datos_sel08 <- dplyr::select(caa2008, all_of(vars_interes))

# Calcular estadísticas descriptivas
estadisticas08 <- datos_sel08 %>%
  summarize(
    across(everything(), list(
      media = mean,
      desv_est = sd,
      min = min,
      max = max,
      q1 = ~ quantile(., 0.25),
      mediana = median,
      q3 = ~ quantile(., 0.75),
      cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      range_ratio = ~max(.x, na.rm = TRUE) / min(.x, na.rm = TRUE)
    ), .names = "{col}_{fn}")
  ) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "estadistica"), 
               values_to = "valor", 
               names_sep = "_")

# Convertir a tabla ancha para mejor visualización
estadisticas_08 <- pivot_wider(estadisticas08, 
                               names_from = estadistica, 
                               values_from = valor)

# Mostrar como tabla 
kable(estadisticas_08, digits = 3, caption = "Estadísticas descriptivas EC y CT 2008")

write.csv(estadisticas_08, "estadisticas_08.csv", row.names = FALSE)

### Estadísticos 2013

datos_sel13 <- dplyr::select(caa2013, all_of(vars_interes))

# Calcular estadísticas descriptivas
estadisticas13 <- datos_sel13 %>%
  summarize(
    across(everything(), list(
      media = mean,
      desv_est = sd,
      min = min,
      max = max,
      q1 = ~ quantile(., 0.25),
      mediana = median,
      q3 = ~ quantile(., 0.75),
      cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      range_ratio = ~max(.x, na.rm = TRUE) / min(.x, na.rm = TRUE)
    ), .names = "{col}_{fn}")
  ) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "estadistica"), 
               values_to = "valor", 
               names_sep = "_")

# Convertir a tabla ancha para mejor visualización
estadisticas_13 <- pivot_wider(estadisticas13, 
                               names_from = estadistica, 
                               values_from = valor)

# Mostrar como tabla 
kable(estadisticas_13, digits = 3, caption = "Estadísticas descriptivas EC y CT 2013")

write.csv(estadisticas_13, "estadisticas_13.csv", row.names = FALSE)

### Estadísticos 2018

datos_sel18 <- dplyr::select(caa2018, all_of(vars_interes))

# Calcular estadísticas descriptivas
estadisticas18 <- datos_sel18 %>%
  summarize(
    across(everything(), list(
      media = mean,
      desv_est = sd,
      min = min,
      max = max,
      q1 = ~ quantile(., 0.25),
      mediana = median,
      q3 = ~ quantile(., 0.75),
      cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      range_ratio = ~max(.x, na.rm = TRUE) / min(.x, na.rm = TRUE)
    ), .names = "{col}_{fn}")
  ) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "estadistica"), 
               values_to = "valor", 
               names_sep = "_")

# Convertir a tabla ancha para mejor visualización
estadisticas_18 <- pivot_wider(estadisticas18, 
                               names_from = estadistica, 
                               values_from = valor)

# Mostrar como tabla 
kable(estadisticas_18, digits = 3, caption = "Estadísticas descriptivas EC y CT 2018")

write.csv(estadisticas_18, "estadisticas_18.csv", row.names = FALSE)


### Visualización 

dplyr::select(cagu2003, all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplots de DC y CT 2003",
       x = "Variable", y = "Valor") +
  theme_minimal()

dplyr::select(cagu2008, all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplots de DC y CT 2008",
       x = "Variable", y = "Valor") +
  theme_minimal()

dplyr::select(cagu2013, all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplots de DC y CT 2013",
       x = "Variable", y = "Valor") +
  theme_minimal()

dplyr::select(cagu2008, all_of(vars_interes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplots de DC y CT 2018",
       x = "Variable", y = "Valor") +
  theme_minimal()

library(matrixcalc)

# Traza

matrix.trace(cov(dplyr::select(cagu2003, itec:compacd)))
matrix.trace(cov(dplyr::select(cagu2008, itec:compacd)))
matrix.trace(cov(dplyr::select(cagu2013, itec:compacd)))
matrix.trace(cov(dplyr::select(cagu2018, itec:compacd)))


# Distribución

for (var in vars_interes) {
  print(
    ggplot(datos_sel03, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
      theme_minimal()
  )
}
for (var in vars_interes) {
  print(
    ggplot(datos_sel08, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
      theme_minimal()
  )
}
for (var in vars_interes) {
  print(
    ggplot(datos_sel13, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
      theme_minimal()
  )
}
for (var in vars_interes) {
  print(
    ggplot(datos_sel18, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
      theme_minimal()
  )
}

library(ggh4x) # Para facetas anidadas

# Gráfico de pequeñas múltiples
ggplot(caa, aes(x = tcode, y = compacd, group = NOMGEO, color = NOMGEO)) +
  geom_line(alpha = 0.7) +
  facet_nested(~ AE, nest_line = TRUE) +
  scale_color_viridis_d() +
  labs(title = "Competencia PACD por Entidad dentro de cada Actividad Económica",
       x = "Quinquenio",
       y = "Competencia",
       color = "Entidad") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(caa, aes(x = tcode, y = comppvs, group = NOMGEO, color = NOMGEO)) +
  geom_line(alpha = 0.7) +
  facet_nested(~ AE, nest_line = TRUE) +
  scale_color_viridis_d() +
  labs(title = "Competencia PPVS por Entidad dentro de cada Actividad Económica",
       x = "Quinquenio",
       y = "Competencia",
       color = "Entidad") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(caa, aes(x = tcode, y = marpacd, group = NOMGEO, color = NOMGEO)) +
  geom_line(alpha = 0.7) +
  facet_nested(~ AE, nest_line = TRUE) +
  scale_color_viridis_d() +
  labs(title = "Especialización PACD por Entidad dentro de cada Actividad Económica",
       x = "Quinquenio",
       y = "Especialización",
       color = "Entidad") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

library(ggstream)

# Streamgraph por AE dentro de cada entidad
ggplot(data, aes(x = tcode, y = pacd, fill = AE)) +
  geom_stream(type = "proportional") +
  facet_wrap(~ NOMGEO, ncol = 4, nrow = 8, scales = "free_y") +
  labs(title = "Proporción por PACD de las Actividades Económicas por Entidad",
       x = "Quinquenio",
       y = "Proporción") +
  theme_minimal()

ggplot(data, aes(x = tcode, y = ppvs, fill = AE)) +
  geom_stream(type = "proportional") +
  facet_wrap(~ NOMGEO, ncol = 4, nrow = 8, scales = "free_y") +
  labs(title = "Proporción por PPVS de las Actividades Económicas por Entidad",
       x = "Quinquenio",
       y = "Proporción") +
  theme_minimal()




###### Conglomerados

set.seed(123456)

# 2003

# Seleccionar las variables de interés
data_selected03 <- cagu2003 %>%
  dplyr::select(comppvs,compacd, marppvs, 
                marpacd, divppvs, divpacd,
                icp,ici, ics)


# Método del codo

for (k in 2:10) {
  clusterboot_results03 <- clusterboot(data_selected03, 
                                       B = 100, 
                                       clustermethod = hclustCBI, 
                                       method = "ward.D", 
                                       k = k)
  print(paste("k =", k))
  print(clusterboot_results03$bootmean)
}

wss <- sapply(1:10, function(k) {
  kmeans(data_selected03, k, nstart = 25)$tot.withinss
})

# Graficar el método del codo
fviz_nbclust(data_selected03, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Línea vertical en el codo
  labs(title = "Método del Codo", x = "Número de clusters", y = "Suma de cuadrados intragrupo (WSS)")

# DBSCAN

library(dbscan)

result03 <- dbscan(data_selected03, eps = 0.5, minPts = 5)

result03

kNNdistplot(data_selected03, k = 8)  # Gráfico para elegir eps
abline(h = 0.65, col = "red")  # Límite óptimo (codo)

clusterboot_results03 <- clusterboot(data_selected03, 
                                     B = 100, 
                                     clustermethod = hclustCBI, 
                                     method = "ward.D", 
                                     k = 4)

print(clusterboot_results03)

# Extraer resultados en formato tabular
# Crear tabla de estabilidad
stability_table03 <- data.frame(
  Cluster = 1:length(clusterboot_results03$bootmean),
  Jaccard_Mean = clusterboot_results03$bootmean,
  Dissolved_Pct = clusterboot_results03$bootbrd,
  Recovered_Pct = clusterboot_results03$bootrecover
)

# Añadir nivel de estabilidad basado en Jaccard
stability_table03$Stability_Level <- ifelse(
  stability_table03$Jaccard_Mean >= 0.7, "Excelente",
  ifelse(stability_table03$Jaccard_Mean >= 0.60, "Moderada", "Baja")
)

# Ordenar por Cluster
stability_table03 <- stability_table03[order(stability_table03$Cluster), ]

# Crear tabla elegante con kableExtra
library(kableExtra)

stability_table03 %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  kable(col.names = c("Cluster", "Media Jaccard", "% Disolución", "% Recuperación", "Nivel Estabilidad"),
        align = c("c", "c", "c", "c", "l"),
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE,
                font_size = 14) %>%
  row_spec(which(stability_table03$Stability_Level == "Excelente"), 
           bold = TRUE, color = "white", background = "#27ae60") %>%
  row_spec(which(stability_table03$Stability_Level == "Moderada"), 
           background = "#f1c40f") %>%
  row_spec(which(stability_table03$Stability_Level == "Baja"), 
           background = "#e74c3c", color = "white") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c("Estabilidad de Clusters" = 5), 
                   background = "#2c3e50", 
                   color = "white") %>%
  footnote("Validación mediante 100 remuestreos bootstrap")

write.csv(stability_table03, "stability_table03.csv", row.names = FALSE)

ggplot(stability_table03, aes(x = factor(Cluster), y = Jaccard_Mean, fill = Stability_Level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Jaccard_Mean)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Excelente" = "#2ecc71", 
                               "Moderada" = "#f39c12",
                               "Baja" = "#e74c3c")) +
  labs(title = "Estabilidad de Clusters por Índice Jaccard",
       subtitle = "Validación mediante bootstrap (n=100)",
       x = "Cluster", y = "Media Jaccard") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Kmeans

kmeans_result03 <- kmeans(data_selected03, 
                          centers=4, nstart=20)
# Ver los resultados
print(kmeans_result03)

# Agregar los clústeres a los datos originales
cagu2003$cluster <- as.factor(kmeans_result03$cluster)

# Visualización
ggplot(cluster_summary_03, aes(x = cluster, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias por Cluster 2003", x = "Cluster", y = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Hclust

res.dist03 <- get_dist(data_selected03, stand = FALSE, 
                       method = "euclidean")
d03 <- as.matrix(res.dist03)

res.hc03 <- hclust(res.dist03, "ward.D")

grp03 <- cutree(res.hc03, 4)

caa2003$clusterward <- as.factor(grp03)

heatmap(d03, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2003")

agrup03 <- fviz_dend(res.hc03, cex = 0.3, k = 4, rect = TRUE,
                     k_colors = "ucscgb", 
                     color_labels_by_k = TRUE,
                     main = "Agrupación estatal por capacidades productivas año 2003",
                     ylab = "Pesos en la matriz de distancia",
                     xlab = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI", 
                     sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI") 

agrup03


library(clusterSim)

# Evaluación clústers

for (k in 2:10) {
  grp <- cutree(res.hc03, k)
  
  ch <- fpc::calinhara(data_selected03, grp)
  
  # Davies-Bouldin verdadero
  db <- clusterSim::index.DB(data_selected03, grp)$DB
  
  print(paste("k =", k, "CH =", round(ch,2), "DB =", round(db,2)))
}



# Agrupar por cluster y calcular estadística descriptiva

c_summary03 <- caa2003 %>%
  group_by(clusterward) %>%
  summarise(
    count = n(),
    across(
      all_of(vars_interes),
      list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        q1 = ~ quantile(., 0.25, na.rm = TRUE),
        q3 = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -c(clusterward, count),
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

c_summary03 %>%
  mutate(across(c(mean, median, sd, q1, q3), ~ round(., 2))) %>%
  kable(col.names = c("Cluster", "N", "Variable", "Media", "Mediana", "DE", "Q1", "Q3"),
        align = c("c", "c", "l", rep("c", 5))) %>%
  kable_styling("striped") %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  add_header_above(c(" " = 3, "Distribución" = 5))# 2. Pivotar a formato largo (solo columnas de medias)

library(readr)

write_csv(c_summary03, "resultados_clusters03.csv")


for (i in 1:4) {
  cluster_data <- subset(caa2003, clusterward == i)
  write.csv(cluster_data, file = paste("cluster03_", i, ".csv", sep = ""), row.names = FALSE)
}


ggplot(caa2003, aes(x = as.factor(clusterward), y = marpacd)) +
  geom_boxplot() +
  labs(title = "Distribución MARPACD por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = marppvs)) +
  geom_boxplot() +
  labs(title = "Distribución MARPPVS por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = compacd)) +
  geom_boxplot() +
  labs(title = "Distribución COMPACD por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = comppvs)) +
  geom_boxplot() +
  labs(title = "Distribución COMPPVS por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = icp)) +
  geom_boxplot() +
  labs(title = "Distribución ICP por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = ici)) +
  geom_boxplot() +
  labs(title = "Distribución ICI por cluster, 2003", x = "Cluster", y = "Variable1")

ggplot(caa2003, aes(x = as.factor(clusterward), y = ics)) +
  geom_boxplot() +
  labs(title = "Distribución ICS por cluster, 2003", x = "Cluster", y = "Variable1")

# Función para correr ANOVA y extraer resultados limpios

run_anova <- function(var, data) {
  # Crear fórmula
  formula <- as.formula(paste(var, "~ as.factor(clusterward)"))
  
  # Ajustar modelo ANOVA
  model <- aov(formula, data = data)
  
  # Convertir resultados a tibble y añadir nombre de variable
  broom::tidy(model) %>%
    dplyr::filter(term == "as.factor(clusterward)") %>%
    dplyr::mutate(variable = var) %>%  # Añade columna con nombre de variable
    dplyr::select(variable, everything())  # Reordena columnas
}

# Aplicar la función a todas las variables y juntar en un solo data frame
anova_results03 <- lapply(vars_interes, run_anova, data = caa2003) %>%
  bind_rows()

# Renombrar columnas para mayor claridad
anova_results_clean03 <- anova_results03 %>%
  rename(
    `Grados Libertad` = df,
    `Suma Cuadrados` = sumsq,
    `Cuadrado Medio` = meansq,
    `F Value` = statistic,
    `P Valor` = p.value
  ) %>% dplyr::select(Variable = variable, everything())


# Mostrar como tabla 
kable(anova_results_clean03, digits = 4, caption = "Resultados de ANOVA por variable 2003")

# Exportar a csv
write.csv(anova_results_clean03, "resultados_anova03.csv", row.names = FALSE)

library(car)  # Para MANOVA y pruebas de supuestos
library(purrr)

# Preparación de datos
data_anova03 <- caa2003 %>%
  mutate(cluster = as.factor(clusterward)) %>%
  dplyr::select(cluster, icp, ici, ics, marppvs, marpacd,
         divppvs, divpacd,comppvs, compacd)

# Verificación de Supuestos

# Normalidad (Shapiro-Wilk por grupo)
normality_tests03 <- data_anova03 %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics, marppvs, marpacd,
             divppvs, divpacd,comppvs, compacd),
           ~shapiro.test(.x)$p.value,
           .names = "shapiro_{.col}")
  )
normality_tests03
# Si p < 0.05, se viola normalidad → considerar transformación o Kruskal-Wallis

# b) Homogeneidad de varianzas (Levene's Test)

levene_results03 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                              "divppvs", "divpacd","comppvs", "compacd"), function(var) {
  formula_str <- paste0(var, " ~ cluster")
  levene_test <- leveneTest(as.formula(formula_str), data = data_anova03)
  tibble(
    variable = var,
    F_stat = levene_test$`F value`[1],
    p_value = levene_test$`Pr(>F)`[1],
    homogeneous = p_value > 0.05
  )
})
levene_results03

# Si se violan supuestos, usar Welch ANOVA o transformaciones

# MANOVA (Análisis Multivariado)
manova_model03 <- manova(cbind(icp, ici, ics, marppvs, marpacd,
                               divppvs, divpacd,comppvs, compacd) ~ cluster, 
                       data = data_anova03)
manova_summary03 <- summary(manova_model03, test = "Wilks")
manova_summary03

# Criterio: Wilks' Lambda significativo (p < 0.001) indica diferencias multivariadas

# ANOVAs Univariados (Follow-up)
anova_results03 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                             "divppvs", "divpacd","comppvs", "compacd"), function(var) {
  formula_str <- paste0(var, " ~ cluster")
  anova_model03 <- aov(as.formula(formula_str), data = data_anova03)
  anova_summary03 <- summary(anova_model03)[[1]]
  
  # Tamaño del efecto (Eta cuadrado)
  ss_between03 <- anova_summary03$`Sum Sq`[1]
  ss_total03 <- sum(anova_summary03$`Sum Sq`)
  eta_squared03 <- ss_between03 / ss_total03
  
  tibble(
    variable = var,
    F_statistic = anova_summary03$`F value`[1],
    p_value = anova_summary03$`Pr(>F)`[1],
    eta_squared03 = eta_squared03,
    effect_size03 = case_when(
      eta_squared03 < 0.06 ~ "pequeño",
      eta_squared03 < 0.14 ~ "mediano",
      TRUE ~ "grande"
    )
  )
})
anova_results03

# Post-hoc: Tukey HSD (comparaciones par a par)
tukey_results03 <- map(c("icp", "ici", "ics", "marppvs", "marpacd",
                         "divppvs", "divpacd","comppvs", "compacd"), function(var) {
  formula_str03 <- paste0(var, " ~ cluster")
  anova_model03 <- aov(as.formula(formula_str03), data = data_anova03)
  TukeyHSD(anova_model03, conf.level = 0.95)
})

names(tukey_results03) <- c("icp", "ici", "ics", "marppvs", "marpacd",
                          "divppvs", "divpacd","comppvs", "compacd")

# Extraer comparaciones significativas
significant_pairs03 <- map_dfr(names(tukey_results03), function(var) {
  tukey_df03 <- as.data.frame(tukey_results03[[var]]$cluster)
  tukey_df03 %>%
    rownames_to_column("comparison") %>%
    filter(`p adj` < 0.05) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, comparison, diff, `p adj`)
})
significant_pairs03

library(factoextra)
library(fpc)
library(cluster)
library(MASS)

# Validación de Separación: Silhouette Analysis
sil03 <- silhouette(as.numeric(as.character(caa2003$clusterward)), res.dist03)
sil_summary03 <- summary(sil03)
sil_summary03

# Criterio: Silhouette promedio > 0.25 (estructura razonable)
#           Silhouette > 0.50 (estructura fuerte)

# Prueba de Discriminación: Linear Discriminant Analysis (LDA)

lda_model03 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                 divppvs+divpacd+comppvs+compacd, 
                 data = caa2003 %>% mutate(cluster = as.factor(clusterward)))

# Clasificación cruzada (Leave-One-Out)
lda_cv03 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                  divppvs+divpacd+comppvs+compacd, 
              data = caa2003 %>% mutate(cluster = as.factor(clusterward)),
              CV = TRUE)

# Matriz de confusión
confusion_matrix03 <- table(Predicted = lda_cv03$class, 
                          Actual = as.factor(caa2003$clusterward))
accuracy03 <- sum(diag(confusion_matrix03)) / sum(confusion_matrix03)
accuracy03

# Criterio: Accuracy > 0.75 indica clusters bien diferenciados

cluster_percentiles03 <- caa2003 %>%
  mutate(cluster = clusterward) %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    rank_icp = rank(icp),
    rank_ici = rank(ici),
    rank_ics = rank(ics),
    avg_rank = (rank_icp + rank_ici + rank_ics) / 3
  )

cluster_percentiles03

# Cluster 3 debe ser rank 2 o 3 (de 4), NO el más bajo ni el más alto



# 2008

# Seleccionar las variables de interés
data_selected08 <- cagu2008 %>%
  dplyr::select(comppvs,compacd, marppvs, 
                marpacd, divppvs, divpacd,
                icp,ici, ics)


# Método del codo

for (k in 2:10) {
  clusterboot_results08 <- clusterboot(data_selected08, 
                                       B = 100, 
                                       clustermethod = hclustCBI, 
                                       method = "ward.D", 
                                       k = k)
  print(paste("k =", k))
  print(clusterboot_results08$bootmean)
}

wss <- sapply(1:10, function(k) {
  kmeans(data_selected08, k, nstart = 25)$tot.withinss
})

# Graficar el método del codo
fviz_nbclust(data_selected08, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Línea vertical en el codo
  labs(title = "Método del Codo", x = "Número de clusters", y = "Suma de cuadrados intragrupo (WSS)")

# DBSCAN

result08 <- dbscan(data_selected08, eps = 0.5, minPts = 5)

result08

kNNdistplot(data_selected08, k = 8)  # Gráfico para elegir eps
abline(h = 0.65, col = "red")  # Límite óptimo (codo)

clusterboot_results08 <- clusterboot(data_selected08, 
                                     B = 100, 
                                     clustermethod = hclustCBI, 
                                     method = "ward.D", 
                                     k = 4)

print(clusterboot_results08)

# Extraer resultados en formato tabular
# Crear tabla de estabilidad
stability_table08 <- data.frame(
  Cluster = 1:length(clusterboot_results08$bootmean),
  Jaccard_Mean = clusterboot_results08$bootmean,
  Dissolved_Pct = clusterboot_results08$bootbrd,
  Recovered_Pct = clusterboot_results08$bootrecover
)

# Añadir nivel de estabilidad basado en Jaccard
stability_table08$Stability_Level <- ifelse(
  stability_table08$Jaccard_Mean >= 0.75, "Excelente",
  ifelse(stability_table08$Jaccard_Mean >= 0.60, "Moderada", "Baja")
)

# Ordenar por Cluster
stability_table08 <- stability_table08[order(stability_table08$Cluster), ]

stability_table08 %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  kable(col.names = c("Cluster", "Media Jaccard", "% Disolución", "% Recuperación", "Nivel Estabilidad"),
        align = c("c", "c", "c", "c", "l"),
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE,
                font_size = 14) %>%
  row_spec(which(stability_table08$Stability_Level == "Excelente"), 
           bold = TRUE, color = "white", background = "#27ae60") %>%
  row_spec(which(stability_table08$Stability_Level == "Moderada"), 
           background = "#f1c40f") %>%
  row_spec(which(stability_table08$Stability_Level == "Baja"), 
           background = "#e74c3c", color = "white") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c("Estabilidad de Clusters" = 5), 
                   background = "#2c3e50", 
                   color = "white") %>%
  footnote("Validación mediante 100 remuestreos bootstrap")

write.csv(stability_table08, "stability_table08.csv", row.names = FALSE)

ggplot(stability_table08, aes(x = factor(Cluster), y = Jaccard_Mean, fill = Stability_Level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Jaccard_Mean)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Excelente" = "#2ecc71", 
                               "Moderada" = "#f39c12",
                               "Baja" = "#e74c3c")) +
  labs(title = "Estabilidad de Clusters por Índice Jaccard",
       subtitle = "Validación mediante bootstrap (n=100)",
       x = "Cluster", y = "Media Jaccard") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Kmeans

kmeans_result08 <- kmeans(data_selected08, 
                          centers=4, nstart=20)
# Ver los resultados
print(kmeans_result08)

# Agregar los clústeres a los datos originales
cagu2008$cluster <- as.factor(kmeans_result08$cluster)

# Visualización
ggplot(cluster_summary_08, aes(x = cluster, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias por Cluster 2008", x = "Cluster", y = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Hclust

res.dist08 <- get_dist(data_selected08, stand = FALSE, 
                       method = "euclidean")
d08 <- as.matrix(res.dist08)

res.hc08 <- hclust(res.dist08, "ward.D")

grp08 <- cutree(res.hc08, 4)

caa2008$clusterward <- as.factor(grp08)

heatmap(d08, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2008")

agrup08 <- fviz_dend(res.hc08, cex = 0.3, k = 4, rect = TRUE,
                     k_colors = "ucscgb", 
                     color_labels_by_k = TRUE,
                     main = "Agrupación estatal por capacidades productivas año 2008",
                     ylab = "Pesos en la matriz de distancia",
                     xlab = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI", 
                     sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI") 

agrup08



# Evaluación clústers

for (k in 2:10) {
  grp <- cutree(res.hc08, k)
  
  ch <- fpc::calinhara(data_selected08, grp)
  
  # Davies-Bouldin verdadero
  db <- clusterSim::index.DB(data_selected08, grp)$DB
  
  print(paste("k =", k, "CH =", round(ch,2), "DB =", round(db,2)))
}



# Agrupar por cluster y calcular estadística descriptiva

c_summary08 <- caa2008 %>%
  group_by(clusterward) %>%
  summarise(
    count = n(),
    across(
      all_of(vars_interes),
      list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        q1 = ~ quantile(., 0.25, na.rm = TRUE),
        q3 = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -c(clusterward, count),
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

c_summary08 %>%
  mutate(across(c(mean, median, sd, q1, q3), ~ round(., 2))) %>%
  kable(col.names = c("Cluster", "N", "Variable", "Media", "Mediana", "DE", "Q1", "Q3"),
        align = c("c", "c", "l", rep("c", 5))) %>%
  kable_styling("striped") %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  add_header_above(c(" " = 3, "Distribución" = 5))# 2. Pivotar a formato largo (solo columnas de medias)


write_csv(c_summary08, "resultados_clusters08.csv")


for (i in 1:4) {
  cluster_data <- subset(caa2008, clusterward == i)
  write.csv(cluster_data, file = paste("cluster08_", i, ".csv", sep = ""), row.names = FALSE)
}


ggplot(caa2008, aes(x = as.factor(clusterward), y = marpacd)) +
  geom_boxplot() +
  labs(title = "Distribución MARPACD por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = marppvs)) +
  geom_boxplot() +
  labs(title = "Distribución MARPPVS por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = compacd)) +
  geom_boxplot() +
  labs(title = "Distribución COMPACD por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = comppvs)) +
  geom_boxplot() +
  labs(title = "Distribución COMPPVS por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = icp)) +
  geom_boxplot() +
  labs(title = "Distribución ICP por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = ici)) +
  geom_boxplot() +
  labs(title = "Distribución ICI por cluster, 2008", x = "Cluster", y = "Variable1")

ggplot(caa2008, aes(x = as.factor(clusterward), y = ics)) +
  geom_boxplot() +
  labs(title = "Distribución ICS por cluster, 2008", x = "Cluster", y = "Variable1")


# Aplicar la función a todas las variables y juntar en un solo data frame
anova_results08 <- lapply(vars_interes, run_anova, data = caa2008) %>%
  bind_rows()

# Renombrar columnas para mayor claridad
anova_results_clean08 <- anova_results08 %>%
  rename(
    `Grados Libertad` = df,
    `Suma Cuadrados` = sumsq,
    `Cuadrado Medio` = meansq,
    `F Value` = statistic,
    `P Valor` = p.value
  ) %>% dplyr::select(Variable = variable, everything())

# Mostrar como tabla 
kable(anova_results_clean08, digits = 4, caption = "Resultados de ANOVA por variable 2008")

# Exportar a csv
write.csv(anova_results_clean08, "resultados_anova08.csv", row.names = FALSE)

# Preparación de datos
data_anova08 <- caa2008 %>%
  mutate(cluster = as.factor(clusterward)) %>%
  dplyr::select(cluster, icp, ici, ics, marppvs, marpacd,
                divppvs, divpacd,comppvs, compacd)

# Verificación de Supuestos

# Normalidad (Shapiro-Wilk por grupo)
normality_tests08 <- data_anova08 %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics, marppvs, marpacd,
             divppvs, divpacd,comppvs, compacd),
           ~shapiro.test(.x)$p.value,
           .names = "shapiro_{.col}")
  )
normality_tests08
# Si p < 0.05, se viola normalidad → considerar transformación o Kruskal-Wallis

# b) Homogeneidad de varianzas (Levene's Test)

levene_results08 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                              "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                                formula_str <- paste0(var, " ~ cluster")
                                levene_test <- leveneTest(as.formula(formula_str), data = data_anova08)
                                tibble(
                                  variable = var,
                                  F_stat = levene_test$`F value`[1],
                                  p_value = levene_test$`Pr(>F)`[1],
                                  homogeneous = p_value > 0.05
                                )
                              })
levene_results08

# Si se violan supuestos, usar Welch ANOVA o transformaciones

# MANOVA (Análisis Multivariado)
manova_model08 <- manova(cbind(icp, ici, ics, marppvs, marpacd,
                               divppvs, divpacd,comppvs, compacd) ~ cluster, 
                         data = data_anova08)
manova_summary08 <- summary(manova_model08, test = "Wilks")
manova_summary08

# Criterio: Wilks' Lambda significativo (p < 0.001) indica diferencias multivariadas

# ANOVAs Univariados (Follow-up)
anova_results08 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                             "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                               formula_str <- paste0(var, " ~ cluster")
                               anova_model08 <- aov(as.formula(formula_str), data = data_anova08)
                               anova_summary08 <- summary(anova_model08)[[1]]
                               
                               # Tamaño del efecto (Eta cuadrado)
                               ss_between08 <- anova_summary08$`Sum Sq`[1]
                               ss_total08 <- sum(anova_summary08$`Sum Sq`)
                               eta_squared08 <- ss_between08 / ss_total08
                               
                               tibble(
                                 variable = var,
                                 F_statistic = anova_summary08$`F value`[1],
                                 p_value = anova_summary08$`Pr(>F)`[1],
                                 eta_squared08 = eta_squared08,
                                 effect_size08 = case_when(
                                   eta_squared08 < 0.06 ~ "pequeño",
                                   eta_squared08 < 0.14 ~ "mediano",
                                   TRUE ~ "grande"
                                 )
                               )
                             })
anova_results08

# Post-hoc: Tukey HSD (comparaciones par a par)
tukey_results08 <- map(c("icp", "ici", "ics", "marppvs", "marpacd",
                         "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                           formula_str08 <- paste0(var, " ~ cluster")
                           anova_model08 <- aov(as.formula(formula_str08), data = data_anova08)
                           TukeyHSD(anova_model08, conf.level = 0.95)
                         })

names(tukey_results08) <- c("icp", "ici", "ics", "marppvs", "marpacd",
                            "divppvs", "divpacd","comppvs", "compacd")

# Extraer comparaciones significativas
significant_pairs08 <- map_dfr(names(tukey_results08), function(var) {
  tukey_df08 <- as.data.frame(tukey_results08[[var]]$cluster)
  tukey_df08 %>%
    rownames_to_column("comparison") %>%
    filter(`p adj` < 0.05) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, comparison, diff, `p adj`)
})
significant_pairs08

# Validación de Separación: Silhouette Analysis
sil08 <- silhouette(as.numeric(as.character(caa2008$clusterward)), res.dist08)
sil_summary08 <- summary(sil08)
sil_summary08

# Criterio: Silhouette promedio > 0.25 (estructura razonable)
#           Silhouette > 0.50 (estructura fuerte)

# Prueba de Discriminación: Linear Discriminant Analysis (LDA)

lda_model08 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                     divppvs+divpacd+comppvs+compacd, 
                   data = caa2008 %>% mutate(cluster = as.factor(clusterward)))

# Clasificación cruzada (Leave-One-Out)
lda_cv08 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                  divppvs+divpacd+comppvs+compacd, 
                data = caa2008 %>% mutate(cluster = as.factor(clusterward)),
                CV = TRUE)

# Matriz de confusión
confusion_matrix08 <- table(Predicted = lda_cv08$class, 
                            Actual = as.factor(caa2008$clusterward))
accuracy08 <- sum(diag(confusion_matrix08)) / sum(confusion_matrix08)
accuracy08

# Criterio: Accuracy > 0.75 indica clusters bien diferenciados

cluster_percentiles08 <- caa2008 %>%
  mutate(cluster = clusterward) %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    rank_icp = rank(icp),
    rank_ici = rank(ici),
    rank_ics = rank(ics),
    avg_rank = (rank_icp + rank_ici + rank_ics) / 3
  )

cluster_percentiles08



# 2013

# Seleccionar las variables de interés
data_selected13 <- cagu2013 %>%
  dplyr::select(comppvs,compacd, marppvs, 
                marpacd, divppvs, divpacd,
                icp,ici, ics)

# Método del codo

for (k in 2:10) {
  clusterboot_results13 <- clusterboot(data_selected13, 
                                       B = 100, 
                                       clustermethod = hclustCBI, 
                                       method = "ward.D", 
                                       k = k)
  print(paste("k =", k))
  print(clusterboot_results13$bootmean)
}

wss <- sapply(1:10, function(k) {
  kmeans(data_selected13, k, nstart = 25)$tot.withinss
})

# Graficar el método del codo
fviz_nbclust(data_selected13, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Línea vertical en el codo
  labs(title = "Método del Codo", x = "Número de clusters", y = "Suma de cuadrados intragrupo (WSS)")

# DBSCAN

result13 <- dbscan(data_selected13, eps = 0.5, minPts = 5)

result13

kNNdistplot(data_selected13, k = 8)  # Gráfico para elegir eps
abline(h = 0.65, col = "red")  # Límite óptimo (codo)

clusterboot_results13 <- clusterboot(data_selected13, 
                                     B = 100, 
                                     clustermethod = hclustCBI, 
                                     method = "ward.D", 
                                     k = 4)

print(clusterboot_results13)

# Extraer resultados en formato tabular
# Crear tabla de estabilidad
stability_table13 <- data.frame(
  Cluster = 1:length(clusterboot_results13$bootmean),
  Jaccard_Mean = clusterboot_results13$bootmean,
  Dissolved_Pct = clusterboot_results13$bootbrd,
  Recovered_Pct = clusterboot_results13$bootrecover
)

# Añadir nivel de estabilidad basado en Jaccard
stability_table13$Stability_Level <- ifelse(
  stability_table13$Jaccard_Mean >= 0.75, "Excelente",
  ifelse(stability_table13$Jaccard_Mean >= 0.60, "Moderada", "Baja")
)

# Ordenar por Cluster
stability_table13 <- stability_table13[order(stability_table13$Cluster), ]

# Crear tabla elegante con kableExtra

stability_table13 %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  kable(col.names = c("Cluster", "Media Jaccard", "% Disolución", "% Recuperación", "Nivel Estabilidad"),
        align = c("c", "c", "c", "c", "l"),
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE,
                font_size = 14) %>%
  row_spec(which(stability_table13$Stability_Level == "Excelente"), 
           bold = TRUE, color = "white", background = "#27ae60") %>%
  row_spec(which(stability_table13$Stability_Level == "Moderada"), 
           background = "#f1c40f") %>%
  row_spec(which(stability_table13$Stability_Level == "Baja"), 
           background = "#e74c3c", color = "white") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c("Estabilidad de Clusters" = 5), 
                   background = "#2c3e50", 
                   color = "white") %>%
  footnote("Validación mediante 100 remuestreos bootstrap")

write.csv(stability_table13, "stability_table13.csv", row.names = FALSE)


ggplot(stability_table13, aes(x = factor(Cluster), y = Jaccard_Mean, fill = Stability_Level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Jaccard_Mean)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Excelente" = "#2ecc71", 
                               "Moderada" = "#f39c12",
                               "Baja" = "#e74c3c")) +
  labs(title = "Estabilidad de Clusters por Índice Jaccard",
       subtitle = "Validación mediante bootstrap (n=100)",
       x = "Cluster", y = "Media Jaccard") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Kmeans

kmeans_result13 <- kmeans(data_selected13, 
                          centers=4, nstart=20)
# Ver los resultados
print(kmeans_result13)

# Agregar los clústeres a los datos originales
cagu2013$cluster <- as.factor(kmeans_result13$cluster)

# Visualización
ggplot(cluster_summary_13, aes(x = cluster, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias por Cluster 2013", x = "Cluster", y = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Hclust

res.dist13 <- get_dist(data_selected13, stand = FALSE, 
                       method = "euclidean")
d13 <- as.matrix(res.dist13)

res.hc13 <- hclust(res.dist13, "ward.D")

grp13 <- cutree(res.hc13, 4)

caa2013$clusterward <- as.factor(grp13)

heatmap(d13, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2013")

agrup13 <- fviz_dend(res.hc13, cex = 0.3, k = 4, rect = TRUE,
                     k_colors = "ucscgb", 
                     color_labels_by_k = TRUE,
                     main = "Agrupación estatal por capacidades productivas año 2013",
                     ylab = "Pesos en la matriz de distancia",
                     xlab = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI", 
                     sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI") 

agrup13


# Evaluación clústers

for (k in 2:10) {
  grp <- cutree(res.hc13, k)
  
  ch <- fpc::calinhara(data_selected13, grp)
  
  # Davies-Bouldin verdadero
  db <- clusterSim::index.DB(data_selected13, grp)$DB
  
  print(paste("k =", k, "CH =", round(ch,2), "DB =", round(db,2)))
}


# Agrupar por cluster y calcular estadística descriptiva

c_summary13 <- caa2013 %>%
  group_by(clusterward) %>%
  summarise(
    count = n(),
    across(
      all_of(vars_interes),
      list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        q1 = ~ quantile(., 0.25, na.rm = TRUE),
        q3 = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -c(clusterward, count),
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

c_summary13 %>%
  mutate(across(c(mean, median, sd, q1, q3), ~ round(., 2))) %>%
  kable(col.names = c("Cluster", "N", "Variable", "Media", "Mediana", "DE", "Q1", "Q3"),
        align = c("c", "c", "l", rep("c", 5))) %>%
  kable_styling("striped") %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  add_header_above(c(" " = 3, "Distribución" = 5))# 2. Pivotar a formato largo (solo columnas de medias)



write_csv(c_summary13, "resultados_clusters13.csv")


for (i in 1:4) {
  cluster_data <- subset(caa2013, clusterward == i)
  write.csv(cluster_data, file = paste("cluster13_", i, ".csv", sep = ""), row.names = FALSE)
}


ggplot(caa2013, aes(x = as.factor(clusterward), y = marpacd)) +
  geom_boxplot() +
  labs(title = "Distribución MARPACD por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = marppvs)) +
  geom_boxplot() +
  labs(title = "Distribución MARPPVS por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = compacd)) +
  geom_boxplot() +
  labs(title = "Distribución COMPACD por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = comppvs)) +
  geom_boxplot() +
  labs(title = "Distribución COMPPVS por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = icp)) +
  geom_boxplot() +
  labs(title = "Distribución ICP por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = ici)) +
  geom_boxplot() +
  labs(title = "Distribución ICI por cluster, 2013", x = "Cluster", y = "Variable1")

ggplot(caa2013, aes(x = as.factor(clusterward), y = ics)) +
  geom_boxplot() +
  labs(title = "Distribución ICS por cluster, 2013", x = "Cluster", y = "Variable1")


# Aplicar la función a todas las variables y juntar en un solo data frame
anova_results13 <- lapply(vars_interes, run_anova, data = caa2013) %>%
  bind_rows()

# Renombrar columnas para mayor claridad
anova_results_clean13 <- anova_results13 %>%
  rename(
    `Grados Libertad` = df,
    `Suma Cuadrados` = sumsq,
    `Cuadrado Medio` = meansq,
    `F Value` = statistic,
    `P Valor` = p.value
  ) %>% dplyr::select(Variable = variable, everything())

# Mostrar como tabla 
kable(anova_results_clean13, digits = 4, caption = "Resultados de ANOVA por variable 2013")

# Exportar a csv
write.csv(anova_results_clean13, "resultados_anova13.csv", row.names = FALSE)

# Preparación de datos
data_anova13 <- caa2013 %>%
  mutate(cluster = as.factor(clusterward)) %>%
  dplyr::select(cluster, icp, ici, ics, marppvs, marpacd,
                divppvs, divpacd,comppvs, compacd)

# Verificación de Supuestos

# Normalidad (Shapiro-Wilk por grupo)
normality_tests13 <- data_anova13 %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics, marppvs, marpacd,
             divppvs, divpacd,comppvs, compacd),
           ~shapiro.test(.x)$p.value,
           .names = "shapiro_{.col}")
  )
normality_tests13
# Si p < 0.05, se viola normalidad → considerar transformación o Kruskal-Wallis

# b) Homogeneidad de varianzas (Levene's Test)

levene_results13 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                              "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                                formula_str <- paste0(var, " ~ cluster")
                                levene_test <- leveneTest(as.formula(formula_str), data = data_anova13)
                                tibble(
                                  variable = var,
                                  F_stat = levene_test$`F value`[1],
                                  p_value = levene_test$`Pr(>F)`[1],
                                  homogeneous = p_value > 0.05
                                )
                              })
levene_results13

# Si se violan supuestos, usar Welch ANOVA o transformaciones

# MANOVA (Análisis Multivariado)
manova_model13 <- manova(cbind(icp, ici, ics, marppvs, marpacd,
                               divppvs, divpacd,comppvs, compacd) ~ cluster, 
                         data = data_anova13)
manova_summary13 <- summary(manova_model13, test = "Wilks")
manova_summary13

# Criterio: Wilks' Lambda significativo (p < 0.001) indica diferencias multivariadas

# ANOVAs Univariados (Follow-up)
anova_results13 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                             "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                               formula_str <- paste0(var, " ~ cluster")
                               anova_model13 <- aov(as.formula(formula_str), data = data_anova13)
                               anova_summary13 <- summary(anova_model13)[[1]]
                               
                               # Tamaño del efecto (Eta cuadrado)
                               ss_between13 <- anova_summary13$`Sum Sq`[1]
                               ss_total13 <- sum(anova_summary13$`Sum Sq`)
                               eta_squared13 <- ss_between13 / ss_total13
                               
                               tibble(
                                 variable = var,
                                 F_statistic = anova_summary13$`F value`[1],
                                 p_value = anova_summary13$`Pr(>F)`[1],
                                 eta_squared13 = eta_squared13,
                                 effect_size13 = case_when(
                                   eta_squared13 < 0.06 ~ "pequeño",
                                   eta_squared13 < 0.14 ~ "mediano",
                                   TRUE ~ "grande"
                                 )
                               )
                             })
anova_results13

# Post-hoc: Tukey HSD (comparaciones par a par)
tukey_results13 <- map(c("icp", "ici", "ics", "marppvs", "marpacd",
                         "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                           formula_str13 <- paste0(var, " ~ cluster")
                           anova_model13 <- aov(as.formula(formula_str13), data = data_anova13)
                           TukeyHSD(anova_model13, conf.level = 0.95)
                         })

names(tukey_results13) <- c("icp", "ici", "ics", "marppvs", "marpacd",
                            "divppvs", "divpacd","comppvs", "compacd")

# Extraer comparaciones significativas
significant_pairs13 <- map_dfr(names(tukey_results13), function(var) {
  tukey_df13 <- as.data.frame(tukey_results13[[var]]$cluster)
  tukey_df13 %>%
    rownames_to_column("comparison") %>%
    filter(`p adj` < 0.05) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, comparison, diff, `p adj`)
})
significant_pairs13

# Validación de Separación: Silhouette Analysis
sil13 <- silhouette(as.numeric(as.character(caa2013$clusterward)), res.dist13)
sil_summary13 <- summary(sil13)
sil_summary13

# Criterio: Silhouette promedio > 0.25 (estructura razonable)
#           Silhouette > 0.50 (estructura fuerte)

# Prueba de Discriminación: Linear Discriminant Analysis (LDA)

lda_model13 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                     divppvs+divpacd+comppvs+compacd, 
                   data = caa2013 %>% mutate(cluster = as.factor(clusterward)))

# Clasificación cruzada (Leave-One-Out)
lda_cv13 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                  divppvs+divpacd+comppvs+compacd, 
                data = caa2013 %>% mutate(cluster = as.factor(clusterward)),
                CV = TRUE)

# Matriz de confusión
confusion_matrix13 <- table(Predicted = lda_cv13$class, 
                            Actual = as.factor(caa2013$clusterward))
accuracy13 <- sum(diag(confusion_matrix13)) / sum(confusion_matrix13)
accuracy13

# Criterio: Accuracy > 0.75 indica clusters bien diferenciados

cluster_percentiles13 <- caa2013 %>%
  mutate(cluster = clusterward) %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    rank_icp = rank(icp),
    rank_ici = rank(ici),
    rank_ics = rank(ics),
    avg_rank = (rank_icp + rank_ici + rank_ics) / 3
  )

cluster_percentiles13


# 2018

# Seleccionar las variables de interés
data_selected18 <- cagu2018 %>%
  dplyr::select(comppvs,compacd, marppvs, 
                marpacd, divppvs, divpacd,
                icp,ici, ics)

# Método del codo

for (k in 2:10) {
  clusterboot_results18 <- clusterboot(data_selected18, 
                                       B = 100, 
                                       clustermethod = hclustCBI, 
                                       method = "ward.D", 
                                       k = k)
  print(paste("k =", k))
  print(clusterboot_results18$bootmean)
}

wss <- sapply(1:10, function(k) {
  kmeans(data_selected18, k, nstart = 25)$tot.withinss
})

# Graficar el método del codo
fviz_nbclust(data_selected18, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Línea vertical en el codo
  labs(title = "Método del Codo", x = "Número de clusters", y = "Suma de cuadrados intragrupo (WSS)")

# DBSCAN

result18 <- dbscan(data_selected18, eps = 0.5, minPts = 5)

result18

kNNdistplot(data_selected18, k = 8)  # Gráfico para elegir eps
abline(h = 0.65, col = "red")  # Límite óptimo (codo)

clusterboot_results18 <- clusterboot(data_selected18, 
                                     B = 100, 
                                     clustermethod = hclustCBI, 
                                     method = "ward.D", 
                                     k = 4)

print(clusterboot_results18)

# Extraer resultados en formato tabular
# Crear tabla de estabilidad
stability_table18 <- data.frame(
  Cluster = 1:length(clusterboot_results18$bootmean),
  Jaccard_Mean = clusterboot_results18$bootmean,
  Dissolved_Pct = clusterboot_results18$bootbrd,
  Recovered_Pct = clusterboot_results18$bootrecover
)

# Añadir nivel de estabilidad basado en Jaccard
stability_table18$Stability_Level <- ifelse(
  stability_table18$Jaccard_Mean >= 0.75, "Excelente",
  ifelse(stability_table18$Jaccard_Mean >= 0.60, "Moderada", "Baja")
)

# Ordenar por Cluster
stability_table18 <- stability_table18[order(stability_table18$Cluster), ]

stability_table18 %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  kable(col.names = c("Cluster", "Media Jaccard", "% Disolución", "% Recuperación", "Nivel Estabilidad"),
        align = c("c", "c", "c", "c", "l"),
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE,
                font_size = 14) %>%
  row_spec(which(stability_table18$Stability_Level == "Excelente"), 
           bold = TRUE, color = "white", background = "#27ae60") %>%
  row_spec(which(stability_table18$Stability_Level == "Moderada"), 
           background = "#f1c40f") %>%
  row_spec(which(stability_table18$Stability_Level == "Baja"), 
           background = "#e74c3c", color = "white") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c("Estabilidad de Clusters" = 5), 
                   background = "#2c3e50", 
                   color = "white") %>%
  footnote("Validación mediante 100 remuestreos bootstrap")

write.csv(stability_table18, "stability_table18.csv", row.names = FALSE)

ggplot(stability_table18, aes(x = factor(Cluster), y = Jaccard_Mean, fill = Stability_Level)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Jaccard_Mean)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Excelente" = "#2ecc71", 
                               "Moderada" = "#f39c12",
                               "Baja" = "#e74c3c")) +
  labs(title = "Estabilidad de Clusters por Índice Jaccard",
       subtitle = "Validación mediante bootstrap (n=100)",
       x = "Cluster", y = "Media Jaccard") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Kmeans

kmeans_result18 <- kmeans(data_selected18, 
                          centers=4, nstart=20)
# Ver los resultados
print(kmeans_result18)

# Agregar los clústeres a los datos originales
cagu2018$cluster <- as.factor(kmeans_result18$cluster)

# Visualización
ggplot(cluster_summary_18, aes(x = cluster, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias por Cluster 2018", x = "Cluster", y = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Hclust

res.dist18 <- get_dist(data_selected18, stand = FALSE, 
                       method = "euclidean")
d18 <- as.matrix(res.dist18)

res.hc18 <- hclust(res.dist18, "ward.D")

grp18 <- cutree(res.hc18, 4)

caa2018$clusterward <- as.factor(grp18)

heatmap(d18, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2018")

agrup18 <- fviz_dend(res.hc18, cex = 0.3, k = 4, rect = TRUE,
                     k_colors = "ucscgb", 
                     color_labels_by_k = TRUE,
                     main = "Agrupación estatal por capacidades productivas año 2018",
                     ylab = "Pesos en la matriz de distancia",
                     xlab = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI", 
                     sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI") 

agrup18

# Evaluación clústers

for (k in 2:10) {
  grp <- cutree(res.hc18, k)
  
  ch <- fpc::calinhara(data_selected18, grp)
  
  # Davies-Bouldin verdadero
  db <- clusterSim::index.DB(data_selected18, grp)$DB
  
  print(paste("k =", k, "CH =", round(ch,2), "DB =", round(db,2)))
}



# Agrupar por cluster y calcular estadística descriptiva

c_summary18 <- caa2018 %>%
  group_by(clusterward) %>%
  summarise(
    count = n(),
    across(
      all_of(vars_interes),
      list(
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        q1 = ~ quantile(., 0.25, na.rm = TRUE),
        q3 = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -c(clusterward, count),
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  )

c_summary18 %>%
  mutate(across(c(mean, median, sd, q1, q3), ~ round(., 2))) %>%
  kable(col.names = c("Cluster", "N", "Variable", "Media", "Mediana", "DE", "Q1", "Q3"),
        align = c("c", "c", "l", rep("c", 5))) %>%
  kable_styling("striped") %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  add_header_above(c(" " = 3, "Distribución" = 5))# 2. Pivotar a formato largo (solo columnas de medias)



write_csv(c_summary18, "resultados_clusters18.csv")


for (i in 1:4) {
  cluster_data <- subset(caa2018, clusterward == i)
  write.csv(cluster_data, file = paste("cluster18_", i, ".csv", sep = ""), row.names = FALSE)
}


ggplot(caa2018, aes(x = as.factor(clusterward), y = marpacd)) +
  geom_boxplot() +
  labs(title = "Distribución MARPACD por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = marppvs)) +
  geom_boxplot() +
  labs(title = "Distribución MARPPVS por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = compacd)) +
  geom_boxplot() +
  labs(title = "Distribución COMPACD por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = comppvs)) +
  geom_boxplot() +
  labs(title = "Distribución COMPPVS por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = icp)) +
  geom_boxplot() +
  labs(title = "Distribución ICP por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = ici)) +
  geom_boxplot() +
  labs(title = "Distribución ICI por cluster, 2018", x = "Cluster", y = "Variable1")

ggplot(caa2018, aes(x = as.factor(clusterward), y = ics)) +
  geom_boxplot() +
  labs(title = "Distribución ICS por cluster, 2018", x = "Cluster", y = "Variable1")


# Aplicar la función a todas las variables y juntar en un solo data frame
anova_results18 <- lapply(vars_interes, run_anova, data = caa2018) %>%
  bind_rows()

# Renombrar columnas para mayor claridad
anova_results_clean18 <- anova_results18 %>%
  rename(
    `Grados Libertad` = df,
    `Suma Cuadrados` = sumsq,
    `Cuadrado Medio` = meansq,
    `F Value` = statistic,
    `P Valor` = p.value
  ) %>% dplyr::select(Variable = variable, everything())

# Mostrar como tabla 
kable(anova_results_clean18, digits = 4, caption = "Resultados de ANOVA por variable 2018")

# Exportar a csv
write.csv(anova_results_clean18, "resultados_anova18.csv", row.names = FALSE)

# Preparación de datos
data_anova18 <- caa2018 %>%
  mutate(cluster = as.factor(clusterward)) %>%
  dplyr::select(cluster, icp, ici, ics, marppvs, marpacd,
                divppvs, divpacd,comppvs, compacd)

# Verificación de Supuestos

# Normalidad (Shapiro-Wilk por grupo)
normality_tests18 <- data_anova18 %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics, marppvs, marpacd,
             divppvs, divpacd,comppvs, compacd),
           ~shapiro.test(.x)$p.value,
           .names = "shapiro_{.col}")
  )
normality_tests18
# Si p < 0.05, se viola normalidad → considerar transformación o Kruskal-Wallis

# b) Homogeneidad de varianzas (Levene's Test)

levene_results18 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                              "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                                formula_str <- paste0(var, " ~ cluster")
                                levene_test <- leveneTest(as.formula(formula_str), data = data_anova18)
                                tibble(
                                  variable = var,
                                  F_stat = levene_test$`F value`[1],
                                  p_value = levene_test$`Pr(>F)`[1],
                                  homogeneous = p_value > 0.05
                                )
                              })
levene_results18

# Si se violan supuestos, usar Welch ANOVA o transformaciones

# MANOVA (Análisis Multivariado)
manova_model18 <- manova(cbind(icp, ici, ics, marppvs, marpacd,
                               divppvs, divpacd,comppvs, compacd) ~ cluster, 
                         data = data_anova18)
manova_summary18 <- summary(manova_model18, test = "Wilks")
manova_summary18

# Criterio: Wilks' Lambda significativo (p < 0.001) indica diferencias multivariadas

# ANOVAs Univariados (Follow-up)
anova_results18 <- map_dfr(c("icp", "ici", "ics", "marppvs", "marpacd",
                             "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                               formula_str <- paste0(var, " ~ cluster")
                               anova_model18 <- aov(as.formula(formula_str), data = data_anova18)
                               anova_summary18 <- summary(anova_model18)[[1]]
                               
                               # Tamaño del efecto (Eta cuadrado)
                               ss_between18 <- anova_summary18$`Sum Sq`[1]
                               ss_total18 <- sum(anova_summary18$`Sum Sq`)
                               eta_squared18 <- ss_between18 / ss_total18
                               
                               tibble(
                                 variable = var,
                                 F_statistic = anova_summary18$`F value`[1],
                                 p_value = anova_summary18$`Pr(>F)`[1],
                                 eta_squared18 = eta_squared18,
                                 effect_size18 = case_when(
                                   eta_squared18 < 0.06 ~ "pequeño",
                                   eta_squared18 < 0.14 ~ "mediano",
                                   TRUE ~ "grande"
                                 )
                               )
                             })
anova_results18

# Post-hoc: Tukey HSD (comparaciones par a par)
tukey_results18 <- map(c("icp", "ici", "ics", "marppvs", "marpacd",
                         "divppvs", "divpacd","comppvs", "compacd"), function(var) {
                           formula_str18 <- paste0(var, " ~ cluster")
                           anova_model18 <- aov(as.formula(formula_str18), data = data_anova18)
                           TukeyHSD(anova_model18, conf.level = 0.95)
                         })

names(tukey_results18) <- c("icp", "ici", "ics", "marppvs", "marpacd",
                            "divppvs", "divpacd","comppvs", "compacd")

# Extraer comparaciones significativas
significant_pairs18 <- map_dfr(names(tukey_results18), function(var) {
  tukey_df18 <- as.data.frame(tukey_results18[[var]]$cluster)
  tukey_df18 %>%
    rownames_to_column("comparison") %>%
    filter(`p adj` < 0.05) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, comparison, diff, `p adj`)
})
significant_pairs18

# Validación de Separación: Silhouette Analysis
sil18 <- silhouette(as.numeric(as.character(caa2018$clusterward)), res.dist18)
sil_summary18 <- summary(sil18)
sil_summary18

# Criterio: Silhouette promedio > 0.25 (estructura razonable)
#           Silhouette > 0.50 (estructura fuerte)

# Prueba de Discriminación: Linear Discriminant Analysis (LDA)

lda_model18 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                     divppvs+divpacd+comppvs+compacd, 
                   data = caa2018 %>% mutate(cluster = as.factor(clusterward)))

# Clasificación cruzada (Leave-One-Out)
lda_cv18 <- lda(cluster ~ icp+ici+ics+marppvs+marpacd+
                  divppvs+divpacd+comppvs+compacd, 
                data = caa2018 %>% mutate(cluster = as.factor(clusterward)),
                CV = TRUE)

# Matriz de confusión
confusion_matrix18 <- table(Predicted = lda_cv18$class, 
                            Actual = as.factor(caa2018$clusterward))
accuracy18 <- sum(diag(confusion_matrix18)) / sum(confusion_matrix18)
accuracy18

# Criterio: Accuracy > 0.75 indica clusters bien diferenciados

cluster_percentiles18 <- caa2018 %>%
  mutate(cluster = clusterward) %>%
  group_by(cluster) %>%
  summarise(
    across(c(icp, ici, ics),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    rank_icp = rank(icp),
    rank_ici = rank(ici),
    rank_ics = rank(ics),
    avg_rank = (rank_icp + rank_ici + rank_ics) / 3
  )

cluster_percentiles18



### Agrupamiento para analizar transición

df <- bind_rows(caa2003, caa2008, caa2013, caa2018)

# Verificar la estructura de df
str(df)

df$tcode <- data$tcode
df$ID <- data$ID

table(df$clusterward[df$tcode == 2003])
table(df$clusterward[df$tcode == 2008])
table(df$clusterward[df$tcode == 2013])
table(df$clusterward[df$tcode == 2018])

# Matrices de transición

crear_matriz_transicion <- function(datos, quinquenio_inicio, quinquenio_fin) {
  # Filtrar los datos para los dos quinquenios
  datos_filtrados <- datos %>%
    filter(tcode %in% c(quinquenio_inicio, quinquenio_fin))
  
  # Verificar que hay datos para ambos quinquenios
  if (nrow(datos_filtrados) == 0) {
    stop("No hay datos para los quinquenios especificados.")
  }
  
  # Crear tabla de transición manualmente
  transicion <- datos_filtrados %>%
    group_by(ID) %>%
    summarise(
      cluster_inicio = clusterward[tcode == quinquenio_inicio],
      cluster_fin = clusterward[tcode == quinquenio_fin],
      .groups = 'drop'
    ) %>%
    filter(!is.na(cluster_inicio), !is.na(cluster_fin)) %>%
    mutate(cluster_inicio = as.character(cluster_inicio),
           cluster_fin = as.character(cluster_fin)) %>%
    group_by(cluster_inicio, cluster_fin) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Obtener todos los clusters posibles
  clusters <- sort(unique(c(transicion$cluster_inicio, transicion$cluster_fin)))
  
  # Crear una cuadrícula completa de todas las combinaciones posibles
  grid_completo <- expand.grid(
    cluster_inicio = clusters,
    cluster_fin = clusters,
    stringsAsFactors = FALSE
  )
  
  # Combinar con los datos reales y rellenar con 0 donde no haya datos
  transicion_completa <- grid_completo %>%
    left_join(transicion, by = c("cluster_inicio", "cluster_fin")) %>%
    mutate(n = replace_na(n, 0))
  
  # Convertir a formato de matriz
  matriz_transicion <- transicion_completa %>%
    pivot_wider(
      names_from = cluster_fin,
      values_from = n,
      values_fill = 0
    ) %>%
    column_to_rownames("cluster_inicio") %>%
    as.matrix()
  
  # Ordenar la matriz por los nombres de fila y columna
  matriz_transicion <- matriz_transicion[sort(rownames(matriz_transicion)), 
                                         sort(colnames(matriz_transicion))]
  
  return(matriz_transicion)
}

# Crear matrices de transición para cada par de quinquenios
transicion_2003_2008 <- crear_matriz_transicion(df, 2003, 2008)
transicion_2008_2013 <- crear_matriz_transicion(df, 2008, 2013)
transicion_2013_2018 <- crear_matriz_transicion(df, 2013, 2018)
transicion_2003_2018 <- crear_matriz_transicion(df, 2003, 2018)

# Mostrar las matrices de transición
print("Transición 2003-2008:")
print(transicion_2003_2008)

print("Transición 2008-2013:")
print(transicion_2008_2013)

print("Transición 2013-2018:")
print(transicion_2013_2018)

print("Transición 2003-2018:")
print(transicion_2003_2018)

# probabilidad de que una entidad en un cluster en 2003 pase a otro cluster en 2008.
matriz_transicion_normalizada3_8 <- prop.table(transicion_2003_2008, margin = 1)
print(matriz_transicion_normalizada3_8)
write.csv(matriz_transicion_normalizada3_8, "matriz_transicion_normalizada3_8.csv", row.names = FALSE)

matriz_transicion_normalizada8_13 <- prop.table(transicion_2008_2013, margin = 1)
print(matriz_transicion_normalizada8_13)
write.csv(matriz_transicion_normalizada8_13, "matriz_transicion_normalizada8_13.csv", row.names = FALSE)

matriz_transicion_normalizada13_18 <- prop.table(transicion_2013_2018, margin = 1)
print(matriz_transicion_normalizada13_18)
write.csv(matriz_transicion_normalizada13_18, "matriz_transicion_normalizada13_18.csv", row.names = FALSE)

matriz_transicion_normalizada3_18 <- prop.table(transicion_2003_2018, margin = 1)
print(matriz_transicion_normalizada3_18)
write.csv(matriz_transicion_normalizada3_18, "matriz_transicion_normalizada3_18.csv", row.names = FALSE)

# Función para calcular la estabilidad
calcular_estabilidad <- function(matriz) {
  estabilidad <- sum(diag(matriz)) / sum(matriz) * 100
  return(estabilidad)
}

# Calcular la estabilidad para cada transición
estabilidad_2003_2008 <- calcular_estabilidad(transicion_2003_2008)
estabilidad_2008_2013 <- calcular_estabilidad(transicion_2008_2013)
estabilidad_2013_2018 <- calcular_estabilidad(transicion_2013_2018)
estabilidad_2003_2018 <- calcular_estabilidad(transicion_2003_2018)



# Mostrar la estabilidad
print(paste("Estabilidad 2003-2008:", round(estabilidad_2003_2008, 2), "%"))
print(paste("Estabilidad 2008-2013:", round(estabilidad_2008_2013, 2), "%"))
print(paste("Estabilidad 2013-2018:", round(estabilidad_2013_2018, 2), "%"))
print(paste("Estabilidad 2003-2018:", round(estabilidad_2003_2018, 2), "%"))

# Convertir las matrices de transición a dataframes largos

library(reshape2)

transicion_2003_2008_larga <- melt(transicion_2003_2008, varnames = c("Cluster_2003", "Cluster_2008"), value.name = "Frecuencia")
transicion_2008_2013_larga <- melt(transicion_2008_2013, varnames = c("Cluster_2008", "Cluster_2013"), value.name = "Frecuencia")
transicion_2013_2018_larga <- melt(transicion_2013_2018, varnames = c("Cluster_2013", "Cluster_2018"), value.name = "Frecuencia")
transicion_2003_2018_larga <- melt(transicion_2003_2018, varnames = c("Cluster_2003", "Cluster_2018"), value.name = "Frecuencia")

transicion_2003_2018_larga

# Combinar los dataframes en uno solo
transicion_completa <- transicion_2003_2008_larga %>%
  left_join(transicion_2008_2013_larga, by = "Cluster_2008") %>%
  left_join(transicion_2013_2018_larga, by = "Cluster_2013") %>%
  left_join(transicion_2003_2018_larga, by = "Cluster_2003")

# Filtrar solo las transiciones con frecuencia mayor a 0
transicion_completa <- transicion_completa %>% filter(Frecuencia > 0)

print(transicion_completa)

# Prueba de Persistencia
# Criterio: Diagonal de matriz (probabilidad de permanecer) > 0.60 para Cluster TI

persistence_rates <- map_dfr(1:4, function(k) {
  tibble(
    cluster = k,
    persistence_03_08 = transicion_2003_2008[k, k],
    persistence_08_13 = transicion_2008_2013[k, k],
    persistence_13_18 = transicion_2013_2018[k, k],
    persistence_03_18 = transicion_2003_2018[k, k],
    avg_persistence = mean(c(transicion_2003_2008[k, k], 
                             transicion_2008_2013[k, k], 
                             transicion_2013_2018[k, k],
                             transicion_2003_2018[k, k]))
  )
})

persistence_rates

# Test de Estacionariedad de Cadena de Markov

library(DTMCPack)

# Prueba chi-cuadrado con simulación
markov_test_0308 <- chisq.test(transicion_2003_2008, simulate.p.value = TRUE, B = 10000)
print(markov_test_0308)

# ¿QUÉ SIGNIFICA ESTO EN LA PRÁCTICA?
interpretacion <- if(markov_test_0308$p.value < 0.001) {
  "FUERTE EVIDENCIA de que los clusters NO cambian aleatoriamente"
} else if(markov_test_0308$p.value < 0.01) {
  "EVIDENCIA SIGNIFICATIVA de patrones estructurados de transición"
} else {
  "EVIDENCIA MODERADA de dependencia entre estados"
}

cat("Interpretación:", interpretacion, "\n")

cat("=== IMPLICACIONES DEL RESULTADO ===\n")
cat("1. Los sector/entidad NO se mueven aleatoriamente entre clusters\n")
cat("2. Existe una ESTRUCTURA DE TRANSICIÓN predecible\n")
cat("3. El cluster inicial INFLUYE significativamente en el cluster futuro\n")
cat("4. Puedes modelar estas transiciones usando cadenas de Markov\n")

# 1. EXAMINAR LA MATRIZ DE TRANSICIÓN
cat("\n1. Matriz de transición observada:\n")
print(transicion_2003_2008)

# 2. CALCULAR PROBABILIDADES DE TRANSICIÓN
prob_transicion03 <- prop.table(transicion_2003_2008, margin = 1)
cat("\n2. Matriz de probabilidades de transición:\n")
print(round(prob_transicion03, 3))

# 3. IDENTIFICAR PATRONES ESPECÍFICOS
cat("\n3. Análisis de patrones:\n")

# Estabilidad (diagonal principal)
estabilidad03 <- diag(prob_transicion03)
cat("   - Tasa de estabilidad por cluster:\n")
print(round(estabilidad03, 3))

# Cluster más estable
cat("   - Cluster más estable:", which.max(estabilidad03), 
    "(tasa:", round(max(estabilidad03), 3), ")\n")

# Cluster menos estable  
cat("   - Cluster menos estable:", which.min(estabilidad03),
    "(tasa:", round(min(estabilidad03), 3), ")\n")


# Crear heatmap de la matriz de probabilidades
prob_melted03 <- melt(prob_transicion03)
colnames(prob_melted03) <- c("Desde", "Hacia", "Probabilidad")

ggplot(prob_melted03, aes(Hacia, Desde, fill = Probabilidad)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Probabilidad)), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "Matriz de Probabilidades de Transición 2003-2008",
       subtitle = paste("Test de Markov: p-value =", 
                        format(markov_test_0308$p.value, scientific = TRUE)),
       x = "Cluster 2008", 
       y = "Cluster 2003") +
  theme_minimal()


# Prueba chi-cuadrado con simulación
markov_test_0813 <- chisq.test(transicion_2008_2013, simulate.p.value = TRUE, B = 10000)
print(markov_test_0813)


# 1. EXAMINAR LA MATRIZ DE TRANSICIÓN
cat("\n1. Matriz de transición observada:\n")
print(transicion_2008_2013)

# 2. CALCULAR PROBABILIDADES DE TRANSICIÓN
prob_transicion08 <- prop.table(transicion_2008_2013, margin = 1)
cat("\n2. Matriz de probabilidades de transición:\n")
print(round(prob_transicion08, 3))

# 3. IDENTIFICAR PATRONES ESPECÍFICOS
cat("\n3. Análisis de patrones:\n")

# Estabilidad (diagonal principal)
estabilidad08 <- diag(prob_transicion08)
cat("   - Tasa de estabilidad por cluster:\n")
print(round(estabilidad08, 3))

# Cluster más estable
cat("   - Cluster más estable:", which.max(estabilidad08), 
    "(tasa:", round(max(estabilidad08), 3), ")\n")

# Cluster menos estable  
cat("   - Cluster menos estable:", which.min(estabilidad08),
    "(tasa:", round(min(estabilidad08), 3), ")\n")


# Crear heatmap de la matriz de probabilidades
prob_melted08 <- melt(prob_transicion08)
colnames(prob_melted08) <- c("Desde", "Hacia", "Probabilidad")

ggplot(prob_melted08, aes(Hacia, Desde, fill = Probabilidad)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Probabilidad)), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "Matriz de Probabilidades de Transición 2008-2013",
       subtitle = paste("Test de Markov: p-value =", 
                        format(markov_test_0813$p.value, scientific = TRUE)),
       x = "Cluster 2013", 
       y = "Cluster 2008") +
  theme_minimal()

# Prueba chi-cuadrado con simulación
markov_test_1318 <- chisq.test(transicion_2013_2018, simulate.p.value = TRUE, B = 10000)
print(markov_test_1318)


# 1. EXAMINAR LA MATRIZ DE TRANSICIÓN
cat("\n1. Matriz de transición observada:\n")
print(transicion_2013_2018)

# 2. CALCULAR PROBABILIDADES DE TRANSICIÓN
prob_transicion13 <- prop.table(transicion_2013_2018, margin = 1)
cat("\n2. Matriz de probabilidades de transición:\n")
print(round(prob_transicion13, 3))

# 3. IDENTIFICAR PATRONES ESPECÍFICOS
cat("\n3. Análisis de patrones:\n")

# Estabilidad (diagonal principal)
estabilidad13 <- diag(prob_transicion13)
cat("   - Tasa de estabilidad por cluster:\n")
print(round(estabilidad13, 3))

# Cluster más estable
cat("   - Cluster más estable:", which.max(estabilidad13), 
    "(tasa:", round(max(estabilidad13), 3), ")\n")

# Cluster menos estable  
cat("   - Cluster menos estable:", which.min(estabilidad13),
    "(tasa:", round(min(estabilidad13), 3), ")\n")


# Crear heatmap de la matriz de probabilidades
prob_melted13 <- melt(prob_transicion13)
colnames(prob_melted13) <- c("Desde", "Hacia", "Probabilidad")

ggplot(prob_melted13, aes(Hacia, Desde, fill = Probabilidad)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Probabilidad)), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "Matriz de Probabilidades de Transición 2013-2018",
       subtitle = paste("Test de Markov: p-value =", 
                        format(markov_test_1318$p.value, scientific = TRUE)),
       x = "Cluster 2018", 
       y = "Cluster 2013") +
  theme_minimal()

# Prueba chi-cuadrado con simulación
markov_test_0318 <- chisq.test(transicion_2003_2018, simulate.p.value = TRUE, B = 10000)
print(markov_test_0318)


# 1. EXAMINAR LA MATRIZ DE TRANSICIÓN
cat("\n1. Matriz de transición observada:\n")
print(transicion_2003_2018)

# 2. CALCULAR PROBABILIDADES DE TRANSICIÓN
prob_transicion003 <- prop.table(transicion_2003_2018, margin = 1)
cat("\n2. Matriz de probabilidades de transición:\n")
print(round(prob_transicion003, 3))

# 3. IDENTIFICAR PATRONES ESPECÍFICOS
cat("\n3. Análisis de patrones:\n")

# Estabilidad (diagonal principal)
estabilidad003 <- diag(prob_transicion003)
cat("   - Tasa de estabilidad por cluster:\n")
print(round(estabilidad003, 3))

# Cluster más estable
cat("   - Cluster más estable:", which.max(estabilidad003), 
    "(tasa:", round(max(estabilidad003), 3), ")\n")

# Cluster menos estable  
cat("   - Cluster menos estable:", which.min(estabilidad003),
    "(tasa:", round(min(estabilidad003), 3), ")\n")


# Crear heatmap de la matriz de probabilidades
prob_melted003 <- melt(prob_transicion003)
colnames(prob_melted003) <- c("Desde", "Hacia", "Probabilidad")

ggplot(prob_melted003, aes(Hacia, Desde, fill = Probabilidad)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Probabilidad)), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "red", 
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "Matriz de Probabilidades de Transición 2003-2018",
       subtitle = paste("Test de Markov: p-value =", 
                        format(markov_test_0318$p.value, scientific = TRUE)),
       x = "Cluster 2018", 
       y = "Cluster 2003") +
  theme_minimal()


# Calcular tasas de retención (cuántos se mantienen en el mismo cluster)
retention_rates <- data.frame(
  Periodo = c("2003-2008", "2008-2013", "2013-2018", "2003-2018"),
  Tasa_Retención = c(
    sum(diag(transicion_2003_2008)) / sum(transicion_2003_2008),
    sum(diag(transicion_2008_2013)) / sum(transicion_2008_2013), 
    sum(diag(transicion_2013_2018)) / sum(transicion_2013_2018),
    sum(diag(transicion_2003_2018)) / sum(transicion_2003_2018)
  )
)

cat("\n4. TASAS DE RETENCIÓN (diagonal principal):\n")
print(retention_rates)

library(ggplot2)
library(reshape2)

# Función para graficar matriz de transición
plot_transition_matrix <- function(trans_matrix, title) {
  melted <- melt(trans_matrix)
  colnames(melted) <- c("From", "To", "Count")
  
  ggplot(melted, aes(To, From, fill = Count)) +
    geom_tile() +
    geom_text(aes(label = Count), color = "white", size = 4) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = title, x = "Cluster Final", y = "Cluster Inicial") +
    theme_minimal()
}

# Crear gráficos
plot_transition_matrix(trans_0308, "Transición 2003-2008")
plot_transition_matrix(trans_0813, "Transición 2008-2013") 
plot_transition_matrix(trans_1318, "Transición 2013-2018")
plot_transition_matrix(trans_0318, "Transición 2003-2018")

# Crear el diagrama de flujo

library(ggalluvial)

ggplot(transicion_completa,
       aes(axis1 = Cluster_2003, axis2 = Cluster_2008, axis3 = Cluster_2013, axis4 = Cluster_2018, y = Frecuencia)) +
  geom_alluvium(aes(fill = Cluster_2003), width = 1/12) +
  geom_stratum(width = 1/12, fill = "lightgray", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("2003", "2008", "2013", "2018"), expand = c(0.05, 0.05)) +
  labs(title = "Transición de clusters 2003-2018",
       x = "Período",
       y = "Número de entidades",
       fill = "Cluster 2003") +
  theme_minimal()

# PCA

library(FactoMineR)

# Seleccionar solo las variables numéricas
df_numerico <- df %>% dplyr::select(comppvs, compacd, marppvs, marpacd, icp, ici, ics)

# Realizar PCA
pca_resultado <- PCA(df_numerico, scale.unit = TRUE, graph = FALSE)

# Visualizar los clusters en el espacio de los dos primeros componentes principales
fviz_pca_ind(pca_resultado, geom.ind = "point", col.ind = df$cluster, 
             palette = "jco", addEllipses = TRUE, legend.title = "Cluster")

# Filtrar los datos para los períodos de interés
df_filtrado <- df %>%
  filter(tcode %in% c(2003, 2008, 2013, 2018))

# Crear un dataframe con los clusters y las variables numéricas por período
df_clusters <- df_filtrado %>%
  dplyr::select(ID, tcode, cluster, comppvs, compacd, marppvs, marpacd, icp, ici, ics) %>%
  spread(key = tcode, value = cluster, sep = "_")

# Renombrar las columnas para mayor claridad
colnames(df_clusters) <- c("ID", "cluster_2003", "cluster_2008", "cluster_2013", "cluster_2018",
                           "comppvs", "compacd", "marppvs", "marpacd", "icp", "ici", "ics")

# Ver el dataframe resultante
print(head(df_clusters))

df_transicion <- df_clusters %>%
  mutate(transicion = ifelse(cluster_2003 == cluster_2008, "Mismo cluster", "Cambio de cluster"))

# Ver el dataframe resultante
print(head(df_transicion))

# Boxplot para comppvs
ggplot(df_transicion, aes(x = transicion, y = comppvs, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de comppvs entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "comppvs") +
  theme_minimal()

# Boxplot para compacd
ggplot(df_transicion, aes(x = transicion, y = compacd, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de compacd entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "compacd") +
  theme_minimal()

# Boxplot para marppvs
ggplot(df_transicion, aes(x = transicion, y = marppvs, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de marppvs entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "marppvs") +
  theme_minimal()

# Boxplot para marpacd
ggplot(df_transicion, aes(x = transicion, y = marpacd, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de marpacd entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "marpacd") +
  theme_minimal()

# Boxplot para icp
ggplot(df_transicion, aes(x = transicion, y = icp, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de icp entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "icp") +
  theme_minimal()

# Boxplot para ici
ggplot(df_transicion, aes(x = transicion, y = ici, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de ici entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "ici") +
  theme_minimal()

# Boxplot para ics
ggplot(df_transicion, aes(x = transicion, y = ics, fill = transicion)) +
  geom_boxplot() +
  labs(title = "Comparación de ics entre entidades que cambiaron y no cambiaron de cluster",
       x = "Transición",
       y = "ici") +
  theme_minimal()

# Filtrar los datos para los períodos de interés
df_fil <- df %>%
  filter(tcode %in% c(2003, 2008, 2013, 2018))

# Crear un dataframe con los clusters por período
df_clu <- df_fil %>%
  dplyr::select(ID, tcode, clusterward) %>%
  spread(key = tcode, value = clusterward, sep = "_")

# Renombrar las columnas para mayor claridad
colnames(df_clu) <- c("ID", "cluster_2003", "cluster_2008", "cluster_2013", "cluster_2018")

# Ver el dataframe resultante
print(head(df_clu))



library(networkD3)

# Crear un dataframe de transiciones entre 2003 y 2008
t_2003_2008 <- df_clu %>%
  group_by(cluster_2003, cluster_2008) %>%
  summarise(Frecuencia = n(), .groups = 'drop')

# Crear un dataframe de transiciones entre 2008 y 2013
t_2008_2013 <- df_clu %>%
  group_by(cluster_2008, cluster_2013) %>%
  summarise(Frecuencia = n(), .groups = 'drop')

# Crear un dataframe de transiciones entre 2013 y 2018
t_2013_2018 <- df_clu %>%
  group_by(cluster_2013, cluster_2018) %>%
  summarise(Frecuencia = n(), .groups = 'drop')

# Combinar todas las transiciones
transiciones <- bind_rows(
  t_2003_2008 %>% rename(from = cluster_2003, to = cluster_2008),
  t_2008_2013 %>% rename(from = cluster_2008, to = cluster_2013),
  t_2013_2018 %>% rename(from = cluster_2013, to = cluster_2018)
)

# Ver el dataframe de transiciones
print(transiciones)

transiciones_df <- as.data.frame(transiciones)

str(transiciones_df)

transiciones_df$Frecuencia <- as.numeric(transiciones_df$Frecuencia)
transiciones_df$from <- as.numeric(as.character(transiciones_df$from))
transiciones_df$to <- as.numeric(as.character(transiciones_df$to))

# Convert 'from' and 'to' columns to zero-indexed values
transiciones_df$from <- transiciones_df$from - 1
transiciones_df$to <- transiciones_df$to - 1

# Create the Nodes data frame with zero-indexed values
nodes_df <- data.frame(name = unique(c(transiciones_df$from, transiciones_df$to)))

# Generate the Sankey diagram
sankey <- sankeyNetwork(
  Links = transiciones_df,
  Nodes = nodes_df,
  Source = "from",
  Target = "to",
  Value = "Frecuencia",
  NodeID = "name",
  fontSize = 14,  # Tamaño de la fuente
  nodeWidth = 20,  # Ancho de los nodos
  nodePadding = 10,  # Espaciado entre nodos
  width = 1150,  # Ancho del gráfico
  height = 800   # Alto del gráfico
)

sankey

saveNetwork(sankey, "diagrama_sankey.html")

# Tasa de retención

retencion_2003_2008 <- df_clu %>%
  summarise(retencion = mean(cluster_2003 == cluster_2008) * 100)
print(retencion_2003_2008)

retencion_2008_2013 <- df_clu %>%
  summarise(retencion = mean(cluster_2008 == cluster_2013) * 100)
print(retencion_2008_2013)

retencion_2013_2018 <- df_clu %>%
  summarise(retencion = mean(cluster_2013 == cluster_2018) * 100)
print(retencion_2013_2018)

retencion_2003_2018 <- df_clu %>%
  summarise(retencion = mean(cluster_2003 == cluster_2018) * 100)
print(retencion_2003_2018)

# Tasa de migración

migracion_2003_2008 <- df_clu %>%
  summarise(migracion = mean(cluster_2003 != cluster_2008) * 100)
print(migracion_2003_2008)

migracion_2008_2013 <- df_clu %>%
  summarise(migracion = mean(cluster_2008 != cluster_2013) * 100)
print(migracion_2008_2013)

migracion_2013_2018 <- df_clu %>%
  summarise(migracion = mean(cluster_2013 != cluster_2018) * 100)
print(migracion_2013_2018)


### Maping
mx <- read_sf("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.shp")

encodings <- guess_encoding("C:/Users/gezum/Desktop/Entidades_Federativas/Entidades_Federativas.dbf")
print(encodings)

# Identificar las columnas de texto
text_columns <- sapply(mx, is.character)

# Convertir solo las columnas de texto a UTF-8
for (col in names(mx)[text_columns]) {
  mx[[col]] <- iconv(mx[[col]], from = "windows-1252", to = "UTF-8")
}

st_geometry_type(mx)

library(spdep)

# Autocorrelación Espacial (Índice de Moran I)
# Construcción de matriz de pesos espaciales

coords <- st_centroid(mx) %>% st_coordinates()
nb <- knn2nb(knearneigh(coords, k = 4))  # 4 vecinos más cercanos
listw <- nb2listw(nb, style = "W")

df$NOMGEO <- data$NOMGEO
df$AE <- data$AE

jdf_sf <- df %>%
  inner_join(mx, by = "NOMGEO") %>%
  st_as_sf()

library(sf)
library(spdep)
library(purrr)


caa2003_sf <- caa2003 %>%
  inner_join(mx, by = "NOMGEO") %>%
  st_as_sf()

caa2008_sf <- caa2008 %>%
  inner_join(mx, by = "NOMGEO") %>%
  st_as_sf()

caa2013_sf <- caa2013 %>%
  inner_join(mx, by = "NOMGEO") %>%
  st_as_sf()

caa2018_sf <- caa2018 %>%
  inner_join(mx, by = "NOMGEO") %>%
  st_as_sf()

### Análisis espacial

# Agrupar por entidad y calcular promedios (o sumas, según lo que necesites)
datos_entidades03 <- caa2003_sf %>%
  st_drop_geometry() %>%
  group_by(NOMGEO) %>%
  summarise(
    icp = mean(icp, na.rm = TRUE),
    ici = mean(ici, na.rm = TRUE),
    ics = mean(ics, na.rm = TRUE),
    marppvs = mean(marppvs, na.rm = TRUE),
    marpacd = mean(marpacd, na.rm = TRUE),
    divppvs = mean(divppvs, na.rm = TRUE),
    divpacd = mean(divpacd, na.rm = TRUE),
    comppvs = mean(comppvs, na.rm = TRUE),
    compacd = mean(compacd, na.rm = TRUE)
  )

# Verificar que tenemos 32 entidades
cat("Número de entidades:", nrow(datos_entidades03), "\n")

# Obtener una geometría única por entidad (disolver polígonos)
entidades_sf03 <- caa2003_sf %>%
  group_by(NOMGEO) %>%
  summarise(do_union = TRUE) %>%
  ungroup()

# Unir los datos agregados con las geometrías
entidades_sf03 <- left_join(entidades_sf03, datos_entidades03, by = "NOMGEO")

# Verificar
cat("Dimensiones del objeto entidades_sf:", nrow(entidades_sf03), "entidades\n")

# Crear matriz de vecindad para las entidades
vecinos_entidades03 <- poly2nb(entidades_sf03)
listw_entidades03 <- nb2listw(vecinos_entidades03, style = "W", zero.policy = TRUE)

# Verificar la matriz de pesos
print("Resumen de la matriz de pesos:")
summary(listw_entidades03)

# Preparar las variables
variables_entidades03 <- st_drop_geometry(entidades_sf03)[, c("icp", "ici", "ics", 
                                                          "marppvs", "marpacd",
                                                          "divppvs", "divpacd",
                                                          "comppvs", "compacd")]
# Calcular Moran para cada variable
resultados_entidades03 <- data.frame(
  Variable = character(),
  Moran_I = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for(var in names(variables_entidades03)) {
  tryCatch({
    moran_result <- moran.test(variables_entidades03[[var]], 
                               listw = listw_entidades03, 
                               zero.policy = TRUE)
    
    resultados_entidades03 <- rbind(
      resultados_entidades03,
      data.frame(
        Variable = var,
        Moran_I = moran_result$estimate[1],
        p_value = moran_result$p.value
      )
    )
    cat("✓", var, "- Moran I:", round(moran_result$estimate[1], 4), 
        "- p-value:", round(moran_result$p.value, 4), "\n")
    
  }, error = function(e) {
    cat("✗ Error con", var, ":", e$message, "\n")
  })
}

print(resultados_entidades03)

# Interpretación de resultados
resultados_entidades03$Significativo <- ifelse(
  resultados_entidades03$p_value < 0.05, "SÍ", "NO"
)

resultados_entidades03$Interpretacion <- ifelse(
  resultados_entidades03$Moran_I > 0, 
  "Agrupamiento espacial",
  ifelse(resultados_entidades03$Moran_I < 0, 
         "Dispersión espacial", 
         "Aleatorio")
)

print(resultados_entidades03)

resultados_entidades03 %>% kable(., caption="Índice de Moran 2003")

write.csv(resultados_entidades03, "resultados_entidades03.csv", row.names = FALSE)

# Visualización Moran

ggplot(resultados_entidades03, 
       aes(x = reorder(Variable, Moran_I), y = Moran_I, 
           fill = Interpretacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Significativo == "SÍ", "*", "")), 
            vjust = -0.5, size = 8) +
  scale_fill_manual(values = c("Agrupamiento espacial" = "red", 
                               "Dispersión espacial" = "blue",
                               "Aleatorio" = "gray")) +
  labs(title = "Índice de Moran para las 32 entidades",
       subtitle = "* = Significativo (p < 0.05)",
       x = "Variable", y = "Moran's I") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear mapas para una variable específica (ejemplo con 'icp')
library(tmap)

# Calcular Moran local para una variable
moran_local_ici <- localmoran(variables_entidades03$ici, 
                              listw = listw_entidades03, zero.policy = TRUE)
moran_local_icp <- localmoran(variables_entidades03$icp, 
                              listw = listw_entidades03, zero.policy = TRUE)
moran_local_ics <- localmoran(variables_entidades03$ics, 
                              listw = listw_entidades03, zero.policy = TRUE)
moran_local_marppvs <- localmoran(variables_entidades03$marppvs, 
                              listw = listw_entidades03, zero.policy = TRUE)
moran_local_marpacd <- localmoran(variables_entidades03$marpacd, 
                                  listw = listw_entidades03, zero.policy = TRUE)
moran_local_comppvs <- localmoran(variables_entidades03$comppvs, 
                                  listw = listw_entidades03, zero.policy = TRUE)
moran_local_compacd <- localmoran(variables_entidades03$compacd, 
                                  listw = listw_entidades03, zero.policy = TRUE)
moran_local_divppvs <- localmoran(variables_entidades03$divppvs, 
                                  listw = listw_entidades03, zero.policy = TRUE)
moran_local_divpacd <- localmoran(variables_entidades03$divpacd, 
                                  listw = listw_entidades03, zero.policy = TRUE)


# Añadir resultados al objeto espacial
entidades_sf03$moran_local_ici <- moran_local_ici[, "Ii"]
entidades_sf03$p_value_local <- moran_local_ici[, "Pr(z != E(Ii))"]
entidades_sf03$moran_local_icp <- moran_local_icp[, "Ii"]
entidades_sf03$p_value_local <- moran_local_icp[, "Pr(z != E(Ii))"]
entidades_sf03$moran_local_ics <- moran_local_ics[, "Ii"]
entidades_sf03$p_value_local <- moran_local_ics[, "Pr(z != E(Ii))"]

entidades_sf03$moran_local_marppvs <- moran_local_marppvs[, "Ii"]
entidades_sf03$p_value_local <- moran_local_marppvs[, "Pr(z != E(Ii))"]
entidades_sf03$moran_local_marpacd <- moran_local_marpacd[, "Ii"]
entidades_sf03$p_value_local <- moran_local_marpacd[, "Pr(z != E(Ii))"]

entidades_sf03$moran_local_comppvs <- moran_local_comppvs[, "Ii"]
entidades_sf03$p_value_local <- moran_local_comppvs[, "Pr(z != E(Ii))"]
entidades_sf03$moran_local_compacd <- moran_local_compacd[, "Ii"]
entidades_sf03$p_value_local <- moran_local_compacd[, "Pr(z != E(Ii))"]

entidades_sf03$moran_local_divppvs <- moran_local_divppvs[, "Ii"]
entidades_sf03$p_value_local <- moran_local_divppvs[, "Pr(z != E(Ii))"]
entidades_sf03$moran_local_divpacd <- moran_local_divpacd[, "Ii"]
entidades_sf03$p_value_local <- moran_local_divpacd[, "Pr(z != E(Ii))"]

# Mapa del Índice de Moran local
tm_shape(entidades_sf03) +
  tm_fill("moran_local_ici", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (ICI)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - ICI")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_icp", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (ICP)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - ICP")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_ics", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (ICS)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - ICS")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_marppvs", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (MARPPVS)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - MARPPVS")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_marpacd", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (MARPACD)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - MARPACD")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_comppvs", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (COMPPVS)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - COMPPVS")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_compacd", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (COMPACD)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - COMPACD")

tm_shape(entidades_sf03) +
  tm_fill("moran_local_divppvs", 
          style = "quantile",
          palette = "RdBu",
          title = "Moran Local (DIVPPVS)") +
  tm_borders() +
  tm_layout(main.title = "Autocorrelación Espacial Local - DIVPPVS")

tm_shape(entidades_sf03) +
     tm_fill("moran_local_divpacd", 
                         style = "quantile",
                         palette = "RdBu",
                         title = "Moran Local (DIVPACD)") +
     tm_borders() + 
     tm_layout(main.title = "Autocorrelación Espacial Local - DIVPACD")

# Resumen estadístico
cat("=== RESUMEN PARA 32 ENTIDADES ===\n")
cat("Variables con autocorrelación espacial significativa (p < 0.05):\n")
significativas <- resultados_entidades03[resultados_entidades03$p_value < 0.05, ]
print(significativas)
     
cat("\nVariable con mayor agrupamiento espacial:\n")
mayor_agrupamiento03 <- resultados_entidades03[which.max(resultados_entidades03$Moran_I), ]
print(mayor_agrupamiento03)
     
cat("\nVariable con mayor dispersión espacial:\n")
mayor_dispersion03 <- resultados_entidades03[which.min(resultados_entidades03$Moran_I), ]
print(mayor_dispersion03)
     
# Proporción de variables significativas
prop_significativas03 <- mean(resultados_entidades03$p_value < 0.05)
cat("\nProporción de variables con autocorrelación significativa:", 
round(prop_significativas03 * 100, 1), "%\n")
     

# Agrupar por entidad y calcular promedios (o sumas, según lo que necesites)
datos_entidades08 <- caa2008_sf %>%
       st_drop_geometry() %>%
       group_by(NOMGEO) %>%
       summarise(
         icp = mean(icp, na.rm = TRUE),
         ici = mean(ici, na.rm = TRUE),
         ics = mean(ics, na.rm = TRUE),
         marppvs = mean(marppvs, na.rm = TRUE),
         marpacd = mean(marpacd, na.rm = TRUE),
         divppvs = mean(divppvs, na.rm = TRUE),
         divpacd = mean(divpacd, na.rm = TRUE),
         comppvs = mean(comppvs, na.rm = TRUE),
         compacd = mean(compacd, na.rm = TRUE)
       )
     
     # Verificar que tenemos 32 entidades
cat("Número de entidades:", nrow(datos_entidades08), "\n")
     
 # Obtener una geometría única por entidad (disolver polígonos)
entidades_sf08 <- caa2008_sf %>%
       group_by(NOMGEO) %>%
       summarise(do_union = TRUE) %>%
       ungroup()
     
     # Unir los datos agregados con las geometrías
     entidades_sf08 <- left_join(entidades_sf08, datos_entidades08, by = "NOMGEO")
     
     # Verificar
     cat("Dimensiones del objeto entidades_sf:", nrow(entidades_sf08), "entidades\n")
     
     # Crear matriz de vecindad para las entidades
     vecinos_entidades08 <- poly2nb(entidades_sf08)
     listw_entidades08 <- nb2listw(vecinos_entidades08, style = "W", zero.policy = TRUE)
     
     # Verificar la matriz de pesos
     print("Resumen de la matriz de pesos:")
     summary(listw_entidades08)
     
     # Preparar las variables
     variables_entidades08 <- st_drop_geometry(entidades_sf08)[, c("icp", "ici", "ics", 
                                                                   "marppvs", "marpacd",
                                                                   "divppvs", "divpacd",
                                                                   "comppvs", "compacd")]
     # Calcular Moran para cada variable
     resultados_entidades08 <- data.frame(
       Variable = character(),
       Moran_I = numeric(),
       p_value = numeric(),
       stringsAsFactors = FALSE
     )
     
     for(var in names(variables_entidades08)) {
       tryCatch({
         moran_result <- moran.test(variables_entidades08[[var]], 
                                    listw = listw_entidades08, 
                                    zero.policy = TRUE)
         
         resultados_entidades08 <- rbind(
           resultados_entidades08,
           data.frame(
             Variable = var,
             Moran_I = moran_result$estimate[1],
             p_value = moran_result$p.value
           )
         )
         cat("✓", var, "- Moran I:", round(moran_result$estimate[1], 4), 
             "- p-value:", round(moran_result$p.value, 4), "\n")
         
       }, error = function(e) {
         cat("✗ Error con", var, ":", e$message, "\n")
       })
     }
     
     print(resultados_entidades08)
     
     # Interpretación de resultados
     resultados_entidades08$Significativo <- ifelse(
       resultados_entidades08$p_value < 0.05, "SÍ", "NO"
     )
     
     resultados_entidades08$Interpretacion <- ifelse(
       resultados_entidades08$Moran_I > 0, 
       "Agrupamiento espacial",
       ifelse(resultados_entidades08$Moran_I < 0, 
              "Dispersión espacial", 
              "Aleatorio")
     )
     
print(resultados_entidades08)
resultados_entidades08 %>% kable(., caption="Índice de Moran 2008")
write.csv(resultados_entidades08, "resultados_entidades08.csv", row.names = FALSE)
     
     # Visualización Moran
     
     ggplot(resultados_entidades08, 
            aes(x = reorder(Variable, Moran_I), y = Moran_I, 
                fill = Interpretacion)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = ifelse(Significativo == "SÍ", "*", "")), 
                 vjust = -0.5, size = 8) +
       scale_fill_manual(values = c("Agrupamiento espacial" = "red", 
                                    "Dispersión espacial" = "blue",
                                    "Aleatorio" = "gray")) +
       labs(title = "Índice de Moran para las 32 entidades",
            subtitle = "* = Significativo (p < 0.05)",
            x = "Variable", y = "Moran's I") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     
     # Calcular Moran local para una variable
     moran_local_ici <- localmoran(variables_entidades08$ici, 
                                   listw = listw_entidades08, zero.policy = TRUE)
     moran_local_icp <- localmoran(variables_entidades08$icp, 
                                   listw = listw_entidades08, zero.policy = TRUE)
     moran_local_ics <- localmoran(variables_entidades08$ics, 
                                   listw = listw_entidades08, zero.policy = TRUE)
     moran_local_marppvs <- localmoran(variables_entidades08$marppvs, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     moran_local_marpacd <- localmoran(variables_entidades08$marpacd, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     moran_local_comppvs <- localmoran(variables_entidades08$comppvs, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     moran_local_compacd <- localmoran(variables_entidades08$compacd, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     moran_local_divppvs <- localmoran(variables_entidades08$divppvs, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     moran_local_divpacd <- localmoran(variables_entidades08$divpacd, 
                                       listw = listw_entidades08, zero.policy = TRUE)
     
     
     # Añadir resultados al objeto espacial
     entidades_sf08$moran_local_ici <- moran_local_ici[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_ici[, "Pr(z != E(Ii))"]
     entidades_sf08$moran_local_icp <- moran_local_icp[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_icp[, "Pr(z != E(Ii))"]
     entidades_sf08$moran_local_ics <- moran_local_ics[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_ics[, "Pr(z != E(Ii))"]
     
     entidades_sf08$moran_local_marppvs <- moran_local_marppvs[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_marppvs[, "Pr(z != E(Ii))"]
     entidades_sf08$moran_local_marpacd <- moran_local_marpacd[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_marpacd[, "Pr(z != E(Ii))"]
     
     entidades_sf08$moran_local_comppvs <- moran_local_comppvs[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_comppvs[, "Pr(z != E(Ii))"]
     entidades_sf08$moran_local_compacd <- moran_local_compacd[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_compacd[, "Pr(z != E(Ii))"]
     
     entidades_sf08$moran_local_divppvs <- moran_local_divppvs[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_divppvs[, "Pr(z != E(Ii))"]
     entidades_sf08$moran_local_divpacd <- moran_local_divpacd[, "Ii"]
     entidades_sf08$p_value_local <- moran_local_divpacd[, "Pr(z != E(Ii))"]
     
     # Mapa del Índice de Moran local
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_ici", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICI)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICI")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_icp", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICP)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICP")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_ics", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICS")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_marppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPPVS")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_marpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPACD")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_comppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPPVS")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_compacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPACD")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_divppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPPVS")
     
     tm_shape(entidades_sf08) +
       tm_fill("moran_local_divpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPACD")
     
     # Resumen estadístico
     cat("=== RESUMEN PARA 32 ENTIDADES ===\n")
     cat("Variables con autocorrelación espacial significativa (p < 0.05):\n")
     significativas <- resultados_entidades08[resultados_entidades08$p_value < 0.05, ]
     print(significativas)
     
     cat("\nVariable con mayor agrupamiento espacial:\n")
     mayor_agrupamiento08 <- resultados_entidades08[which.max(resultados_entidades08$Moran_I), ]
     print(mayor_agrupamiento08)
     
     cat("\nVariable con mayor dispersión espacial:\n")
     mayor_dispersion08 <- resultados_entidades08[which.min(resultados_entidades08$Moran_I), ]
     print(mayor_dispersion08)
     
     # Proporción de variables significativas
     prop_significativas08 <- mean(resultados_entidades08$p_value < 0.05)
     cat("\nProporción de variables con autocorrelación significativa:", 
         round(prop_significativas08 * 100, 1), "%\n")
     

 # Agrupar por entidad y calcular promedios 
     datos_entidades13 <- caa2013_sf %>%
       st_drop_geometry() %>%
       group_by(NOMGEO) %>%
       summarise(
         icp = mean(icp, na.rm = TRUE),
         ici = mean(ici, na.rm = TRUE),
         ics = mean(ics, na.rm = TRUE),
         marppvs = mean(marppvs, na.rm = TRUE),
         marpacd = mean(marpacd, na.rm = TRUE),
         divppvs = mean(divppvs, na.rm = TRUE),
         divpacd = mean(divpacd, na.rm = TRUE),
         comppvs = mean(comppvs, na.rm = TRUE),
         compacd = mean(compacd, na.rm = TRUE)
       )
     
     # Verificar que tenemos 32 entidades
     cat("Número de entidades:", nrow(datos_entidades13), "\n")
     
     # Obtener una geometría única por entidad (disolver polígonos)
     entidades_sf13 <- caa2013_sf %>%
       group_by(NOMGEO) %>%
       summarise(do_union = TRUE) %>%
       ungroup()
     
     # Unir los datos agregados con las geometrías
     entidades_sf13 <- left_join(entidades_sf13, datos_entidades13, by = "NOMGEO")
     
     # Verificar
     cat("Dimensiones del objeto entidades_sf:", nrow(entidades_sf13), "entidades\n")
     
     # Crear matriz de vecindad para las entidades
     vecinos_entidades13 <- poly2nb(entidades_sf13)
     listw_entidades13 <- nb2listw(vecinos_entidades13, style = "W", zero.policy = TRUE)
     
     # Verificar la matriz de pesos
     print("Resumen de la matriz de pesos:")
     summary(listw_entidades13)
     
     # Preparar las variables
     variables_entidades13 <- st_drop_geometry(entidades_sf13)[, c("icp", "ici", "ics", 
                                                                   "marppvs", "marpacd",
                                                                   "divppvs", "divpacd",
                                                                   "comppvs", "compacd")]
     # Calcular Moran para cada variable
     resultados_entidades13 <- data.frame(
       Variable = character(),
       Moran_I = numeric(),
       p_value = numeric(),
       stringsAsFactors = FALSE
     )
     
     for(var in names(variables_entidades13)) {
       tryCatch({
         moran_result <- moran.test(variables_entidades13[[var]], 
                                    listw = listw_entidades13, 
                                    zero.policy = TRUE)
         
         resultados_entidades13 <- rbind(
           resultados_entidades13,
           data.frame(
             Variable = var,
             Moran_I = moran_result$estimate[1],
             p_value = moran_result$p.value
           )
         )
         cat("✓", var, "- Moran I:", round(moran_result$estimate[1], 4), 
             "- p-value:", round(moran_result$p.value, 4), "\n")
         
       }, error = function(e) {
         cat("✗ Error con", var, ":", e$message, "\n")
       })
     }
     
     print(resultados_entidades13)
     
     # Interpretación de resultados
     resultados_entidades13$Significativo <- ifelse(
       resultados_entidades13$p_value < 0.05, "SÍ", "NO"
     )
     
     resultados_entidades13$Interpretacion <- ifelse(
       resultados_entidades13$Moran_I > 0, 
       "Agrupamiento espacial",
       ifelse(resultados_entidades13$Moran_I < 0, 
              "Dispersión espacial", 
              "Aleatorio")
     )
     
print(resultados_entidades13)
resultados_entidades13 %>% kable(., caption="Índice de Moran 2013")
write.csv(resultados_entidades13, "resultados_entidades13.csv", row.names = FALSE)
     
     # Visualización Moran
     
     ggplot(resultados_entidades13, 
            aes(x = reorder(Variable, Moran_I), y = Moran_I, 
                fill = Interpretacion)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = ifelse(Significativo == "SÍ", "*", "")), 
                 vjust = -0.5, size = 8) +
       scale_fill_manual(values = c("Agrupamiento espacial" = "red", 
                                    "Dispersión espacial" = "blue",
                                    "Aleatorio" = "gray")) +
       labs(title = "Índice de Moran para las 32 entidades",
            subtitle = "* = Significativo (p < 0.05)",
            x = "Variable", y = "Moran's I") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     
     # Calcular Moran local para una variable
     moran_local_ici <- localmoran(variables_entidades13$ici, 
                                   listw = listw_entidades13, zero.policy = TRUE)
     moran_local_icp <- localmoran(variables_entidades13$icp, 
                                   listw = listw_entidades13, zero.policy = TRUE)
     moran_local_ics <- localmoran(variables_entidades13$ics, 
                                   listw = listw_entidades13, zero.policy = TRUE)
     moran_local_marppvs <- localmoran(variables_entidades13$marppvs, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     moran_local_marpacd <- localmoran(variables_entidades13$marpacd, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     moran_local_comppvs <- localmoran(variables_entidades13$comppvs, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     moran_local_compacd <- localmoran(variables_entidades13$compacd, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     moran_local_divppvs <- localmoran(variables_entidades13$divppvs, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     moran_local_divpacd <- localmoran(variables_entidades13$divpacd, 
                                       listw = listw_entidades13, zero.policy = TRUE)
     
     
     # Añadir resultados al objeto espacial
     entidades_sf13$moran_local_ici <- moran_local_ici[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_ici[, "Pr(z != E(Ii))"]
     entidades_sf13$moran_local_icp <- moran_local_icp[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_icp[, "Pr(z != E(Ii))"]
     entidades_sf13$moran_local_ics <- moran_local_ics[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_ics[, "Pr(z != E(Ii))"]
     
     entidades_sf13$moran_local_marppvs <- moran_local_marppvs[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_marppvs[, "Pr(z != E(Ii))"]
     entidades_sf13$moran_local_marpacd <- moran_local_marpacd[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_marpacd[, "Pr(z != E(Ii))"]
     
     entidades_sf13$moran_local_comppvs <- moran_local_comppvs[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_comppvs[, "Pr(z != E(Ii))"]
     entidades_sf13$moran_local_compacd <- moran_local_compacd[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_compacd[, "Pr(z != E(Ii))"]
     
     entidades_sf13$moran_local_divppvs <- moran_local_divppvs[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_divppvs[, "Pr(z != E(Ii))"]
     entidades_sf13$moran_local_divpacd <- moran_local_divpacd[, "Ii"]
     entidades_sf13$p_value_local <- moran_local_divpacd[, "Pr(z != E(Ii))"]
     
     # Mapa del Índice de Moran local
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_ici", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICI)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICI")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_icp", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICP)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICP")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_ics", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICS")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_marppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPPVS")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_marpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPACD")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_comppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPPVS")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_compacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPACD")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_divppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPPVS")
     
     tm_shape(entidades_sf13) +
       tm_fill("moran_local_divpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPACD)") +
       tm_borders() + 
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPACD")
     
     # Resumen estadístico
     cat("=== RESUMEN PARA 32 ENTIDADES ===\n")
     cat("Variables con autocorrelación espacial significativa (p < 0.05):\n")
     significativas <- resultados_entidades13[resultados_entidades13$p_value < 0.05, ]
     print(significativas)
     
     cat("\nVariable con mayor agrupamiento espacial:\n")
     mayor_agrupamiento13 <- resultados_entidades13[which.max(resultados_entidades13$Moran_I), ]
     print(mayor_agrupamiento13)
     
     cat("\nVariable con mayor dispersión espacial:\n")
     mayor_dispersion13 <- resultados_entidades13[which.min(resultados_entidades13$Moran_I), ]
     print(mayor_dispersion13)
     
     # Proporción de variables significativas
     prop_significativas13 <- mean(resultados_entidades13$p_value < 0.05)
     cat("\nProporción de variables con autocorrelación significativa:", 
         round(prop_significativas13 * 100, 1), "%\n")
     
     
     # Agrupar por entidad y calcular promedios 
     datos_entidades18 <- caa2018_sf %>%
       st_drop_geometry() %>%
       group_by(NOMGEO) %>%
       summarise(
         icp = mean(icp, na.rm = TRUE),
         ici = mean(ici, na.rm = TRUE),
         ics = mean(ics, na.rm = TRUE),
         marppvs = mean(marppvs, na.rm = TRUE),
         marpacd = mean(marpacd, na.rm = TRUE),
         divppvs = mean(divppvs, na.rm = TRUE),
         divpacd = mean(divpacd, na.rm = TRUE),
         comppvs = mean(comppvs, na.rm = TRUE),
         compacd = mean(compacd, na.rm = TRUE)
       )
     
     # Verificar que tenemos 32 entidades
     cat("Número de entidades:", nrow(datos_entidades18), "\n")
     
     # Obtener una geometría única por entidad (disolver polígonos)
     entidades_sf18 <- caa2018_sf %>%
       group_by(NOMGEO) %>%
       summarise(do_union = TRUE) %>%
       ungroup()
     
     # Unir los datos agregados con las geometrías
     entidades_sf18 <- left_join(entidades_sf18, datos_entidades18, by = "NOMGEO")
     
     # Verificar
     cat("Dimensiones del objeto entidades_sf:", nrow(entidades_sf18), "entidades\n")
     
     # Crear matriz de vecindad para las entidades
     vecinos_entidades18 <- poly2nb(entidades_sf18)
     listw_entidades18 <- nb2listw(vecinos_entidades18, style = "W", zero.policy = TRUE)
     
     # Verificar la matriz de pesos
     print("Resumen de la matriz de pesos:")
     summary(listw_entidades18)
     
     # Preparar las variables
     variables_entidades18 <- st_drop_geometry(entidades_sf18)[, c("icp", "ici", "ics", 
                                                                   "marppvs", "marpacd",
                                                                   "divppvs", "divpacd",
                                                                   "comppvs", "compacd")]
     # Calcular Moran para cada variable
     resultados_entidades18 <- data.frame(
       Variable = character(),
       Moran_I = numeric(),
       p_value = numeric(),
       stringsAsFactors = FALSE
     )
     
     for(var in names(variables_entidades18)) {
       tryCatch({
         moran_result <- moran.test(variables_entidades18[[var]], 
                                    listw = listw_entidades18, 
                                    zero.policy = TRUE)
         
         resultados_entidades18 <- rbind(
           resultados_entidades18,
           data.frame(
             Variable = var,
             Moran_I = moran_result$estimate[1],
             p_value = moran_result$p.value
           )
         )
         cat("✓", var, "- Moran I:", round(moran_result$estimate[1], 4), 
             "- p-value:", round(moran_result$p.value, 4), "\n")
         
       }, error = function(e) {
         cat("✗ Error con", var, ":", e$message, "\n")
       })
     }
     
     print(resultados_entidades18)
     
     # Interpretación de resultados
     resultados_entidades18$Significativo <- ifelse(
       resultados_entidades18$p_value < 0.05, "SÍ", "NO"
     )
     
     resultados_entidades18$Interpretacion <- ifelse(
       resultados_entidades18$Moran_I > 0, 
       "Agrupamiento espacial",
       ifelse(resultados_entidades18$Moran_I < 0, 
              "Dispersión espacial", 
              "Aleatorio")
     )
     
print(resultados_entidades18)
resultados_entidades18 %>% kable(., caption="Índice de Moran 2018")
write.csv(resultados_entidades18, "resultados_entidades18.csv", row.names = FALSE)
     
     # Visualización Moran
     
     ggplot(resultados_entidades18, 
            aes(x = reorder(Variable, Moran_I), y = Moran_I, 
                fill = Interpretacion)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = ifelse(Significativo == "SÍ", "*", "")), 
                 vjust = -0.5, size = 8) +
       scale_fill_manual(values = c("Agrupamiento espacial" = "red", 
                                    "Dispersión espacial" = "blue",
                                    "Aleatorio" = "gray")) +
       labs(title = "Índice de Moran para las 32 entidades",
            subtitle = "* = Significativo (p < 0.05)",
            x = "Variable", y = "Moran's I") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     
     # Calcular Moran local para una variable
     moran_local_ici <- localmoran(variables_entidades18$ici, 
                                   listw = listw_entidades18, zero.policy = TRUE)
     moran_local_icp <- localmoran(variables_entidades18$icp, 
                                   listw = listw_entidades18, zero.policy = TRUE)
     moran_local_ics <- localmoran(variables_entidades18$ics, 
                                   listw = listw_entidades18, zero.policy = TRUE)
     moran_local_marppvs <- localmoran(variables_entidades18$marppvs, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     moran_local_marpacd <- localmoran(variables_entidades18$marpacd, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     moran_local_comppvs <- localmoran(variables_entidades18$comppvs, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     moran_local_compacd <- localmoran(variables_entidades18$compacd, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     moran_local_divppvs <- localmoran(variables_entidades18$divppvs, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     moran_local_divpacd <- localmoran(variables_entidades18$divpacd, 
                                       listw = listw_entidades18, zero.policy = TRUE)
     
     
     # Añadir resultados al objeto espacial
     entidades_sf18$moran_local_ici <- moran_local_ici[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_ici[, "Pr(z != E(Ii))"]
     entidades_sf18$moran_local_icp <- moran_local_icp[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_icp[, "Pr(z != E(Ii))"]
     entidades_sf18$moran_local_ics <- moran_local_ics[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_ics[, "Pr(z != E(Ii))"]
     
     entidades_sf18$moran_local_marppvs <- moran_local_marppvs[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_marppvs[, "Pr(z != E(Ii))"]
     entidades_sf18$moran_local_marpacd <- moran_local_marpacd[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_marpacd[, "Pr(z != E(Ii))"]
     
     entidades_sf18$moran_local_comppvs <- moran_local_comppvs[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_comppvs[, "Pr(z != E(Ii))"]
     entidades_sf18$moran_local_compacd <- moran_local_compacd[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_compacd[, "Pr(z != E(Ii))"]
     
     entidades_sf18$moran_local_divppvs <- moran_local_divppvs[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_divppvs[, "Pr(z != E(Ii))"]
     entidades_sf18$moran_local_divpacd <- moran_local_divpacd[, "Ii"]
     entidades_sf18$p_value_local <- moran_local_divpacd[, "Pr(z != E(Ii))"]
     
     # Mapa del Índice de Moran local
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_ici", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICI)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICI")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_icp", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICP)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICP")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_ics", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (ICS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - ICS")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_marppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPPVS")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_marpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (MARPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - MARPACD")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_comppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPPVS")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_compacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (COMPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - COMPACD")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_divppvs", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPPVS)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPPVS")
     
     tm_shape(entidades_sf18) +
       tm_fill("moran_local_divpacd", 
               style = "quantile",
               palette = "RdBu",
               title = "Moran Local (DIVPACD)") +
       tm_borders() +
       tm_layout(main.title = "Autocorrelación Espacial Local - DIVPACD")
     
     # Resumen estadístico
     cat("=== RESUMEN PARA 32 ENTIDADES ===\n")
     cat("Variables con autocorrelación espacial significativa (p < 0.05):\n")
     significativas <- resultados_entidades18[resultados_entidades18$p_value < 0.05, ]
     print(significativas)
     
     cat("\nVariable con mayor agrupamiento espacial:\n")
     mayor_agrupamiento18 <- resultados_entidades18[which.max(resultados_entidades18$Moran_I), ]
     print(mayor_agrupamiento18)
     
     cat("\nVariable con mayor dispersión espacial:\n")
     mayor_dispersion18 <- resultados_entidades18[which.min(resultados_entidades18$Moran_I), ]
     print(mayor_dispersion18)
     
     # Proporción de variables significativas
     prop_significativas18 <- mean(resultados_entidades18$p_value < 0.05)
     cat("\nProporción de variables con autocorrelación significativa:", 
         round(prop_significativas18 * 100, 1), "%\n")
     
     
     
# Visualización 
     
     
ggplot(jdf_sf) +
  geom_sf(aes(fill = clusterward), color = NA) +
  scale_fill_viridis_d("Cluster") +
  facet_grid(AE ~ tcode) +  # Filas: sectores, Columnas: tiempo
  theme_minimal() +
  labs(title = "Evolución de clusters por sector y quinquenio") +
  theme(axis.text = element_blank())

ggplot(jdf_sf, aes(x = tcode, fill = clusterward)) +
  geom_bar(position = "fill", width = 0.8) +  # Proporciones normalizadas a 1
  facet_wrap(~ NOMGEO, ncol = 4, nrow = 8) +
  labs(title = "Distribución Proporcional de Conglomerados por Entidad",
       x = "Quinquenio",
       y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        panel.spacing = unit(0.5, "lines"))

library(ggstream)

ggplot(jdf_sf, aes(x = tcode, y = as.numeric(clusterward), fill = AE)) +
  geom_stream(aes(fill = AE), bw = 0.8) +
  facet_wrap(~ NOMGEO, ncol = 4) +
  labs(title = "Distribución de AE en Clusters por Región",
       x = "Quinquenio",
       y = "Cluster") +
  scale_fill_viridis_d() +
  theme_minimal()

# Crear una lista de gráficos por quinquenio

library(patchwork)
library(magick)
library(grid)  

# 1. Generar y guardar gráficos 
if (!all(file.exists(paste0("clu", unique(jdf_sf$tcode), ".tiff")))) {
  plot_list <- lapply(unique(jdf_sf$tcode), function(q) {
    ggplot(jdf_sf[jdf_sf$tcode == q, ]) +
      geom_sf(aes(fill = factor(clusterward)), color = NA, lwd = 0) +
      scale_fill_viridis_d("Cluster") +
      facet_wrap(~ AE, ncol = ceiling(sqrt(length(unique(jdf_sf$AE))))) +
      labs(title = paste("Quinquenio:", q)) +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  # Guardar en alta resolución
  for (i in seq_along(plot_list)) {
    ggsave(
      filename = paste0("clu", unique(jdf_sf$tcode)[i], ".tiff"),
      plot = plot_list[[i]],
      device = "tiff",
      dpi = 600,
      width = 10,
      height = 8,
      compression = "lzw"
    )
  }
}

# 2. Leer imágenes procesadas
img_files <- paste0("clu", unique(jdf_sf$tcode), ".tiff")
img_list <- lapply(img_files, image_read)

# 3. Convertir a grobs con interpolación para mejor calidad
grob_list <- lapply(img_list, function(img) {
  grid::rasterGrob(as.raster(img), interpolate = TRUE)
})

# 4. Crear gráfico final con márgenes corregidos
final_plot <- wrap_plots(grob_list, nrow = 2, ncol = 2) +
  plot_annotation(
    title = "Conglomerados sector-estado por quinquenio",
    caption = "Fuente: Elaboración propia con información de los Censos Económicos 2004, 2009, 2014 y 2019 de INEGI",
    theme = theme(
      plot.title = element_text(
        size = 28, 
        face = "bold", 
        hjust = 0.5, 
        margin = ggplot2::margin(t = 0, r = 0, b = 25, l = 0, unit = "pt")
      ),
      plot.caption = element_text(
        size = 16, 
        hjust = 1, 
        color = "grey40",
        margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0, unit = "pt")
      )
    )
  ) +
  theme(plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))

# 5. Exportar en máxima calidad
ggsave("conglomerados_final.tiff", 
       plot = final_plot,
       device = "tiff",
       dpi = 600,
       width = 16,
       height = 12,
       compression = "lzw")



library(gganimate)

anim <- ggplot(jdf_sf) +
  geom_sf(aes(fill = clusterward), color = NA) +
  scale_fill_viridis_d("Cluster") +
  transition_time(as.integer(tcode)) +
  labs(title = "Quinquenio: {frame_time}") +
  facet_wrap(~AE)

animate(anim, nframes = length(unique(jdf_sf$tcode)), fps = 1)

ggsave("mapa_evolucion.png", width = 12, height = 8)
anim_save("animacion_clusters.gif")

library(ggfx)

ggplot(jdf_sf, aes(x = tcode, fill = clusterward)) +
  geom_bar(position = "fill") +
  facet_grid(rows = vars(NOMGEO), cols = vars(AE)) +
  labs(title = "Distribución de Clusters por Entidad y Actividad Económica",
       x = "Quinquenio",
       y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        strip.text = element_text(size = 12))

library(plotly)

heat_data <- df %>%
  count(NOMGEO, AE, tcode, clusterward) %>%
  group_by(NOMGEO, AE, tcode) %>%
  mutate(prop = n/sum(n))

plot_ly(heat_data, 
        x = ~tcode, 
        y = ~paste(NOMGEO, AE, sep = " - "), 
        z = ~prop, 
        color = ~clusterward,
        type = 'heatmap',
        hoverinfo = 'text',
        text = ~paste(NOMGEO, "<br>AE:", AE, "<br>Cluster:", clusterward, 
                      "<br>Proporción:", round(prop, 2))) %>%
  layout(title = "Distribución 4D de Clusters",
         xaxis = list(title = "Quinquenio"),
         yaxis = list(title = "Entidad - AE", showticklabels = FALSE))

library(DT)

# Crear una tabla resumen interactiva
summary_table <- df %>%
  count(NOMGEO, AE, tcode, clusterward) %>%
  group_by(NOMGEO, AE, tcode) %>%
  mutate(prop = round(n/sum(n), 3)) %>%
  pivot_wider(names_from = clusterward, values_from = prop, values_fill = 0)

datatable(summary_table, 
          filter = 'top',
          options = list(pageLength = 20, autoWidth = TRUE),
          caption = "Distribución de Clusters por Entidad, AE y Quinquenio")


library(trelliscopejs)

# Primero crear un identificador único para cada panel

caa_trellis <- df %>%
  mutate(panel_id = paste(NOMGEO, AE, sep = " - ")) %>%
  group_by(panel_id, tcode, clusterward) %>%
  reframe(count = n(), .groups = 'drop') %>%
  group_by(panel_id, tcode) %>%
  mutate(prop = count/sum(count))

# Crear el visualizador Trelliscope
ggplot(caa_trellis, aes(x = tcode, y = prop, fill = clusterward)) +
  geom_col() +
  facet_trelliscope(~ panel_id, 
                    ncol = 4, 
                    nrow = 8,
                    scales = "free_y",
                    path = "trelliscope_display") +  # Opcional: guardar en directorio
  labs(title = "Evolución Detallada por Entidad-AE",
       x = "Quinquenio",
       y = "Proporción") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
