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
library(ggrepel)
library(cowplot)
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
library(vegan)
library(fastDummies)
library(writexl)



########### Análisis espacial

### Constructing the spatial weights matrix

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

####### Análisis economías de localización y urbanización

indgen <- baseind

indgen[is.na(indgen)] <- 0

cor_matrix <- indgen %>%
  select(eepacd, eeppvs, marpacd, marppvs, compacd, comppvs,
         divpacd, divppvs, prpacdr, prppvsr,) %>%
  cor(use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
colnames(cor_df) <- c("variable1", "variable2", "correlation")

f_cor_table <- cor_df %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_cor_table)

colnames(cor_df) <- c("Variable1", "Variable2", "Correlation")

ggplot(data = cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Matriz de Correlaciones", x = "Variable", y = "Variable")

########## Análisis descriptivo

summary_table_eepacd <- indgen %>%
  group_by(NOMGEO) %>%
  summarise(media = mean(eepacd),
            ds = sd(eepacd),
            min = min(eepacd),
            Q1 = quantile(eepacd, 0.25),
            mediana = median(eepacd),
            Q3 = quantile(eepacd, 0.75),
            max = max(eepacd),
            RIQ = IQR(eepacd), n = n())

summary_table_eepacd <- as_tibble(summary_table_eepacd)

formatted_table_eepacd <- summary_table_eepacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_eepacd)

summary_table_eeppvs <- indgen %>%
  group_by(NOMGEO) %>%
  summarise(media = mean(eeppvs),
            ds = sd(eeppvs),
            min = min(eeppvs),
            Q1 = quantile(eeppvs, 0.25),
            mediana = median(eeppvs),
            Q3 = quantile(eeppvs, 0.75),
            max = max(eeppvs),
            RIQ = IQR(eeppvs), n = n())

summary_table_eeppvs <- as_tibble(summary_table_eeppvs)

formatted_table_eeppvs <- summary_table_eeppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_eeppvs)


summary_table_prppvs <- indgen %>%
  group_by(NOMGEO) %>%
  summarise(media = mean(prppvs),
            ds = sd(prppvs),
            min = min(prppvs),
            Q1 = quantile(prppvs, 0.25),
            mediana = median(prppvs),
            Q3 = quantile(prppvs, 0.75),
            max = max(prppvs),
            RIQ = IQR(prppvs))

summary_table_prppvs <- as_tibble(summary_table_prppvs)

formatted_table_prppvs <- summary_table_prppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prppvs)

summary_table_prpacd <- indgen %>%
  group_by(NOMGEO) %>%
  summarise(media = mean(prpacd),
            ds = sd(prpacd),
            min = min(prpacd),
            Q1 = quantile(prpacd, 0.25),
            mediana = median(prpacd),
            Q3 = quantile(prpacd, 0.75),
            max = max(prpacd),
            RIQ = IQR(prpacd))

summary_table_prpacd <- as_tibble(summary_table_prpacd)

formatted_table_prpacd <- summary_table_prpacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prpacd)

summary_table_ee <- indgen %>%
  summarise(across(c(eeppvs:prpacd), list(
    media = mean,
    desviacion_estandar = sd,
    minimo = min,
    Q1 = ~ quantile(., 0.25),
    mediana = median,
    Q3 = ~ quantile(., 0.75),
    maximo = max,
    RIQ = IQR
  )))

long_table_ee <- summary_table_ee %>%
  pivot_longer(cols = everything(), names_to = c("variable", "statistic"), names_sep = "_")

wide_table_ee <- long_table_ee %>%
  pivot_wider(names_from = statistic, values_from = value)

formatted_table_ee <- wide_table_ee %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ee)


### Segmentation

joined_df <- left_join(indgen, mx, by = c("NOMGEO" = "NOMGEO", "CVE_GEO"="CVEGEO"))
dpsf <- st_sf(joined_df)
dpsf01 <- merge(dpsf, d01, by = "NOMGEO")

dp03 <- dpsf01 %>% filter(tcode == "2003")
dp08 <- dpsf01 %>% filter(tcode == "2008")
dp13 <- dpsf01 %>% filter(tcode == "2013")
dp18 <- dpsf01 %>% filter(tcode == "2018")

### Índices de Moran

moran.test(dp03$marppvs, pesos)
moran.test(dp03$marpacd,pesos)
moran.test(dp03$divppvs, pesos)
moran.test(dp03$divpacd,pesos)
moran.test(dp03$comppvs, pesos)
moran.test(dp03$compacd, pesos)
moran.test(dp03$prppvs, pesos)
moran.test(dp03$prpacd, pesos)
moran.test(dp03$roa, pesos)
moran.test(dp03$roic, pesos)
moran.test(dp03$dca, pesos)
moran.test(dp03$mbi, pesos)
moran.test(dp03$iaf, pesos)
moran.test(dp03$srppvs, pesos)
moran.test(dp03$srpacd, pesos)

moran.test(dp08$marppvs, pesos)
moran.test(dp08$marpacd,pesos)
moran.test(dp08$divppvs, pesos)
moran.test(dp08$divpacd,pesos)
moran.test(dp08$comppvs, pesos)
moran.test(dp08$compacd, pesos)
moran.test(dp08$prppvs, pesos)
moran.test(dp08$prpacd, pesos)
moran.test(dp08$roa, pesos)
moran.test(dp08$roic, pesos)
moran.test(dp08$dca, pesos)
moran.test(dp08$mbi, pesos)
moran.test(dp08$iaf, pesos)
moran.test(dp08$srppvs, pesos)
moran.test(dp08$srpacd, pesos)

moran.test(dp13$marppvs, pesos)
moran.test(dp13$marpacd,pesos)
moran.test(dp13$divppvs, pesos)
moran.test(dp13$divpacd,pesos)
moran.test(dp13$comppvs, pesos)
moran.test(dp13$compacd, pesos)
moran.test(dp13$prppvs, pesos)
moran.test(dp13$prpacd, pesos)
moran.test(dp13$roa, pesos)
moran.test(dp13$roic, pesos)
moran.test(dp13$dca, pesos)
moran.test(dp13$mbi, pesos)
moran.test(dp13$iaf, pesos)
moran.test(dp13$srppvs, pesos)
moran.test(dp13$srpacd, pesos)

moran.test(dp18$marppvs, pesos)
moran.test(dp18$marpacd,pesos)
moran.test(dp18$divppvs, pesos)
moran.test(dp18$divpacd,pesos)
moran.test(dp18$comppvs, pesos)
moran.test(dp18$compacd, pesos)
moran.test(dp18$prppvs, pesos)
moran.test(dp18$prpacd, pesos)
moran.test(dp18$roa, pesos)
moran.test(dp18$roic, pesos)
moran.test(dp18$dca, pesos)
moran.test(dp18$mbi, pesos)
moran.test(dp18$iaf, pesos)
moran.test(dp18$srppvs, pesos)
moran.test(dp18$srpacd, pesos)

moran.test(indgen$marppvs, W)
moran.test(indgen$marpacd, W)
moran.test(indgen$divppvs, W)
moran.test(indgen$divpacd, W)
moran.test(indgen$comppvs, W)
moran.test(indgen$compacd, W)
moran.test(indgen$prppvs, W)
moran.test(indgen$prpacd, W)
moran.test(indgen$roa, W)
moran.test(indgen$roic, W)
moran.test(indgen$dca, W)
moran.test(indgen$mbi, W)
moran.test(indgen$iaf, W)
moran.test(indgen$srppvs, W)
moran.test(indgen$srpacd, W)


### Geary's C

geary.test(dp03$marppvs, pesos)
geary.test(dp03$marpacd,pesos)
geary.test(dp03$divppvs, pesos)
geary.test(dp03$divpacd,pesos)
geary.test(dp03$comppvs, pesos)
geary.test(dp03$compacd, pesos)
geary.test(dp03$prppvs, pesos)
geary.test(dp03$prpacd, pesos)
geary.test(dp03$roa, pesos)
geary.test(dp03$roic, pesos)
geary.test(dp03$dca, pesos)
geary.test(dp03$mbi, pesos)
geary.test(dp03$iaf, pesos)
geary.test(dp03$srppvs, pesos)
geary.test(dp03$srpacd, pesos)

geary.test(dp08$marppvs, pesos)
geary.test(dp08$marpacd,pesos)
geary.test(dp08$divppvs, pesos)
geary.test(dp08$divpacd,pesos)
geary.test(dp08$comppvs, pesos)
geary.test(dp08$compacd, pesos)
geary.test(dp08$prppvs, pesos)
geary.test(dp08$prpacd, pesos)
geary.test(dp08$roa, pesos)
geary.test(dp08$roic, pesos)
geary.test(dp08$dca, pesos)
geary.test(dp08$mbi, pesos)
geary.test(dp08$iaf, pesos)
geary.test(dp08$srppvs, pesos)
geary.test(dp08$srpacd, pesos)

geary.test(dp13$marppvs, pesos)
geary.test(dp13$marpacd,pesos)
geary.test(dp13$divppvs, pesos)
geary.test(dp13$divpacd,pesos)
geary.test(dp13$comppvs, pesos)
geary.test(dp13$compacd, pesos)
geary.test(dp13$prppvs, pesos)
geary.test(dp13$prpacd, pesos)
geary.test(dp13$roa, pesos)
geary.test(dp13$roic, pesos)
geary.test(dp13$dca, pesos)
geary.test(dp13$mbi, pesos)
geary.test(dp13$iaf, pesos)
geary.test(dp13$srppvs, pesos)
geary.test(dp13$srpacd, pesos)

geary.test(dp18$marppvs, pesos)
geary.test(dp18$marpacd,pesos)
geary.test(dp18$divppvs, pesos)
geary.test(dp18$divpacd,pesos)
geary.test(dp18$comppvs, pesos)
geary.test(dp18$compacd, pesos)
geary.test(dp18$prppvs, pesos)
geary.test(dp18$prpacd, pesos)
geary.test(dp18$roa, pesos)
geary.test(dp18$roic, pesos)
geary.test(dp18$dca, pesos)
geary.test(dp18$mbi, pesos)
geary.test(dp18$iaf, pesos)
geary.test(dp18$srppvs, pesos)
geary.test(dp18$srpacd, pesos)

geary.test(indgen$marppvs, W)
geary.test(indgen$marpacd, W)
geary.test(indgen$divppvs, W)
geary.test(indgen$divpacd, W)
geary.test(indgen$comppvs, W)
geary.test(indgen$compacd, W)
geary.test(indgen$prppvs, W)
geary.test(indgen$prpacd, W)
geary.test(indgen$roa, W)
geary.test(indgen$roic, W)
geary.test(indgen$dca, W)
geary.test(indgen$mbi, W)
geary.test(indgen$iaf, W)
geary.test(indgen$srppvs, W)
geary.test(indgen$srpacd, W)


### Índice bayesiano empírico

EBImoran.mc(dp03$marppvs, dp03$prppvs, pesos, nsim = 999)
EBImoran.mc(dp03$marpacd, dp03$prpacd, pesos, nsim = 999)
EBImoran.mc(dp03$divppvs, dp03$prppvs, pesos, nsim = 999)
EBImoran.mc(dp03$divpacd, dp03$prpacd, pesos, nsim = 999)
EBImoran.mc(dp03$comppvs, dp03$prppvs, pesos, nsim = 999)
EBImoran.mc(dp03$compacd, dp03$prpacd, pesos, nsim = 999)

EBImoran.mc(dp08$marppvs, dp08$prppvs, pesos, nsim = 999)
EBImoran.mc(dp08$marpacd, dp08$prpacd, pesos, nsim = 999)
EBImoran.mc(dp08$divppvs, dp08$prppvs, pesos, nsim = 999)
EBImoran.mc(dp08$divpacd, dp08$prpacd, pesos, nsim = 999)
EBImoran.mc(dp08$comppvs, dp08$prppvs, pesos, nsim = 999)
EBImoran.mc(dp08$compacd, dp08$prpacd, pesos, nsim = 999)

EBImoran.mc(dp13$marppvs, dp13$prppvs, pesos, nsim = 999)
EBImoran.mc(dp13$marpacd, dp13$prpacd, pesos, nsim = 999)
EBImoran.mc(dp13$divppvs, dp13$prppvs, pesos, nsim = 999)
EBImoran.mc(dp13$divpacd, dp13$prpacd, pesos, nsim = 999)
EBImoran.mc(dp13$comppvs, dp13$prppvs, pesos, nsim = 999)
EBImoran.mc(dp13$compacd, dp13$prpacd, pesos, nsim = 999)

EBImoran.mc(dp18$marppvs, dp18$prppvs, pesos, nsim = 999)
EBImoran.mc(dp18$marpacd, dp18$prpacd, pesos, nsim = 999)
EBImoran.mc(dp18$divppvs, dp18$prppvs, pesos, nsim = 999)
EBImoran.mc(dp18$divpacd, dp18$prpacd, pesos, nsim = 999)
EBImoran.mc(dp18$comppvs, dp18$prppvs, pesos, nsim = 999)
EBImoran.mc(dp18$compacd, dp18$prpacd, pesos, nsim = 999)

EBImoran.mc(indgen$marppvs, indgen$prppvs, W, nsim = 999)
EBImoran.mc(indgen$marpacd, indgen$prpacd, W, nsim = 999)
EBImoran.mc(indgen$divppvs, indgen$prppvs, W, nsim = 999)
EBImoran.mc(indgen$divpacd, indgen$prpacd, W, nsim = 999)
EBImoran.mc(indgen$comppvs, indgen$prppvs, W, nsim = 999)
EBImoran.mc(indgen$compacd, indgen$prpacd, W, nsim = 999)

### LISA

lisaprpacdr <- localmoran(indgen$prpacdr, W)
indgen$lisaprpacdr <- lisaprpacdr[,1]
indgen$lisaprpacdrpv <- as.numeric(lisaprpacdr[,5]< 0.0999)

lisaprppvsr <- localmoran(indgen$prppvsr, W)
indgen$lisaprppvsr <- lisaprppvsr[,1]
indgen$lisaprppvsrpv <- as.numeric(lisaprppvsr[,5]< 0.0999)

lisamarpacd <- localmoran(indgen$marpacd, W)
indgen$lisamarpacd <- lisamarpacd[,1]
indgen$lisamarpacdpv <- as.numeric(lisamarpacd[,5]< 0.0999)

lisamarppvs <- localmoran(indgen$marppvs, W)
indgen$lisamarppvs <- lisamarppvs[,1]
indgen$lisamarppvspv <- as.numeric(lisamarppvs[,5]< 0.0999)

lisadivpacd <- localmoran(indgen$divpacd, W)
indgen$lisadivpacd <- lisadivpacd[,1]
indgen$lisadivpacdpv <- as.numeric(lisadivpacd[,5]< 0.0999)

lisadivppvs <- localmoran(indgen$divppvs, W)
indgen$lisadivppvs <- lisadivppvs[,1]
indgen$lisadivppvspv <- as.numeric(lisadivppvs[,5]< 0.0999)

lisacompacd <- localmoran(indgen$compacd, W)
indgen$lisacompacd <- lisacompacd[,1]
indgen$lisacompacdpv <- as.numeric(lisacompacd[,5]< 0.0999)

lisacomppvs <- localmoran(indgen$comppvs, W)
indgen$lisacomppvs <- lisacomppvs[,1]
indgen$lisacomppvspv <- as.numeric(lisacomppvs[,5]< 0.0999)



###### Indicadores locales de asociación espacial

### G de Getis-Ord

marppvsg <- localG(dp03$marppvs, pesos)
dp03$marppvsg <- as.numeric(marppvsg)
ggplot(dp03) +
  geom_sf(aes(fill = marppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PPVS 2003", fill = "G") +
  theme_minimal()

marpacdg <- localG(dp03$marpacd, pesos)
dp03$marpacdg <- as.numeric(marpacdg)
ggplot(dp03) +
  geom_sf(aes(fill = marpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PACD 2003", fill = "G") +
  theme_minimal()

divppvsg <- localG(dp03$divppvs, pesos)
dp03$divppvsg <- as.numeric(divppvsg)
ggplot(dp03) +
  geom_sf(aes(fill = divppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PPVS 2003", fill = "G") +
  theme_minimal()

divpacdg <- localG(dp03$divpacd, pesos)
dp03$divpacdg <- as.numeric(divpacdg)
ggplot(dp03) +
  geom_sf(aes(fill = divpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PACD 2003", fill = "G") +
  theme_minimal()

comppvsg <- localG(dp03$comppvs, pesos)
dp03$comppvsg <- as.numeric(comppvsg)
ggplot(dp03) +
  geom_sf(aes(fill = comppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PPVS 2003", fill = "G") +
  theme_minimal()

compacdg <- localG(dp03$compacd, pesos)
dp03$compacdg <- as.numeric(compacdg)
ggplot(dp03) +
  geom_sf(aes(fill = compacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PACD 2003", fill = "G") +
  theme_minimal()

prppvsg <- localG(dp03$prppvs, pesos)
dp03$prppvsg <- as.numeric(prppvsg)
ggplot(dp03) +
  geom_sf(aes(fill = prppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PPVS 2003", fill = "G") +
  theme_minimal()

prpacdg <- localG(dp03$prpacd, pesos)
dp03$prpacdg <- as.numeric(prpacdg)
ggplot(dp03) +
  geom_sf(aes(fill = prpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PACD 2003", fill = "G") +
  theme_minimal()

marppvsg <- localG(dp08$marppvs, pesos)
dp08$marppvsg <- as.numeric(marppvsg)
ggplot(dp08) +
  geom_sf(aes(fill = marppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PPVS 2008", fill = "G") +
  theme_minimal()

marpacdg <- localG(dp08$marpacd, pesos)
dp08$marpacdg <- as.numeric(marpacdg)
ggplot(dp08) +
  geom_sf(aes(fill = marpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PACD 2008", fill = "G") +
  theme_minimal()

divppvsg <- localG(dp08$divppvs, pesos)
dp08$divppvsg <- as.numeric(divppvsg)
ggplot(dp08) +
  geom_sf(aes(fill = divppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PPVS 2008", fill = "G") +
  theme_minimal()

divpacdg <- localG(dp08$divpacd, pesos)
dp08$divpacdg <- as.numeric(divpacdg)
ggplot(dp08) +
  geom_sf(aes(fill = divpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PACD 2008", fill = "G") +
  theme_minimal()

comppvsg <- localG(dp08$comppvs, pesos)
dp08$comppvsg <- as.numeric(comppvsg)
ggplot(dp08) +
  geom_sf(aes(fill = comppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PPVS 2008", fill = "G") +
  theme_minimal()

compacdg <- localG(dp08$compacd, pesos)
dp08$compacdg <- as.numeric(compacdg)
ggplot(dp08) +
  geom_sf(aes(fill = compacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PACD 2008", fill = "G") +
  theme_minimal()

prppvsg <- localG(dp08$prppvs, pesos)
dp08$prppvsg <- as.numeric(prppvsg)
ggplot(dp08) +
  geom_sf(aes(fill = prppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PPVS 2008", fill = "G") +
  theme_minimal()

prpacdg <- localG(dp08$prpacd, pesos)
dp08$prpacdg <- as.numeric(prpacdg)
ggplot(dp08) +
  geom_sf(aes(fill = prpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PACD 2008", fill = "G") +
  theme_minimal()

marppvsg <- localG(dp13$marppvs, pesos)
dp13$marppvsg <- as.numeric(marppvsg)
ggplot(dp13) +
  geom_sf(aes(fill = marppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PPVS 2013", fill = "G") +
  theme_minimal()

marpacdg <- localG(dp13$marpacd, pesos)
dp13$marpacdg <- as.numeric(marpacdg)
ggplot(dp13) +
  geom_sf(aes(fill = marpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PACD 2013", fill = "G") +
  theme_minimal()

divppvsg <- localG(dp13$divppvs, pesos)
dp13$divppvsg <- as.numeric(divppvsg)
ggplot(dp13) +
  geom_sf(aes(fill = divppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PPVS 2013", fill = "G") +
  theme_minimal()

divpacdg <- localG(dp13$divpacd, pesos)
dp13$divpacdg <- as.numeric(divpacdg)
ggplot(dp13) +
  geom_sf(aes(fill = divpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PACD 2013", fill = "G") +
  theme_minimal()

comppvsg <- localG(dp13$comppvs, pesos)
dp13$comppvsg <- as.numeric(comppvsg)
ggplot(dp13) +
  geom_sf(aes(fill = comppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PPVS 2013", fill = "G") +
  theme_minimal()

compacdg <- localG(dp13$compacd, pesos)
dp13$compacdg <- as.numeric(compacdg)
ggplot(dp13) +
  geom_sf(aes(fill = compacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PACD 2013", fill = "G") +
  theme_minimal()

prppvsg <- localG(dp13$prppvs, pesos)
dp13$prppvsg <- as.numeric(prppvsg)
ggplot(dp13) +
  geom_sf(aes(fill = prppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PPVS 2013", fill = "G") +
  theme_minimal()

prpacdg <- localG(dp13$prpacd, pesos)
dp13$prpacdg <- as.numeric(prpacdg)
ggplot(dp13) +
  geom_sf(aes(fill = prpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PACD 2013", fill = "G") +
  theme_minimal()

marppvsg <- localG(dp18$marppvs, pesos)
dp18$marppvsg <- as.numeric(marppvsg)
ggplot(dp18) +
  geom_sf(aes(fill = marppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PPVS 2018", fill = "G") +
  theme_minimal()

marpacdg <- localG(dp18$marpacd, pesos)
dp18$marpacdg <- as.numeric(marpacdg)
ggplot(dp18) +
  geom_sf(aes(fill = marpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Especialización PACD 2018", fill = "G") +
  theme_minimal()

divppvsg <- localG(dp18$divppvs, pesos)
dp18$divppvsg <- as.numeric(divppvsg)
ggplot(dp18) +
  geom_sf(aes(fill = divppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PPVS 2018", fill = "G") +
  theme_minimal()

divpacdg <- localG(dp18$divpacd, pesos)
dp18$divpacdg <- as.numeric(divpacdg)
ggplot(dp18) +
  geom_sf(aes(fill = divpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Diversidad PACD 2018", fill = "G") +
  theme_minimal()

comppvsg <- localG(dp18$comppvs, pesos)
dp18$comppvsg <- as.numeric(comppvsg)
ggplot(dp18) +
  geom_sf(aes(fill = comppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PPVS 2018", fill = "G") +
  theme_minimal()

compacdg <- localG(dp18$compacd, pesos)
dp18$compacdg <- as.numeric(compacdg)
ggplot(dp18) +
  geom_sf(aes(fill = compacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Competencia PACD 2018", fill = "G") +
  theme_minimal()

prppvsg <- localG(dp18$prppvs, pesos)
dp18$prppvsg <- as.numeric(prppvsg)
ggplot(dp18) +
  geom_sf(aes(fill = prppvsg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PPVS 2018", fill = "G") +
  theme_minimal()

prpacdg <- localG(dp18$prpacd, pesos)
dp18$prpacdg <- as.numeric(prpacdg)
ggplot(dp18) +
  geom_sf(aes(fill = prpacdg)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "G de Getis-Ord Productividad Relativa PACD 2018", fill = "G") +
  theme_minimal()

### Gráficos ganancias de productividad relativa

ppvsprr03 <- ggplot(data = dp03) +
  geom_sf(aes(fill = scale(prppvsr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2018-2003",
       fill = "PRPPVSR")

ppvsprr03

pacdprr03 <- ggplot(data = dp03) +
  geom_sf(aes(fill = scale(prpacdr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2018-2003",
       fill = "PRPACDR")

pacdprr03

ppvsprr08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = scale(prppvsr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2008-2003",
       fill = "PRPPVSR")

ppvsprr08

pacdprr08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = scale(prpacdr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2008-2003",
       fill = "PRPACDR")

pacdprr08

ppvsprr13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = scale(prppvsr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2013-2008",
       fill = "PRPPVSR")

ppvsprr13

pacdprr13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = scale(prpacdr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2013-2008",
       fill = "PRPACDR")

pacdprr13

ppvsprr18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = scale(prppvsr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2018-2013",
       fill = "PRPPVSR")

ppvsprr18

pacdprr18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = scale(prpacdr))) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_minimal() +
  labs(title = "2018-2013",
       fill = "PRPACDR")

pacdprr18

ppvsprr <- plot_grid(ppvsprr08, ppvsprr13, ppvsprr18,  nrow = 3,
                     align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ppvsprr_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento por lustro PRPPVS", fontface = 'bold', x = 0.5, hjust = 0.5),
  ppvsprr,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(ppvsprr_titled)

pacdprr <- plot_grid(pacdprr08, pacdprr13, pacdprr18,  nrow = 3,
                     align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

pacdprr_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento por lustro PRPACD", fontface = 'bold', x = 0.5, hjust = 0.5),
  pacdprr,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(pacdprr_titled)

prodgen <- plot_grid(ppvsprr_titled, pacdprr_titled,  ncol = 2,
                     align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(prodgen)

### Correlaciones 

kmeansppvs03 <- kmeans(dp03$prppvsr, centers = 8)
dp03$clusterppvs <- as.factor(kmeansppvs03$cluster)

dp03$clusterppvs

kmeanspacd03 <- kmeans(dp03$prpacdr, centers = 8)
dp03$clusterpacd <- as.factor(kmeanspacd03$cluster)

dp03$clusterpacd

kmeansppvs08 <- kmeans(dp08$prppvsr, centers = 8)
dp08$clusterppvs <- as.factor(kmeansppvs08$cluster)

kmeanspacd08 <- kmeans(dp08$prpacdr, centers = 8)
dp08$clusterpacd <- as.factor(kmeanspacd08$cluster)

kmeansppvs13 <- kmeans(dp13$prppvsr, centers = 8)
dp13$clusterppvs <- as.factor(kmeansppvs13$cluster)

kmeanspacd13 <- kmeans(dp13$prpacdr, centers = 8)
dp13$clusterpacd <- as.factor(kmeanspacd13$cluster)

kmeansppvs18 <- kmeans(dp18$prppvsr, centers = 8)
dp18$clusterppvs <- as.factor(kmeansppvs18$cluster)

kmeanspacd18 <- kmeans(dp18$prpacdr, centers = 8)
dp18$clusterpacd <- as.factor(kmeanspacd18$cluster)

prodvscomppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(comppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(comppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs08

prodvscompacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(compacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd08

prodvsdivppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(divppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs08

prodvsdivpacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(divpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd08

prodvsespppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(marppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs08

prodvsesppacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(marpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsesppacd08

prodvseejppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(eeppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(eeppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPPVSR", y = "EEPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejppvs08

prodvseejpacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(eepacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(eepacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2008",
       x = "PRPACDR", y = "EEPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejpacd08

prodvscomppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(comppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(comppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs13

prodvscompacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(compacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd13

prodvsdivppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(divppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs13

prodvsdivpacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(divpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd13

prodvsespppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(marppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs13

prodvsesppacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(marpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsesppacd13

prodvseejppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(eeppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(eeppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPPVSR", y = "EEPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejppvs13

prodvseejpacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(eepacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(eepacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2013",
       x = "PRPACDR", y = "EEPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejpacd13

prodvscomppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(comppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(comppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs18

prodvscompacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(compacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd18

prodvsdivppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(divppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs18

prodvsdivpacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(divpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd18

prodvsespppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(marppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs18

prodvsesppacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(marpacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsesppacd18

prodvseejppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(eeppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prppvsr)) > 1 | abs(scale(eeppvs)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPPVSR", y = "EEPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejppvs18

prodvseejpacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(eepacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = ifelse(abs(scale(prpacdr)) > 1 | abs(scale(eepacd)) > 1, 
                                     NOMGEO, '')), max.overlaps = 9000) +
  labs(title = "2018",
       x = "PRPACDR", y = "EEPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvseejpacd18


prodespgen <- plot_grid(prodvsesppacd08, prodvsesppacd13, prodvsesppacd18,
                        prodvsespppvs08, prodvsespppvs13, prodvsespppvs18, 
                        nrow = 2,
                        align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

prodespgen_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento de la Productividad Relativa vs Especialización", fontface = 'bold', x = 0.5, hjust = 0.5),
  prodespgen,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(prodespgen_titled)

proddivgen <- plot_grid(prodvsdivpacd08, prodvsdivpacd13, prodvsdivpacd18,
                        prodvsdivppvs08, prodvsdivppvs13, prodvsdivppvs18, 
                        nrow = 2,
                        align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

proddivgen_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento de la Productividad Relativa vs Diversidad", fontface = 'bold', x = 0.5, hjust = 0.5),
  proddivgen,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(proddivgen_titled)

prodcomgen <- plot_grid(prodvscompacd08,  prodvscompacd13, prodvscompacd18,
                        prodvscomppvs08, prodvscomppvs13, prodvscomppvs18, 
                        nrow = 2,
                        align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

prodcomgen_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento de la Productividad Relativa vs Competencia", fontface = 'bold', x = 0.5, hjust = 0.5),
  prodcomgen,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(prodcomgen_titled)

prodeejgen <- plot_grid(prodvseejpacd08,  prodvseejpacd13, prodvseejpacd18,
                        prodvseejppvs08, prodvseejppvs13, prodvseejppvs18, 
                        nrow = 2,
                        align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

prodeejgen_titled <- plot_grid(
  ggdraw() + draw_label("Crecimiento de la Productividad Relativa vs Escala Económica", fontface = 'bold', x = 0.5, hjust = 0.5),
  prodeejgen,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(prodeejgen_titled)

prodvsextppvs <- plot_grid(prodvsespppvs08, prodvsespppvs13, prodvsespppvs18,  
                           prodvsdivppvs08, prodvsdivppvs13, prodvsdivppvs18,
                           prodvscomppvs08, prodvscomppvs13, prodvscomppvs18,
                           prodvseejppvs08, prodvseejppvs13, prodvseejppvs18,
                           ncol = 3,
                           align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

print(prodvsextppvs)

prodvsextpacd <- plot_grid(prodvsesppacd08, prodvsesppacd13, prodvsesppacd18,  
                           prodvsdivpacd08, prodvsdivpacd13, prodvsdivpacd18,
                           prodvscompacd08, prodvscompacd13, prodvscompacd18,
                           prodvseejpacd08, prodvseejpacd13, prodvseejpacd18,
                           ncol = 3,
                           align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(prodvsextpacd)

### Gráficos clústers

clupacd03 <- ggplot(data = dp03) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPACD 2018-2003",
       fill = "PRPACDR")

clupacd03

cluppvs03 <- ggplot(data = dp03) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPPVS 2018-2003",
       fill = "PRPPVSR")

cluppvs03

clupacd08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPACD 2008-2003",
       fill = "PRPACDR")

clupacd08

cluppvs08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPPVS 2008-2003",
       fill = "PRPPVSR")

cluppvs08

clupacd13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPACD 2013-2008",
       fill = "PRPACDR")

clupacd13

cluppvs13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPPVS 2013-2008",
       fill = "PRPPVSR")

cluppvs13

clupacd18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPACD 2018-2013",
       fill = "PRPACDR")

clupacd18

cluppvs18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "TMCA PRPPVS 2018-2013",
       fill = "PRPPVSR")

cluppvs18

clugen <- plot_grid(clupacd08, clupacd13, clupacd18,
                    cluppvs08, cluppvs13, cluppvs18,
                    nrow = 2,
                    align = "c", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

clugen_titled <- plot_grid(
  ggdraw() + 
    draw_label("Agrupamiento con base en el Crecimiento de la Productividad Relativa", fontface = 'bold', x = 0.5, hjust = 0.5),
  clugen,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(clugen_titled)

### LISA clusters

lisaprpacdr <- localmoran(dp03$prpacdr, pesos)
dp03$lisaprpacdr <- lisaprpacdr[,1]
dp03$lisaprpacdrpv <- as.numeric(lisaprpacdr[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisaprpacdrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPACDR 2018-2003", fill = "LISA") +
  theme_minimal()

lisaprppvsr <- localmoran(dp03$prppvsr, pesos)
dp03$lisaprppvsr <- lisaprppvsr[,1]
dp03$lisaprppvsrpv <- as.numeric(lisaprppvsr[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisaprppvsrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPPVSR 2018-2003", fill = "LISA") +
  theme_minimal()

lisamarpacd <- localmoran(dp03$marpacd, pesos)
dp03$lisamarpacd <- lisamarpacd[,1]
dp03$lisamarpacdpv <- as.numeric(lisamarpacd[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisamarpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PACD 2003", fill = "LISA") +
  theme_minimal()

lisamarppvs <- localmoran(dp03$marppvs, pesos)
dp03$lisamarppvs <- lisamarppvs[,1]
dp03$lisamarppvspv <- as.numeric(lisamarppvs[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisamarppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PPVS 2003", fill = "LISA") +
  theme_minimal()

lisadivpacd <- localmoran(dp03$divpacd, pesos)
dp03$lisadivpacd <- lisadivpacd[,1]
dp03$lisadivpacdpv <- as.numeric(lisadivpacd[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisadivpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PACD 2003", fill = "LISA") +
  theme_minimal()

lisadivppvs <- localmoran(dp03$divppvs, pesos)
dp03$lisadivppvs <- lisadivppvs[,1]
dp03$lisadivppvspv <- as.numeric(lisadivppvs[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisadivppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PPVS 2003", fill = "LISA") +
  theme_minimal()

lisacompacd <- localmoran(dp03$compacd, pesos)
dp03$lisacompacd <- lisacompacd[,1]
dp03$lisacompacdpv <- as.numeric(lisacompacd[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisacompacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PACD 2003", fill = "LISA") +
  theme_minimal()

lisacomppvs <- localmoran(dp03$comppvs, pesos)
dp03$lisacomppvs <- lisacomppvs[,1]
dp03$lisacomppvspv <- as.numeric(lisacomppvs[,5]< 0.0999)

ggplot(dp03) +
  geom_sf(aes(fill = lisacomppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PPVS 2003", fill = "LISA") +
  theme_minimal()

lisaprpacdr <- localmoran(dp08$prpacdr, pesos)
dp08$lisaprpacdr <- lisaprpacdr[,1]
dp08$lisaprpacdrpv <- as.numeric(lisaprpacdr[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisaprpacdrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPACDR 2008-2003", fill = "LISA") +
  theme_minimal()

lisaprppvsr <- localmoran(dp08$prppvsr, pesos)
dp08$lisaprppvsr <- lisaprppvsr[,1]
dp08$lisaprppvsrpv <- as.numeric(lisaprppvsr[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisaprppvsrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPPVSR 2008-2003", fill = "LISA") +
  theme_minimal()

lisamarpacd <- localmoran(dp08$marpacd, pesos)
dp08$lisamarpacd <- lisamarpacd[,1]
dp08$lisamarpacdpv <- as.numeric(lisamarpacd[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisamarpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PACD 2008", fill = "LISA") +
  theme_minimal()

lisamarppvs <- localmoran(dp08$marppvs, pesos)
dp08$lisamarppvs <- lisamarppvs[,1]
dp08$lisamarppvspv <- as.numeric(lisamarppvs[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisamarppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PPVS 2008", fill = "LISA") +
  theme_minimal()

lisadivpacd <- localmoran(dp08$divpacd, pesos)
dp08$lisadivpacd <- lisadivpacd[,1]
dp08$lisadivpacdpv <- as.numeric(lisadivpacd[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisadivpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PACD 2008", fill = "LISA") +
  theme_minimal()

lisadivppvs <- localmoran(dp08$divppvs, pesos)
dp08$lisadivppvs <- lisadivppvs[,1]
dp08$lisadivppvspv <- as.numeric(lisadivppvs[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisadivppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PPVS 2008", fill = "LISA") +
  theme_minimal()

lisacompacd <- localmoran(dp08$compacd, pesos)
dp08$lisacompacd <- lisacompacd[,1]
dp08$lisacompacdpv <- as.numeric(lisacompacd[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisacompacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PACD 2008", fill = "LISA") +
  theme_minimal()

lisacomppvs <- localmoran(dp08$comppvs, pesos)
dp08$lisacomppvs <- lisacomppvs[,1]
dp08$lisacomppvspv <- as.numeric(lisacomppvs[,5]< 0.0999)

ggplot(dp08) +
  geom_sf(aes(fill = lisacomppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PPVS 2008", fill = "LISA") +
  theme_minimal()

lisaprpacdr <- localmoran(dp13$prpacdr, pesos)
dp13$lisaprpacdr <- lisaprpacdr[,1]
dp13$lisaprpacdrpv <- as.numeric(lisaprpacdr[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisaprpacdrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPACDR 2013-2008", fill = "LISA") +
  theme_minimal()

lisaprppvsr <- localmoran(dp13$prppvsr, pesos)
dp13$lisaprppvsr <- lisaprppvsr[,1]
dp13$lisaprppvsrpv <- as.numeric(lisaprppvsr[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisaprppvsrpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA PRPPVSR 2013-2008", fill = "LISA") +
  theme_minimal()

lisamarpacd <- localmoran(dp13$marpacd, pesos)
dp13$lisamarpacd <- lisamarpacd[,1]
dp13$lisamarpacdpv <- as.numeric(lisamarpacd[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisamarpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PACD 2013", fill = "LISA") +
  theme_minimal()

lisamarppvs <- localmoran(dp13$marppvs, pesos)
dp13$lisamarppvs <- lisamarppvs[,1]
dp13$lisamarppvspv <- as.numeric(lisamarppvs[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisamarppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Especialización PPVS 2013", fill = "LISA") +
  theme_minimal()

lisadivpacd <- localmoran(dp13$divpacd, pesos)
dp13$lisadivpacd <- lisadivpacd[,1]
dp13$lisadivpacdpv <- as.numeric(lisadivpacd[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisadivpacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PACD 2013", fill = "LISA") +
  theme_minimal()

lisadivppvs <- localmoran(dp13$divppvs, pesos)
dp13$lisadivppvs <- lisadivppvs[,1]
dp13$lisadivppvspv <- as.numeric(lisadivppvs[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisadivppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PPVS 2013", fill = "LISA") +
  theme_minimal()

lisacompacd <- localmoran(dp13$compacd, pesos)
dp13$lisacompacd <- lisacompacd[,1]
dp13$lisacompacdpv <- as.numeric(lisacompacd[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisacompacdpv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PACD 2013", fill = "LISA") +
  theme_minimal()

lisacomppvs <- localmoran(dp13$comppvs, pesos)
dp13$lisacomppvs <- lisacomppvs[,1]
dp13$lisacomppvspv <- as.numeric(lisacomppvs[,5]< 0.0999)

ggplot(dp13) +
  geom_sf(aes(fill = lisacomppvspv)) +
  scale_fill_gradient(low = "#132B43", high = "red") +
  labs(title = "Clusters LISA Competencia PPVS 2013", fill = "LISA") +
  theme_minimal()

lisaprpacdr <- localmoran(dp18$prpacdr, pesos)
dp18$lisaprpacdr <- lisaprpacdr[,1]
dp18$lisaprpacdrpv <- as.numeric(lisaprpacdr[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisaprpacdrpv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA PRPACDR 2018-2013", fill = "LISA") +
  theme_minimal()

lisaprppvsr <- localmoran(dp18$prppvsr, pesos)
dp18$lisaprppvsr <- lisaprppvsr[,1]
dp18$lisaprppvsrpv <- as.numeric(lisaprppvsr[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisaprppvsrpv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA PRPPVSR 2018-2013", fill = "LISA") +
  theme_minimal()

lisamarpacd <- localmoran(dp18$marpacd, pesos)
dp18$lisamarpacd <- lisamarpacd[,1]
dp18$lisamarpacdpv <- as.numeric(lisamarpacd[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisamarpacdpv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Especialización PACD 2018", fill = "LISA") +
  theme_minimal()

lisamarppvs <- localmoran(dp18$marppvs, pesos)
dp18$lisamarppvs <- lisamarppvs[,1]
dp18$lisamarppvspv <- as.numeric(lisamarppvs[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisamarppvspv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Especialización PPVS 2018", fill = "LISA") +
  theme_minimal()

lisadivpacd <- localmoran(dp18$divpacd, pesos)
dp18$lisadivpacd <- lisadivpacd[,1]
dp18$lisadivpacdpv <- as.numeric(lisadivpacd[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisadivpacdpv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PACD 2018", fill = "LISA") +
  theme_minimal()

lisadivppvs <- localmoran(dp18$divppvs, pesos)
dp18$lisadivppvs <- lisadivppvs[,1]
dp18$lisadivppvspv <- as.numeric(lisadivppvs[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisadivppvspv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Diversidad PPVS 2018", fill = "LISA") +
  theme_minimal()

lisacompacd <- localmoran(dp18$compacd, pesos)
dp18$lisacompacd <- lisacompacd[,1]
dp18$lisacompacdpv <- as.numeric(lisacompacd[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisacompacdpv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Competencia PACD 2018", fill = "LISA") +
  theme_minimal()

lisacomppvs <- localmoran(dp18$comppvs, pesos)
dp18$lisacomppvs <- lisacomppvs[,1]
dp18$lisacomppvspv <- as.numeric(lisacomppvs[,5]< 0.0999)

ggplot(dp18) +
  geom_sf(aes(fill = lisacomppvspv)) +
  scale_fill_gradient(low = "#182B43", high = "red") +
  labs(title = "Clusters LISA Competencia PPVS 2018", fill = "LISA") +
  theme_minimal()

### Gráficos de red círcular

ind03 <- indgen %>% filter(tcode == "2003")
ind08 <- indgen %>% filter(tcode == "2008")
ind13 <- indgen %>% filter(tcode == "2013")
ind18 <- indgen %>% filter(tcode == "2018")



pecpp03 <- ind03 %>% select(NOMGEO, eeppvs:prpacd)
pecpp03e <- as.data.frame(scale(pecpp03[,-1]))
rownames(pecpp03e) <- pecpp03$NOMGEO                         
pecpp03 <- as.matrix(pecpp03e)
pecpp08 <- ind08 %>% select(NOMGEO, eeppvs:prpacd)
pecpp08e <- as.data.frame(scale(pecpp08[,-1]))
rownames(pecpp08e) <- pecpp08$NOMGEO 
pecpp08 <- as.matrix(pecpp08e)
pecpp13 <- ind13 %>% select(NOMGEO, eeppvs:prpacd)
pecpp13e <- as.data.frame(scale(pecpp13[,-1]))
rownames(pecpp13e) <- pecpp13$NOMGEO 
pecpp13 <- as.matrix(pecpp13e)
pecpp18 <- ind18 %>% select(NOMGEO, eeppvs:prpacd)
pecpp18e <- as.data.frame(scale(pecpp18[,-1]))
rownames(pecpp18e) <- pecpp18$NOMGEO 
pecpp18 <- as.matrix(pecpp18e)

par(mfrow=c(2,2))

chordDiagram(pecpp03, annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

title(main = "2003", line = 0)

chordDiagram(pecpp08, annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

title(main = "2008", line = 0)

chordDiagram(pecpp13, annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

title(main = "2013", line = 0)

chordDiagram(pecpp18, annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

title(main = "2018", line = 0)

circos.clear()



