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



data <- bin

data[is.na(data)] <- 0

summary_table_pacd <- data %>%
  group_by(NOMGEO, tcode) %>%
  summarise(media = mean(pacd),
            ds = sd(pacd),
            min = min(pacd),
            Q1 = quantile(pacd, 0.25),
            mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            max = max(pacd),
            RIQ = IQR(pacd), n = n())

summary_table_pacd <- as_tibble(summary_table_pacd)

formatted_table_pacd <- summary_table_pacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pacd)

s_t_pacd <- data %>%
  group_by(AE, tcode) %>%
  summarise(media = mean(pacd),
            ds = sd(pacd),
            min = min(pacd),
            Q1 = quantile(pacd, 0.25),
            mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            max = max(pacd),
            RIQ = IQR(pacd), n = n())

s_t_pacd <- as_tibble(s_t_pacd)

f_t_pacd <- s_t_pacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_t_pacd)

summary_table_ppvs <- data %>%
  group_by(NOMGEO, tcode) %>%
  summarise(media = mean(ppvs),
            ds = sd(ppvs),
            min = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            max = max(ppvs),
            RIQ = IQR(ppvs), n = n())

summary_table_ppvs <- as_tibble(summary_table_ppvs)

formatted_table_ppvs <- summary_table_ppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ppvs)

s_t_ppvs <- data %>%
  group_by(AE, tcode) %>%
  summarise(media = mean(ppvs),
            ds = sd(ppvs),
            min = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            max = max(ppvs),
            RIQ = IQR(ppvs), n = n())

s_t_ppvs <- as_tibble(s_t_ppvs)

f_t_ppvs <- s_t_ppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_t_ppvs)

summary_table_vacb <- data %>%
  group_by(NOMGEO, tcode) %>%
  summarise(media = mean(vacb),
            ds = sd(vacb),
            min = min(vacb),
            Q1 = quantile(vacb, 0.25),
            mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            max = max(vacb),
            RIQ = IQR(vacb), n = n())

summary_table_vacb <- as_tibble(summary_table_vacb)

formatted_table_vacb <- summary_table_vacb %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_vacb)

s_t_vacb <- data %>%
  group_by(AE, tcode) %>%
  summarise(media = mean(vacb),
            ds = sd(vacb),
            min = min(vacb),
            Q1 = quantile(vacb, 0.25),
            mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            max = max(vacb),
            RIQ = IQR(vacb), n = n())

s_t_vacb <- as_tibble(s_t_vacb)

f_t_vacb <- s_t_vacb %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_t_vacb)

summary_table_pbt <- data %>%
  group_by(NOMGEO, tcode) %>%
  summarise(media = mean(pbt),
            ds = sd(pbt),
            min = min(pbt),
            Q1 = quantile(pbt, 0.25),
            mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            max = max(pbt),
            RIQ = IQR(pbt), n = n())

summary_table_pbt <- as_tibble(summary_table_pbt)

formatted_table_pbt <- summary_table_pbt %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pbt)

s_t_pbt <- data %>%
  group_by(AE, tcode) %>%
  summarise(media = mean(pbt),
            ds = sd(pbt),
            min = min(pbt),
            Q1 = quantile(pbt, 0.25),
            mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            max = max(pbt),
            RIQ = IQR(pbt), n = n())

s_t_pbt <- as_tibble(s_t_pbt)

f_t_pbt <- s_t_pbt %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_t_pbt)

s_table <- data %>%
  group_by(tcode) %>%
  summarise(across(c(pacd, ppvs, vacb, pbt), list(
    Media = ~ mean(., na.rm = TRUE),
    DS = ~ sd(., na.rm = TRUE),
    Mínimo = ~ min(., na.rm = TRUE),
    Q1 = ~ quantile(., 0.25, na.rm = TRUE),
    Mediana = ~ median(., na.rm = TRUE),
    Q3 = ~ quantile(., 0.75, na.rm = TRUE),
    Máximo = ~ max(., na.rm = TRUE),
    RIQ = ~ IQR(., na.rm = TRUE)
  )))

l_table <- s_table %>%
  pivot_longer(cols = -tcode, names_to = c("variable", "statistic"), names_sep = "_")

w_table <- l_table %>%
  pivot_wider(names_from = statistic, values_from = value)

f_table <- w_table %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_table)

correlation_matrix <- data %>%
  select(pacd, ppvs, vacb, pbt) %>%
  cor(use = "complete.obs")
correlation_df <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_df) <- c("variable1", "variable2", "correlation")

f_correlation_table <- correlation_df %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(f_correlation_table)

colnames(correlation_df) <- c("Variable1", "Variable2", "Correlation")

ggplot(data = correlation_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Matriz de Correlaciones", x = "Variable", y = "Variable")

### Segmentación en quinquenios

c03 <- data %>% filter(tcode == "2003")
c08 <- data %>% filter(tcode == "2008")
c13 <- data %>% filter(tcode == "2013")
c18 <- data %>% filter(tcode == "2018")

### Cálculo de razones básicas por fila y columna
ppvs03g <- c03 %>%
  select(AE, NOMGEO, ppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = ppvs)

### Configurar AE como nombres de fila
ppvs03 <- as.data.frame(ppvs03g)
rownames(ppvs03) <- ppvs03g$AE
ppvs03 <- ppvs03 %>% select(-AE)

### Calcular los totales de filas
ppvs03_total_fila <- rowSums(ppvs03, na.rm = TRUE)

### Calcular los totales de columnas
ppvs03_total_col <- colSums(ppvs03, na.rm = TRUE)

### Calcular el total general
ppvs03_total_general <- sum(ppvs03_total_fila, na.rm = TRUE)

### Dividir cada observación entre el total de las filas
ppvs03_norm_fila <- as.data.frame(sapply(1:ncol(ppvs03[,]), function(i) {
  ppvs03[,i] / ppvs03_total_fila}))

### Dividir cada observación entre el total de las columnas
ppvs03_norm_col <- as.data.frame(sapply(1:ncol(ppvs03[, ]), function(i) {
  ppvs03[,i ] / ppvs03_total_col[i]}))

### Dividir cada observación entre el total general
ppvs03__norm_total_fila <- ppvs03_norm_fila / ppvs03_total_general
ppvs03__norm_total_col <- ppvs03_norm_col / ppvs03_total_general

ppvs03i_n <- ppvs03_total_fila/ppvs03_total_general
ppvs03r_n <- ppvs03_total_col/ppvs03_total_general

pacd03g <- c03 %>%
  select(AE, NOMGEO, pacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = pacd)

pacd03 <- as.data.frame(pacd03g)
rownames(pacd03) <- pacd03g$AE
pacd03 <- pacd03 %>% select(-AE)

pacd03_total_fila <- rowSums(pacd03, na.rm = TRUE)

pacd03_total_col <- colSums(pacd03, na.rm = TRUE)

pacd03_total_general <- sum(pacd03_total_fila, na.rm = TRUE)

pacd03_norm_fila <- as.data.frame(sapply(1:ncol(pacd03[,]), function(i) {
  pacd03[,i] / pacd03_total_fila}))

pacd03_norm_col <- as.data.frame(sapply(1:ncol(pacd03[, ]), function(i) {
  pacd03[,i ] / pacd03_total_col[i]}))

pacd03__norm_total_fila <- pacd03_norm_fila / pacd03_total_general
pacd03__norm_total_col <- pacd03_norm_col / pacd03_total_general

pacd03i_n <- pacd03_total_fila/pacd03_total_general
pacd03r_n <- pacd03_total_col/pacd03_total_general

vacb03g <- c03 %>%
  select(AE, NOMGEO, vacb) %>%
  pivot_wider(names_from = NOMGEO, values_from = vacb)

vacb03 <- as.data.frame(vacb03g)
rownames(vacb03) <- vacb03g$AE
vacb03 <- vacb03 %>% select(-AE)

vacb03_total_fila <- rowSums(vacb03, na.rm = TRUE)

vacb03_total_col <- colSums(vacb03, na.rm = TRUE)

vacb03_total_general <- sum(vacb03_total_fila, na.rm = TRUE)

vacb03_norm_fila <- as.data.frame(sapply(1:ncol(vacb03[,]), function(i) {
  vacb03[,i] / vacb03_total_fila}))

vacb03_norm_col <- as.data.frame(sapply(1:ncol(vacb03[, ]), function(i) {
  vacb03[,i ] / vacb03_total_col[i]}))

vacb03__norm_total_fila <- vacb03_norm_fila / vacb03_total_general
vacb03__norm_total_col <- vacb03_norm_col / vacb03_total_general

vacb03i_n <- vacb03_total_fila/vacb03_total_general
vacb03r_n <- vacb03_total_col/vacb03_total_general

ue03g <- c03 %>%
  select(AE, NOMGEO, ue) %>%
  pivot_wider(names_from = NOMGEO, values_from = ue)

ue03 <- as.data.frame(ue03g)
rownames(ue03) <- ue03g$AE
ue03 <- ue03 %>% select(-AE)

ue03_total_fila <- rowSums(ue03, na.rm = TRUE)

ue03_total_col <- colSums(ue03, na.rm = TRUE)

ue03_total_general <- sum(ue03_total_fila, na.rm = TRUE)

ue03_norm_fila <- as.data.frame(sapply(1:ncol(ue03[,]), function(i) {
  ue03[,i] / ue03_total_fila}))

ue03_norm_col <- as.data.frame(sapply(1:ncol(ue03[, ]), function(i) {
  ue03[,i ] / ue03_total_col[i]}))

ue03__norm_total_fila <- ue03_norm_fila / ue03_total_general
ue03__norm_total_col <- ue03_norm_col / ue03_total_general

ue03i_n <- ue03_total_fila/ue03_total_general
ue03r_n <- ue03_total_col/ue03_total_general

tsppvs03g <- c03 %>%
  select(AE, NOMGEO, tsppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = tsppvs)

tsppvs03 <- as.data.frame(tsppvs03g)
rownames(tsppvs03) <- tsppvs03g$AE
tsppvs03 <- tsppvs03 %>% select(-AE)

tsppvs03_total_fila <- rowSums(tsppvs03, na.rm = T)

tsppvs03_total_col <- colSums(tsppvs03, na.rm = T)

tsppvs03_total_general <- sum(tsppvs03_total_fila, na.rm = T)

tsppvs03_norm_fila <- as.data.frame(sapply(1:ncol(tsppvs03[,]), function(i) {
  tsppvs03[,i] / tsppvs03_total_fila}))

tsppvs03_norm_col <- as.data.frame(sapply(1:ncol(tsppvs03[, ]), function(i) {
  tsppvs03[,i ] / tsppvs03_total_col[i]}))

tsppvs03__norm_total_fila <- tsppvs03_norm_fila / tsppvs03_total_general
tsppvs03__norm_total_col <- tsppvs03_norm_col / tsppvs03_total_general

tsppvs03i_n <- tsppvs03_total_fila/tsppvs03_total_general
tsppvs03r_n <- tsppvs03_total_col/tsppvs03_total_general

tspacd03g <- c03 %>%
  select(AE, NOMGEO, tspacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = tspacd)

tspacd03 <- as.data.frame(tspacd03g)
rownames(tspacd03) <- tspacd03g$AE
tspacd03 <- tspacd03 %>% select(-AE)

tspacd03_total_fila <- rowSums(tspacd03, na.rm = T)

tspacd03_total_col <- colSums(tspacd03, na.rm = T)

tspacd03_total_general <- sum(tspacd03_total_fila, na.rm = T)

tspacd03_norm_fila <- as.data.frame(sapply(1:ncol(tspacd03[,]), function(i) {
  tspacd03[,i] / tspacd03_total_fila}))

tspacd03_norm_col <- as.data.frame(sapply(1:ncol(tspacd03[, ]), function(i) {
  tspacd03[,i ] / tspacd03_total_col[i]}))

tspacd03__norm_total_fila <- tspacd03_norm_fila / tspacd03_total_general
tspacd03__norm_total_col <- tspacd03_norm_col / tspacd03_total_general

tspacd03i_n <- tspacd03_total_fila/tspacd03_total_general
tspacd03r_n <- tspacd03_total_col/tspacd03_total_general

ataf03g <- c03 %>%
  select(AE, NOMGEO, ataf) %>%
  pivot_wider(names_from = NOMGEO, values_from = ataf)

ataf03 <- as.data.frame(ataf03g)
rownames(ataf03) <- ataf03g$AE
ataf03 <- ataf03 %>% select(-AE)

ataf03_total_fila <- rowSums(ataf03, na.rm = T)

ataf03_total_col <- colSums(ataf03, na.rm = T)

ataf03_total_general <- sum(ataf03_total_fila, na.rm = T)

ataf03_norm_fila <- as.data.frame(sapply(1:ncol(ataf03[,]), function(i) {
  ataf03[,i] / ataf03_total_fila}))

ataf03_norm_col <- as.data.frame(sapply(1:ncol(ataf03[, ]), function(i) {
  ataf03[,i ] / ataf03_total_col[i]}))

ataf03__norm_total_fila <- ataf03_norm_fila / ataf03_total_general
ataf03__norm_total_col <- ataf03_norm_col / ataf03_total_general

ataf03i_n <- ataf03_total_fila/ataf03_total_general
ataf03r_n <- ataf03_total_col/ataf03_total_general

dtaf03g <- c03 %>%
  select(AE, NOMGEO, dtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = dtaf)

dtaf03 <- as.data.frame(dtaf03g)
rownames(dtaf03) <- dtaf03g$AE
dtaf03 <- dtaf03 %>% select(-AE)

dtaf03_total_fila <- rowSums(dtaf03, na.rm = T)

dtaf03_total_col <- colSums(dtaf03, na.rm = T)

dtaf03_total_general <- sum(dtaf03_total_fila, na.rm = T)

dtaf03_norm_fila <- as.data.frame(sapply(1:ncol(dtaf03[,]), function(i) {
  dtaf03[,i] / dtaf03_total_fila}))

dtaf03_norm_col <- as.data.frame(sapply(1:ncol(dtaf03[, ]), function(i) {
  dtaf03[,i ] / dtaf03_total_col[i]}))

dtaf03__norm_total_fila <- dtaf03_norm_fila / dtaf03_total_general
dtaf03__norm_total_col <- dtaf03_norm_col / dtaf03_total_general

dtaf03i_n <- dtaf03_total_fila/dtaf03_total_general
dtaf03r_n <- dtaf03_total_col/dtaf03_total_general

tga03g <- c03 %>%
  select(AE, NOMGEO, tga) %>%
  pivot_wider(names_from = NOMGEO, values_from = tga)

tga03 <- as.data.frame(tga03g)
rownames(tga03) <- tga03g$AE
tga03 <- tga03 %>% select(-AE)

tga03_total_fila <- rowSums(tga03, na.rm = T)

tga03_total_col <- colSums(tga03, na.rm = T)

tga03_total_general <- sum(tga03_total_fila, na.rm = T)

tga03_norm_fila <- as.data.frame(sapply(1:ncol(tga03[,]), function(i) {
  tga03[,i] / tga03_total_fila}))

tga03_norm_col <- as.data.frame(sapply(1:ncol(tga03[, ]), function(i) {
  tga03[,i ] / tga03_total_col[i]}))

tga03__norm_total_fila <- tga03_norm_fila / tga03_total_general
tga03__norm_total_col <- tga03_norm_col / tga03_total_general

tga03i_n <- tga03_total_fila/tga03_total_general
tga03r_n <- tga03_total_col/tga03_total_general

tin03g <- c03 %>%
  select(AE, NOMGEO, tin) %>%
  pivot_wider(names_from = NOMGEO, values_from = tin)

tin03 <- as.data.frame(tin03g)
rownames(tin03) <- tin03g$AE
tin03 <- tin03 %>% select(-AE)

tin03_total_fila <- rowSums(tin03, na.rm = T)

tin03_total_col <- colSums(tin03, na.rm = T)

tin03_total_general <- sum(tin03_total_fila, na.rm = T)

tin03_norm_fila <- as.data.frame(sapply(1:ncol(tin03[,]), function(i) {
  tin03[,i] / tin03_total_fila}))

tin03_norm_col <- as.data.frame(sapply(1:ncol(tin03[, ]), function(i) {
  tin03[,i ] / tin03_total_col[i]}))

tin03__norm_total_fila <- tin03_norm_fila / tin03_total_general
tin03__norm_total_col <- tin03_norm_col / tin03_total_general

tin03i_n <- tin03_total_fila/tin03_total_general
tin03r_n <- tin03_total_col/tin03_total_general

pbt03g <- c03 %>%
  select(AE, NOMGEO, pbt) %>%
  pivot_wider(names_from = NOMGEO, values_from = pbt)

pbt03 <- as.data.frame(pbt03g)
rownames(pbt03) <- pbt03g$AE
pbt03 <- pbt03 %>% select(-AE)

pbt03_total_fila <- rowSums(pbt03, na.rm = T)

pbt03_total_col <- colSums(pbt03, na.rm = T)

pbt03_total_general <- sum(pbt03_total_fila, na.rm = T)

pbt03_norm_fila <- as.data.frame(sapply(1:ncol(pbt03[,]), function(i) {
  pbt03[,i] / pbt03_total_fila}))

pbt03_norm_col <- as.data.frame(sapply(1:ncol(pbt03[, ]), function(i) {
  pbt03[,i ] / pbt03_total_col[i]}))

pbt03__norm_total_fila <- pbt03_norm_fila / pbt03_total_general
pbt03__norm_total_col <- pbt03_norm_col / pbt03_total_general

pbt03i_n <- pbt03_total_fila/pbt03_total_general
pbt03r_n <- pbt03_total_col/pbt03_total_general

it03g <- c03 %>%
  select(AE, NOMGEO, it) %>%
  pivot_wider(names_from = NOMGEO, values_from = it)

it03 <- as.data.frame(it03g)
rownames(it03) <- it03g$AE
it03 <- it03 %>% select(-AE)

it03_total_fila <- rowSums(it03, na.rm = T)

it03_total_col <- colSums(it03, na.rm = T)

it03_total_general <- sum(it03_total_fila, na.rm = T)

it03_norm_fila <- as.data.frame(sapply(1:ncol(it03[,]), function(i) {
  it03[,i] / it03_total_fila}))

it03_norm_col <- as.data.frame(sapply(1:ncol(it03[, ]), function(i) {
  it03[,i ] / it03_total_col[i]}))

it03__norm_total_fila <- it03_norm_fila / it03_total_general
it03__norm_total_col <- it03_norm_col / it03_total_general

it03i_n <- it03_total_fila/it03_total_general
it03r_n <- it03_total_col/it03_total_general

pot03g <- c03 %>%
  select(AE, NOMGEO, pot) %>%
  pivot_wider(names_from = NOMGEO, values_from = pot)

pot03 <- as.data.frame(pot03g)
rownames(pot03) <- pot03g$AE
pot03 <- pot03 %>% select(-AE)

pot03_total_fila <- rowSums(pot03, na.rm = T)

pot03_total_col <- colSums(pot03, na.rm = T)

pot03_total_general <- sum(pot03_total_fila, na.rm = T)

pot03_norm_fila <- as.data.frame(sapply(1:ncol(pot03[,]), function(i) {
  pot03[,i] / pot03_total_fila}))

pot03_norm_col <- as.data.frame(sapply(1:ncol(pot03[, ]), function(i) {
  pot03[,i ] / pot03_total_col[i]}))

pot03__norm_total_fila <- pot03_norm_fila / pot03_total_general
pot03__norm_total_col <- pot03_norm_col / pot03_total_general

pot03i_n <- pot03_total_fila/pot03_total_general
pot03r_n <- pot03_total_col/pot03_total_general

htpot03g <- c03 %>%
  select(AE, NOMGEO, htpot) %>%
  pivot_wider(names_from = NOMGEO, values_from = htpot)

htpot03 <- as.data.frame(htpot03g)
rownames(htpot03) <- htpot03g$AE
htpot03 <- htpot03 %>% select(-AE)

htpot03_total_fila <- rowSums(htpot03, na.rm = T)

htpot03_total_col <- colSums(htpot03, na.rm = T)

htpot03_total_general <- sum(htpot03_total_fila, na.rm = T)

htpot03_norm_fila <- as.data.frame(sapply(1:ncol(htpot03[,]), function(i) {
  htpot03[,i] / htpot03_total_fila}))

htpot03_norm_col <- as.data.frame(sapply(1:ncol(htpot03[, ]), function(i) {
  htpot03[,i ] / htpot03_total_col[i]}))

htpot03__norm_total_fila <- htpot03_norm_fila / htpot03_total_general
htpot03__norm_total_col <- htpot03_norm_col / htpot03_total_general

htpot03i_n <- htpot03_total_fila/htpot03_total_general
htpot03r_n <- htpot03_total_col/htpot03_total_general

vtaf03g <- c03 %>%
  select(AE, NOMGEO, vtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = vtaf)
vtaf03 <- as.data.frame(vtaf03g)
rownames(vtaf03) <- vtaf03g$AE
vtaf03 <- vtaf03 %>% select(-AE)
vtaf03_total_fila <- rowSums(vtaf03, na.rm = T)
vtaf03_total_col <- colSums(vtaf03, na.rm = T)
vtaf03_total_general <- sum(vtaf03_total_fila, na.rm = T)
vtaf03_norm_fila <- as.data.frame(sapply(1:ncol(vtaf03[,]), function(i) {
  vtaf03[,i] / vtaf03_total_fila}))
vtaf03_norm_col <- as.data.frame(sapply(1:ncol(vtaf03[, ]), function(i) {
  vtaf03[,i ] / vtaf03_total_col[i]}))
vtaf03__norm_total_fila <- vtaf03_norm_fila / vtaf03_total_general
vtaf03__norm_total_col <- vtaf03_norm_col / vtaf03_total_general

vtaf03i_n <- vtaf03_total_fila/vtaf03_total_general
vtaf03r_n <- vtaf03_total_col/vtaf03_total_general

afpp03g <- c03 %>%
  select(AE, NOMGEO, afpp) %>%
  pivot_wider(names_from = NOMGEO, values_from = afpp)
afpp03 <- as.data.frame(afpp03g)
rownames(afpp03) <- afpp03g$AE
afpp03 <- afpp03 %>% select(-AE)
afpp03_total_fila <- rowSums(afpp03, na.rm = T)
afpp03_total_col <- colSums(afpp03, na.rm = T)
afpp03_total_general <- sum(afpp03_total_fila, na.rm = T)
afpp03_norm_fila <- as.data.frame(sapply(1:ncol(afpp03[,]), function(i) {
  afpp03[,i] / afpp03_total_fila}))
afpp03_norm_col <- as.data.frame(sapply(1:ncol(afpp03[, ]), function(i) {
  afpp03[,i ] / afpp03_total_col[i]}))
afpp03__norm_total_fila <- afpp03_norm_fila / afpp03_total_general
afpp03__norm_total_col <- afpp03_norm_col / afpp03_total_general

afpp03i_n <- afpp03_total_fila/afpp03_total_general
afpp03r_n <- afpp03_total_col/afpp03_total_general

atmep03g <- c03 %>%
  select(AE, NOMGEO, atmep) %>%
  pivot_wider(names_from = NOMGEO, values_from = atmep)
atmep03 <- as.data.frame(atmep03g)
rownames(atmep03) <- atmep03g$AE
atmep03 <- atmep03 %>% select(-AE)
atmep03_total_fila <- rowSums(atmep03, na.rm = T)
atmep03_total_col <- colSums(atmep03, na.rm = T)
atmep03_total_general <- sum(atmep03_total_fila, na.rm = T)
atmep03_norm_fila <- as.data.frame(sapply(1:ncol(atmep03[,]), function(i) {
  atmep03[,i] / atmep03_total_fila}))
atmep03_norm_col <- as.data.frame(sapply(1:ncol(atmep03[, ]), function(i) {
  atmep03[,i ] / atmep03_total_col[i]}))
atmep03__norm_total_fila <- atmep03_norm_fila / atmep03_total_general
atmep03__norm_total_col <- atmep03_norm_col / atmep03_total_general

atmep03i_n <- atmep03_total_fila/atmep03_total_general
atmep03r_n <- atmep03_total_col/atmep03_total_general

atbi03g <- c03 %>%
  select(AE, NOMGEO, atbi) %>%
  pivot_wider(names_from = NOMGEO, values_from = atbi)
atbi03 <- as.data.frame(atbi03g)
rownames(atbi03) <- atbi03g$AE
atbi03 <- atbi03 %>% select(-AE)
atbi03_total_fila <- rowSums(atbi03, na.rm = T)
atbi03_total_col <- colSums(atbi03, na.rm = T)
atbi03_total_general <- sum(atbi03_total_fila, na.rm = T)
atbi03_norm_fila <- as.data.frame(sapply(1:ncol(atbi03[,]), function(i) {
  atbi03[,i] / atbi03_total_fila}))
atbi03_norm_col <- as.data.frame(sapply(1:ncol(atbi03[, ]), function(i) {
  atbi03[,i ] / atbi03_total_col[i]}))
atbi03__norm_total_fila <- atbi03_norm_fila / atbi03_total_general
atbi03__norm_total_col <- atbi03_norm_col / atbi03_total_general

atbi03i_n <- atbi03_total_fila/atbi03_total_general
atbi03r_n <- atbi03_total_col/atbi03_total_general

atuet03g <- c03 %>%
  select(AE, NOMGEO, atuet) %>%
  pivot_wider(names_from = NOMGEO, values_from = atuet)
atuet03 <- as.data.frame(atuet03g)
rownames(atuet03) <- atuet03g$AE
atuet03 <- atuet03 %>% select(-AE)
atuet03_total_fila <- rowSums(atuet03, na.rm = T)
atuet03_total_col <- colSums(atuet03, na.rm = T)
atuet03_total_general <- sum(atuet03_total_fila, na.rm = T)
atuet03_norm_fila <- as.data.frame(sapply(1:ncol(atuet03[,]), function(i) {
  atuet03[,i] / atuet03_total_fila}))
atuet03_norm_col <- as.data.frame(sapply(1:ncol(atuet03[, ]), function(i) {
  atuet03[,i ] / atuet03_total_col[i]}))
atuet03__norm_total_fila <- atuet03_norm_fila / atuet03_total_general
atuet03__norm_total_col <- atuet03_norm_col / atuet03_total_general

atuet03i_n <- atuet03_total_fila/atuet03_total_general
atuet03r_n <- atuet03_total_col/atuet03_total_general

atecp03g <- c03 %>%
  select(AE, NOMGEO, atecp) %>%
  pivot_wider(names_from = NOMGEO, values_from = atecp)
atecp03 <- as.data.frame(atecp03g)
rownames(atecp03) <- atecp03g$AE
atecp03 <- atecp03 %>% select(-AE)
atecp03_total_fila <- rowSums(atecp03, na.rm = T)
atecp03_total_col <- colSums(atecp03, na.rm = T)
atecp03_total_general <- sum(atecp03_total_fila, na.rm = T)
atecp03_norm_fila <- as.data.frame(sapply(1:ncol(atecp03[,]), function(i) {
  atecp03[,i] / atecp03_total_fila}))
atecp03_norm_col <- as.data.frame(sapply(1:ncol(atecp03[, ]), function(i) {
  atecp03[,i ] / atecp03_total_col[i]}))
atecp03__norm_total_fila <- atecp03_norm_fila / atecp03_total_general
atecp03__norm_total_col <- atecp03_norm_col / atecp03_total_general

atecp03i_n <- atecp03_total_fila/atecp03_total_general
atecp03r_n <- atecp03_total_col/atecp03_total_general

atma03g <- c03 %>%
  select(AE, NOMGEO, atma) %>%
  pivot_wider(names_from = NOMGEO, values_from = atma)
atma03 <- as.data.frame(atma03g)
rownames(atma03) <- atma03g$AE
atma03 <- atma03 %>% select(-AE)
atma03_total_fila <- rowSums(atma03, na.rm = T)
atma03_total_col <- colSums(atma03, na.rm = T)
atma03_total_general <- sum(atma03_total_fila, na.rm = T)
atma03_norm_fila <- as.data.frame(sapply(1:ncol(atma03[,]), function(i) {
  atma03[,i] / atma03_total_fila}))
atma03_norm_col <- as.data.frame(sapply(1:ncol(atma03[, ]), function(i) {
  atma03[,i ] / atma03_total_col[i]}))
atma03__norm_total_fila <- atma03_norm_fila / atma03_total_general
atma03__norm_total_col <- atma03_norm_col / atma03_total_general

atma03i_n <- atma03_total_fila/atma03_total_general
atma03r_n <- atma03_total_col/atma03_total_general

ppvs08g <- c08 %>%
  select(AE, NOMGEO, ppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = ppvs)

ppvs08 <- as.data.frame(ppvs08g)
rownames(ppvs08) <- ppvs08g$AE
ppvs08 <- ppvs08 %>% select(-AE)

ppvs08_total_fila <- rowSums(ppvs08, na.rm = TRUE)

ppvs08_total_col <- colSums(ppvs08, na.rm = TRUE)

ppvs08_total_general <- sum(ppvs08_total_fila, na.rm = TRUE)

ppvs08_norm_fila <- as.data.frame(sapply(1:ncol(ppvs08[,]), function(i) {
  ppvs08[,i] / ppvs08_total_fila}))

ppvs08_norm_col <- as.data.frame(sapply(1:ncol(ppvs08[, ]), function(i) {
  ppvs08[,i ] / ppvs08_total_col[i]}))

ppvs08__norm_total_fila <- ppvs08_norm_fila / ppvs08_total_general
ppvs08__norm_total_col <- ppvs08_norm_col / ppvs08_total_general

ppvs08i_n <- ppvs08_total_fila/ppvs08_total_general
ppvs08r_n <- ppvs08_total_col/ppvs08_total_general

pacd08g <- c08 %>%
  select(AE, NOMGEO, pacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = pacd)

pacd08 <- as.data.frame(pacd08g)
rownames(pacd08) <- pacd08g$AE
pacd08 <- pacd08 %>% select(-AE)

pacd08_total_fila <- rowSums(pacd08, na.rm = TRUE)

pacd08_total_col <- colSums(pacd08, na.rm = TRUE)

pacd08_total_general <- sum(pacd08_total_fila, na.rm = TRUE)

pacd08_norm_fila <- as.data.frame(sapply(1:ncol(pacd08[,]), function(i) {
  pacd08[,i] / pacd08_total_fila}))

pacd08_norm_col <- as.data.frame(sapply(1:ncol(pacd08[, ]), function(i) {
  pacd08[,i ] / pacd08_total_col[i]}))

pacd08__norm_total_fila <- pacd08_norm_fila / pacd08_total_general
pacd08__norm_total_col <- pacd08_norm_col / pacd08_total_general

pacd08i_n <- pacd08_total_fila/pacd08_total_general
pacd08r_n <- pacd08_total_col/pacd08_total_general

vacb08g <- c08 %>%
  select(AE, NOMGEO, vacb) %>%
  pivot_wider(names_from = NOMGEO, values_from = vacb)

vacb08 <- as.data.frame(vacb08g)
rownames(vacb08) <- vacb08g$AE
vacb08 <- vacb08 %>% select(-AE)

vacb08_total_fila <- rowSums(vacb08, na.rm = TRUE)

vacb08_total_col <- colSums(vacb08, na.rm = TRUE)

vacb08_total_general <- sum(vacb08_total_fila, na.rm = TRUE)

vacb08_norm_fila <- as.data.frame(sapply(1:ncol(vacb08[,]), function(i) {
  vacb08[,i] / vacb08_total_fila}))

vacb08_norm_col <- as.data.frame(sapply(1:ncol(vacb08[, ]), function(i) {
  vacb08[,i ] / vacb08_total_col[i]}))

vacb08__norm_total_fila <- vacb08_norm_fila / vacb08_total_general
vacb08__norm_total_col <- vacb08_norm_col / vacb08_total_general

vacb08i_n <- vacb08_total_fila/vacb08_total_general
vacb08r_n <- vacb08_total_col/vacb08_total_general

ue08g <- c08 %>%
  select(AE, NOMGEO, ue) %>%
  pivot_wider(names_from = NOMGEO, values_from = ue)

ue08 <- as.data.frame(ue08g)
rownames(ue08) <- ue08g$AE
ue08 <- ue08 %>% select(-AE)

ue08_total_fila <- rowSums(ue08, na.rm = TRUE)

ue08_total_col <- colSums(ue08, na.rm = TRUE)

ue08_total_general <- sum(ue08_total_fila, na.rm = TRUE)

ue08_norm_fila <- as.data.frame(sapply(1:ncol(ue08[,]), function(i) {
  ue08[,i] / ue08_total_fila}))

ue08_norm_col <- as.data.frame(sapply(1:ncol(ue08[, ]), function(i) {
  ue08[,i ] / ue08_total_col[i]}))

ue08__norm_total_fila <- ue08_norm_fila / ue08_total_general
ue08__norm_total_col <- ue08_norm_col / ue08_total_general

ue08i_n <- ue08_total_fila/ue08_total_general
ue08r_n <- ue08_total_col/ue08_total_general

tsppvs08g <- c08 %>%
  select(AE, NOMGEO, tsppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = tsppvs)

tsppvs08 <- as.data.frame(tsppvs08g)
rownames(tsppvs08) <- tsppvs08g$AE
tsppvs08 <- tsppvs08 %>% select(-AE)

tsppvs08_total_fila <- rowSums(tsppvs08, na.rm = T)

tsppvs08_total_col <- colSums(tsppvs08, na.rm = T)

tsppvs08_total_general <- sum(tsppvs08_total_fila, na.rm = T)

tsppvs08_norm_fila <- as.data.frame(sapply(1:ncol(tsppvs08[,]), function(i) {
  tsppvs08[,i] / tsppvs08_total_fila}))

tsppvs08_norm_col <- as.data.frame(sapply(1:ncol(tsppvs08[, ]), function(i) {
  tsppvs08[,i ] / tsppvs08_total_col[i]}))

tsppvs08__norm_total_fila <- tsppvs08_norm_fila / tsppvs08_total_general
tsppvs08__norm_total_col <- tsppvs08_norm_col / tsppvs08_total_general

tsppvs08i_n <- tsppvs08_total_fila/tsppvs08_total_general
tsppvs08r_n <- tsppvs08_total_col/tsppvs08_total_general

tspacd08g <- c08 %>%
  select(AE, NOMGEO, tspacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = tspacd)

tspacd08 <- as.data.frame(tspacd08g)
rownames(tspacd08) <- tspacd08g$AE
tspacd08 <- tspacd08 %>% select(-AE)

tspacd08_total_fila <- rowSums(tspacd08, na.rm = T)

tspacd08_total_col <- colSums(tspacd08, na.rm = T)

tspacd08_total_general <- sum(tspacd08_total_fila, na.rm = T)

tspacd08_norm_fila <- as.data.frame(sapply(1:ncol(tspacd08[,]), function(i) {
  tspacd08[,i] / tspacd08_total_fila}))

tspacd08_norm_col <- as.data.frame(sapply(1:ncol(tspacd08[, ]), function(i) {
  tspacd08[,i ] / tspacd08_total_col[i]}))

tspacd08__norm_total_fila <- tspacd08_norm_fila / tspacd08_total_general
tspacd08__norm_total_col <- tspacd08_norm_col / tspacd08_total_general

tspacd08i_n <- tspacd08_total_fila/tspacd08_total_general
tspacd08r_n <- tspacd08_total_col/tspacd08_total_general

ataf08g <- c08 %>%
  select(AE, NOMGEO, ataf) %>%
  pivot_wider(names_from = NOMGEO, values_from = ataf)

ataf08 <- as.data.frame(ataf08g)
rownames(ataf08) <- ataf08g$AE
ataf08 <- ataf08 %>% select(-AE)

ataf08_total_fila <- rowSums(ataf08, na.rm = T)

ataf08_total_col <- colSums(ataf08, na.rm = T)

ataf08_total_general <- sum(ataf08_total_fila, na.rm = T)

ataf08_norm_fila <- as.data.frame(sapply(1:ncol(ataf08[,]), function(i) {
  ataf08[,i] / ataf08_total_fila}))

ataf08_norm_col <- as.data.frame(sapply(1:ncol(ataf08[, ]), function(i) {
  ataf08[,i ] / ataf08_total_col[i]}))

ataf08__norm_total_fila <- ataf08_norm_fila / ataf08_total_general
ataf08__norm_total_col <- ataf08_norm_col / ataf08_total_general

ataf08i_n <- ataf08_total_fila/ataf08_total_general
ataf08r_n <- ataf08_total_col/ataf08_total_general

dtaf08g <- c08 %>%
  select(AE, NOMGEO, dtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = dtaf)

dtaf08 <- as.data.frame(dtaf08g)
rownames(dtaf08) <- dtaf08g$AE
dtaf08 <- dtaf08 %>% select(-AE)

dtaf08_total_fila <- rowSums(dtaf08, na.rm = T)

dtaf08_total_col <- colSums(dtaf08, na.rm = T)

dtaf08_total_general <- sum(dtaf08_total_fila, na.rm = T)

dtaf08_norm_fila <- as.data.frame(sapply(1:ncol(dtaf08[,]), function(i) {
  dtaf08[,i] / dtaf08_total_fila}))

dtaf08_norm_col <- as.data.frame(sapply(1:ncol(dtaf08[, ]), function(i) {
  dtaf08[,i ] / dtaf08_total_col[i]}))

dtaf08__norm_total_fila <- dtaf08_norm_fila / dtaf08_total_general
dtaf08__norm_total_col <- dtaf08_norm_col / dtaf08_total_general

dtaf08i_n <- dtaf08_total_fila/dtaf08_total_general
dtaf08r_n <- dtaf08_total_col/dtaf08_total_general

tga08g <- c08 %>%
  select(AE, NOMGEO, tga) %>%
  pivot_wider(names_from = NOMGEO, values_from = tga)

tga08 <- as.data.frame(tga08g)
rownames(tga08) <- tga08g$AE
tga08 <- tga08 %>% select(-AE)

tga08_total_fila <- rowSums(tga08, na.rm = T)

tga08_total_col <- colSums(tga08, na.rm = T)

tga08_total_general <- sum(tga08_total_fila, na.rm = T)

tga08_norm_fila <- as.data.frame(sapply(1:ncol(tga08[,]), function(i) {
  tga08[,i] / tga08_total_fila}))

tga08_norm_col <- as.data.frame(sapply(1:ncol(tga08[, ]), function(i) {
  tga08[,i ] / tga08_total_col[i]}))

tga08__norm_total_fila <- tga08_norm_fila / tga08_total_general
tga08__norm_total_col <- tga08_norm_col / tga08_total_general

tga08i_n <- tga08_total_fila/tga08_total_general
tga08r_n <- tga08_total_col/tga08_total_general

tin08g <- c08 %>%
  select(AE, NOMGEO, tin) %>%
  pivot_wider(names_from = NOMGEO, values_from = tin)

tin08 <- as.data.frame(tin08g)
rownames(tin08) <- tin08g$AE
tin08 <- tin08 %>% select(-AE)

tin08_total_fila <- rowSums(tin08, na.rm = T)

tin08_total_col <- colSums(tin08, na.rm = T)

tin08_total_general <- sum(tin08_total_fila, na.rm = T)

tin08_norm_fila <- as.data.frame(sapply(1:ncol(tin08[,]), function(i) {
  tin08[,i] / tin08_total_fila}))

tin08_norm_col <- as.data.frame(sapply(1:ncol(tin08[, ]), function(i) {
  tin08[,i ] / tin08_total_col[i]}))

tin08__norm_total_fila <- tin08_norm_fila / tin08_total_general
tin08__norm_total_col <- tin08_norm_col / tin08_total_general

tin08i_n <- tin08_total_fila/tin08_total_general
tin08r_n <- tin08_total_col/tin08_total_general

pbt08g <- c08 %>%
  select(AE, NOMGEO, pbt) %>%
  pivot_wider(names_from = NOMGEO, values_from = pbt)

pbt08 <- as.data.frame(pbt08g)
rownames(pbt08) <- pbt08g$AE
pbt08 <- pbt08 %>% select(-AE)

pbt08_total_fila <- rowSums(pbt08, na.rm = T)

pbt08_total_col <- colSums(pbt08, na.rm = T)

pbt08_total_general <- sum(pbt08_total_fila, na.rm = T)

pbt08_norm_fila <- as.data.frame(sapply(1:ncol(pbt08[,]), function(i) {
  pbt08[,i] / pbt08_total_fila}))

pbt08_norm_col <- as.data.frame(sapply(1:ncol(pbt08[, ]), function(i) {
  pbt08[,i ] / pbt08_total_col[i]}))

pbt08__norm_total_fila <- pbt08_norm_fila / pbt08_total_general
pbt08__norm_total_col <- pbt08_norm_col / pbt08_total_general

pbt08i_n <- pbt08_total_fila/pbt08_total_general
pbt08r_n <- pbt08_total_col/pbt08_total_general

it08g <- c08 %>%
  select(AE, NOMGEO, it) %>%
  pivot_wider(names_from = NOMGEO, values_from = it)

it08 <- as.data.frame(it08g)
rownames(it08) <- it08g$AE
it08 <- it08 %>% select(-AE)

it08_total_fila <- rowSums(it08, na.rm = T)

it08_total_col <- colSums(it08, na.rm = T)

it08_total_general <- sum(it08_total_fila, na.rm = T)

it08_norm_fila <- as.data.frame(sapply(1:ncol(it08[,]), function(i) {
  it08[,i] / it08_total_fila}))

it08_norm_col <- as.data.frame(sapply(1:ncol(it08[, ]), function(i) {
  it08[,i ] / it08_total_col[i]}))

it08__norm_total_fila <- it08_norm_fila / it08_total_general
it08__norm_total_col <- it08_norm_col / it08_total_general

it08i_n <- it08_total_fila/it08_total_general
it08r_n <- it08_total_col/it08_total_general

pot08g <- c08 %>%
  select(AE, NOMGEO, pot) %>%
  pivot_wider(names_from = NOMGEO, values_from = pot)

pot08 <- as.data.frame(pot08g)
rownames(pot08) <- pot08g$AE
pot08 <- pot08 %>% select(-AE)

pot08_total_fila <- rowSums(pot08, na.rm = T)

pot08_total_col <- colSums(pot08, na.rm = T)

pot08_total_general <- sum(pot08_total_fila, na.rm = T)

pot08_norm_fila <- as.data.frame(sapply(1:ncol(pot08[,]), function(i) {
  pot08[,i] / pot08_total_fila}))

pot08_norm_col <- as.data.frame(sapply(1:ncol(pot08[, ]), function(i) {
  pot08[,i ] / pot08_total_col[i]}))

pot08__norm_total_fila <- pot08_norm_fila / pot08_total_general
pot08__norm_total_col <- pot08_norm_col / pot08_total_general

pot08i_n <- pot08_total_fila/pot08_total_general
pot08r_n <- pot08_total_col/pot08_total_general

htpot08g <- c08 %>%
  select(AE, NOMGEO, htpot) %>%
  pivot_wider(names_from = NOMGEO, values_from = htpot)

htpot08 <- as.data.frame(htpot08g)
rownames(htpot08) <- htpot08g$AE
htpot08 <- htpot08 %>% select(-AE)

htpot08_total_fila <- rowSums(htpot08, na.rm = T)

htpot08_total_col <- colSums(htpot08, na.rm = T)

htpot08_total_general <- sum(htpot08_total_fila, na.rm = T)

htpot08_norm_fila <- as.data.frame(sapply(1:ncol(htpot08[,]), function(i) {
  htpot08[,i] / htpot08_total_fila}))

htpot08_norm_col <- as.data.frame(sapply(1:ncol(htpot08[, ]), function(i) {
  htpot08[,i ] / htpot08_total_col[i]}))

htpot08__norm_total_fila <- htpot08_norm_fila / htpot08_total_general
htpot08__norm_total_col <- htpot08_norm_col / htpot08_total_general

htpot08i_n <- htpot08_total_fila/htpot08_total_general
htpot08r_n <- htpot08_total_col/htpot08_total_general

vtaf08g <- c08 %>%
  select(AE, NOMGEO, vtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = vtaf)
vtaf08 <- as.data.frame(vtaf08g)
rownames(vtaf08) <- vtaf08g$AE
vtaf08 <- vtaf08 %>% select(-AE)
vtaf08_total_fila <- rowSums(vtaf08, na.rm = T)
vtaf08_total_col <- colSums(vtaf08, na.rm = T)
vtaf08_total_general <- sum(vtaf08_total_fila, na.rm = T)
vtaf08_norm_fila <- as.data.frame(sapply(1:ncol(vtaf08[,]), function(i) {
  vtaf08[,i] / vtaf08_total_fila}))
vtaf08_norm_col <- as.data.frame(sapply(1:ncol(vtaf08[, ]), function(i) {
  vtaf08[,i ] / vtaf08_total_col[i]}))
vtaf08__norm_total_fila <- vtaf08_norm_fila / vtaf08_total_general
vtaf08__norm_total_col <- vtaf08_norm_col / vtaf08_total_general

vtaf08i_n <- vtaf08_total_fila/vtaf08_total_general
vtaf08r_n <- vtaf08_total_col/vtaf08_total_general

afpp08g <- c08 %>%
  select(AE, NOMGEO, afpp) %>%
  pivot_wider(names_from = NOMGEO, values_from = afpp)
afpp08 <- as.data.frame(afpp08g)
rownames(afpp08) <- afpp08g$AE
afpp08 <- afpp08 %>% select(-AE)
afpp08_total_fila <- rowSums(afpp08, na.rm = T)
afpp08_total_col <- colSums(afpp08, na.rm = T)
afpp08_total_general <- sum(afpp08_total_fila, na.rm = T)
afpp08_norm_fila <- as.data.frame(sapply(1:ncol(afpp08[,]), function(i) {
  afpp08[,i] / afpp08_total_fila}))
afpp08_norm_col <- as.data.frame(sapply(1:ncol(afpp08[, ]), function(i) {
  afpp08[,i ] / afpp08_total_col[i]}))
afpp08__norm_total_fila <- afpp08_norm_fila / afpp08_total_general
afpp08__norm_total_col <- afpp08_norm_col / afpp08_total_general

afpp08i_n <- afpp08_total_fila/afpp08_total_general
afpp08r_n <- afpp08_total_col/afpp08_total_general

atmep08g <- c08 %>%
  select(AE, NOMGEO, atmep) %>%
  pivot_wider(names_from = NOMGEO, values_from = atmep)
atmep08 <- as.data.frame(atmep08g)
rownames(atmep08) <- atmep08g$AE
atmep08 <- atmep08 %>% select(-AE)
atmep08_total_fila <- rowSums(atmep08, na.rm = T)
atmep08_total_col <- colSums(atmep08, na.rm = T)
atmep08_total_general <- sum(atmep08_total_fila, na.rm = T)
atmep08_norm_fila <- as.data.frame(sapply(1:ncol(atmep08[,]), function(i) {
  atmep08[,i] / atmep08_total_fila}))
atmep08_norm_col <- as.data.frame(sapply(1:ncol(atmep08[, ]), function(i) {
  atmep08[,i ] / atmep08_total_col[i]}))
atmep08__norm_total_fila <- atmep08_norm_fila / atmep08_total_general
atmep08__norm_total_col <- atmep08_norm_col / atmep08_total_general

atmep08i_n <- atmep08_total_fila/atmep08_total_general
atmep08r_n <- atmep08_total_col/atmep08_total_general

atbi08g <- c08 %>%
  select(AE, NOMGEO, atbi) %>%
  pivot_wider(names_from = NOMGEO, values_from = atbi)
atbi08 <- as.data.frame(atbi08g)
rownames(atbi08) <- atbi08g$AE
atbi08 <- atbi08 %>% select(-AE)
atbi08_total_fila <- rowSums(atbi08, na.rm = T)
atbi08_total_col <- colSums(atbi08, na.rm = T)
atbi08_total_general <- sum(atbi08_total_fila, na.rm = T)
atbi08_norm_fila <- as.data.frame(sapply(1:ncol(atbi08[,]), function(i) {
  atbi08[,i] / atbi08_total_fila}))
atbi08_norm_col <- as.data.frame(sapply(1:ncol(atbi08[, ]), function(i) {
  atbi08[,i ] / atbi08_total_col[i]}))
atbi08__norm_total_fila <- atbi08_norm_fila / atbi08_total_general
atbi08__norm_total_col <- atbi08_norm_col / atbi08_total_general

atbi08i_n <- atbi08_total_fila/atbi08_total_general
atbi08r_n <- atbi08_total_col/atbi08_total_general

atuet08g <- c08 %>%
  select(AE, NOMGEO, atuet) %>%
  pivot_wider(names_from = NOMGEO, values_from = atuet)
atuet08 <- as.data.frame(atuet08g)
rownames(atuet08) <- atuet08g$AE
atuet08 <- atuet08 %>% select(-AE)
atuet08_total_fila <- rowSums(atuet08, na.rm = T)
atuet08_total_col <- colSums(atuet08, na.rm = T)
atuet08_total_general <- sum(atuet08_total_fila, na.rm = T)
atuet08_norm_fila <- as.data.frame(sapply(1:ncol(atuet08[,]), function(i) {
  atuet08[,i] / atuet08_total_fila}))
atuet08_norm_col <- as.data.frame(sapply(1:ncol(atuet08[, ]), function(i) {
  atuet08[,i ] / atuet08_total_col[i]}))
atuet08__norm_total_fila <- atuet08_norm_fila / atuet08_total_general
atuet08__norm_total_col <- atuet08_norm_col / atuet08_total_general

atuet08i_n <- atuet08_total_fila/atuet08_total_general
atuet08r_n <- atuet08_total_col/atuet08_total_general

atecp08g <- c08 %>%
  select(AE, NOMGEO, atecp) %>%
  pivot_wider(names_from = NOMGEO, values_from = atecp)
atecp08 <- as.data.frame(atecp08g)
rownames(atecp08) <- atecp08g$AE
atecp08 <- atecp08 %>% select(-AE)
atecp08_total_fila <- rowSums(atecp08, na.rm = T)
atecp08_total_col <- colSums(atecp08, na.rm = T)
atecp08_total_general <- sum(atecp08_total_fila, na.rm = T)
atecp08_norm_fila <- as.data.frame(sapply(1:ncol(atecp08[,]), function(i) {
  atecp08[,i] / atecp08_total_fila}))
atecp08_norm_col <- as.data.frame(sapply(1:ncol(atecp08[, ]), function(i) {
  atecp08[,i ] / atecp08_total_col[i]}))
atecp08__norm_total_fila <- atecp08_norm_fila / atecp08_total_general
atecp08__norm_total_col <- atecp08_norm_col / atecp08_total_general

atecp08i_n <- atecp08_total_fila/atecp08_total_general
atecp08r_n <- atecp08_total_col/atecp08_total_general

atma08g <- c08 %>%
  select(AE, NOMGEO, atma) %>%
  pivot_wider(names_from = NOMGEO, values_from = atma)
atma08 <- as.data.frame(atma08g)
rownames(atma08) <- atma08g$AE
atma08 <- atma08 %>% select(-AE)
atma08_total_fila <- rowSums(atma08, na.rm = T)
atma08_total_col <- colSums(atma08, na.rm = T)
atma08_total_general <- sum(atma08_total_fila, na.rm = T)
atma08_norm_fila <- as.data.frame(sapply(1:ncol(atma08[,]), function(i) {
  atma08[,i] / atma08_total_fila}))
atma08_norm_col <- as.data.frame(sapply(1:ncol(atma08[, ]), function(i) {
  atma08[,i ] / atma08_total_col[i]}))
atma08__norm_total_fila <- atma08_norm_fila / atma08_total_general
atma08__norm_total_col <- atma08_norm_col / atma08_total_general

atma08i_n <- atma08_total_fila/atma08_total_general
atma08r_n <- atma08_total_col/atma08_total_general

ppvs13g <- c13 %>%
  select(AE, NOMGEO, ppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = ppvs)

ppvs13 <- as.data.frame(ppvs13g)
rownames(ppvs13) <- ppvs13g$AE
ppvs13 <- ppvs13 %>% select(-AE)


ppvs13_total_fila <- rowSums(ppvs13, na.rm = TRUE)

ppvs13_total_col <- colSums(ppvs13, na.rm = TRUE)

ppvs13_total_general <- sum(ppvs13_total_fila, na.rm = TRUE)

ppvs13_norm_fila <- as.data.frame(sapply(1:ncol(ppvs13[,]), function(i) {
  ppvs13[,i] / ppvs13_total_fila}))

ppvs13_norm_col <- as.data.frame(sapply(1:ncol(ppvs13[, ]), function(i) {
  ppvs13[,i ] / ppvs13_total_col[i]}))

ppvs13__norm_total_fila <- ppvs13_norm_fila / ppvs13_total_general
ppvs13__norm_total_col <- ppvs13_norm_col / ppvs13_total_general

ppvs13i_n <- ppvs13_total_fila/ppvs13_total_general
ppvs13r_n <- ppvs13_total_col/ppvs13_total_general

pacd13g <- c13 %>%
  select(AE, NOMGEO, pacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = pacd)

pacd13 <- as.data.frame(pacd13g)
rownames(pacd13) <- pacd13g$AE
pacd13 <- pacd13 %>% select(-AE)


pacd13_total_fila <- rowSums(pacd13, na.rm = TRUE)

pacd13_total_col <- colSums(pacd13, na.rm = TRUE)

pacd13_total_general <- sum(pacd13_total_fila, na.rm = TRUE)

pacd13_norm_fila <- as.data.frame(sapply(1:ncol(pacd13[,]), function(i) {
  pacd13[,i] / pacd13_total_fila}))

pacd13_norm_col <- as.data.frame(sapply(1:ncol(pacd13[, ]), function(i) {
  pacd13[,i ] / pacd13_total_col[i]}))

pacd13__norm_total_fila <- pacd13_norm_fila / pacd13_total_general
pacd13__norm_total_col <- pacd13_norm_col / pacd13_total_general

pacd13i_n <- pacd13_total_fila/pacd13_total_general
pacd13r_n <- pacd13_total_col/pacd13_total_general

vacb13g <- c13 %>%
  select(AE, NOMGEO, vacb) %>%
  pivot_wider(names_from = NOMGEO, values_from = vacb)

vacb13 <- as.data.frame(vacb13g)
rownames(vacb13) <- vacb13g$AE
vacb13 <- vacb13 %>% select(-AE)

vacb13_total_fila <- rowSums(vacb13, na.rm = TRUE)

vacb13_total_col <- colSums(vacb13, na.rm = TRUE)

vacb13_total_general <- sum(vacb13_total_fila, na.rm = TRUE)


vacb13_norm_fila <- as.data.frame(sapply(1:ncol(vacb13[,]), function(i) {
  vacb13[,i] / vacb13_total_fila}))


vacb13_norm_col <- as.data.frame(sapply(1:ncol(vacb13[, ]), function(i) {
  vacb13[,i ] / vacb13_total_col[i]}))

vacb13__norm_total_fila <- vacb13_norm_fila / vacb13_total_general
vacb13__norm_total_col <- vacb13_norm_col / vacb13_total_general

vacb13i_n <- vacb13_total_fila/vacb13_total_general
vacb13r_n <- vacb13_total_col/vacb13_total_general

ue13g <- c13 %>%
  select(AE, NOMGEO, ue) %>%
  pivot_wider(names_from = NOMGEO, values_from = ue)


ue13 <- as.data.frame(ue13g)
rownames(ue13) <- ue13g$AE
ue13 <- ue13 %>% select(-AE)

ue13_total_fila <- rowSums(ue13, na.rm = TRUE)

ue13_total_col <- colSums(ue13, na.rm = TRUE)

ue13_total_general <- sum(ue13_total_fila, na.rm = TRUE)

ue13_norm_fila <- as.data.frame(sapply(1:ncol(ue13[,]), function(i) {
  ue13[,i] / ue13_total_fila}))

ue13_norm_col <- as.data.frame(sapply(1:ncol(ue13[, ]), function(i) {
  ue13[,i ] / ue13_total_col[i]}))

ue13__norm_total_fila <- ue13_norm_fila / ue13_total_general
ue13__norm_total_col <- ue13_norm_col / ue13_total_general

ue13i_n <- ue13_total_fila/ue13_total_general
ue13r_n <- ue13_total_col/ue13_total_general

tsppvs13g <- c13 %>%
  select(AE, NOMGEO, tsppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = tsppvs)

tsppvs13 <- as.data.frame(tsppvs13g)
rownames(tsppvs13) <- tsppvs13g$AE
tsppvs13 <- tsppvs13 %>% select(-AE)

tsppvs13_total_fila <- rowSums(tsppvs13, na.rm = T)

tsppvs13_total_col <- colSums(tsppvs13, na.rm = T)

tsppvs13_total_general <- sum(tsppvs13_total_fila, na.rm = T)

tsppvs13_norm_fila <- as.data.frame(sapply(1:ncol(tsppvs13[,]), function(i) {
  tsppvs13[,i] / tsppvs13_total_fila}))

tsppvs13_norm_col <- as.data.frame(sapply(1:ncol(tsppvs13[, ]), function(i) {
  tsppvs13[,i ] / tsppvs13_total_col[i]}))

tsppvs13__norm_total_fila <- tsppvs13_norm_fila / tsppvs13_total_general
tsppvs13__norm_total_col <- tsppvs13_norm_col / tsppvs13_total_general

tsppvs13i_n <- tsppvs13_total_fila/tsppvs13_total_general
tsppvs13r_n <- tsppvs13_total_col/tsppvs13_total_general

tspacd13g <- c13 %>%
  select(AE, NOMGEO, tspacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = tspacd)

tspacd13 <- as.data.frame(tspacd13g)
rownames(tspacd13) <- tspacd13g$AE
tspacd13 <- tspacd13 %>% select(-AE)

tspacd13_total_fila <- rowSums(tspacd13, na.rm = T)

tspacd13_total_col <- colSums(tspacd13, na.rm = T)

tspacd13_total_general <- sum(tspacd13_total_fila, na.rm = T)

tspacd13_norm_fila <- as.data.frame(sapply(1:ncol(tspacd13[,]), function(i) {
  tspacd13[,i] / tspacd13_total_fila}))

tspacd13_norm_col <- as.data.frame(sapply(1:ncol(tspacd13[, ]), function(i) {
  tspacd13[,i ] / tspacd13_total_col[i]}))

tspacd13__norm_total_fila <- tspacd13_norm_fila / tspacd13_total_general
tspacd13__norm_total_col <- tspacd13_norm_col / tspacd13_total_general

tspacd13i_n <- tspacd13_total_fila/tspacd13_total_general
tspacd13r_n <- tspacd13_total_col/tspacd13_total_general

ataf13g <- c13 %>%
  select(AE, NOMGEO, ataf) %>%
  pivot_wider(names_from = NOMGEO, values_from = ataf)

ataf13 <- as.data.frame(ataf13g)
rownames(ataf13) <- ataf13g$AE
ataf13 <- ataf13 %>% select(-AE)

ataf13_total_fila <- rowSums(ataf13, na.rm = T)

ataf13_total_col <- colSums(ataf13, na.rm = T)

ataf13_total_general <- sum(ataf13_total_fila, na.rm = T)

ataf13_norm_fila <- as.data.frame(sapply(1:ncol(ataf13[,]), function(i) {
  ataf13[,i] / ataf13_total_fila}))

ataf13_norm_col <- as.data.frame(sapply(1:ncol(ataf13[, ]), function(i) {
  ataf13[,i ] / ataf13_total_col[i]}))

ataf13__norm_total_fila <- ataf13_norm_fila / ataf13_total_general
ataf13__norm_total_col <- ataf13_norm_col / ataf13_total_general

ataf13i_n <- ataf13_total_fila/ataf13_total_general
ataf13r_n <- ataf13_total_col/ataf13_total_general

dtaf13g <- c13 %>%
  select(AE, NOMGEO, dtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = dtaf)

dtaf13 <- as.data.frame(dtaf13g)
rownames(dtaf13) <- dtaf13g$AE
dtaf13 <- dtaf13 %>% select(-AE)

dtaf13_total_fila <- rowSums(dtaf13, na.rm = T)

dtaf13_total_col <- colSums(dtaf13, na.rm = T)

dtaf13_total_general <- sum(dtaf13_total_fila, na.rm = T)

dtaf13_norm_fila <- as.data.frame(sapply(1:ncol(dtaf13[,]), function(i) {
  dtaf13[,i] / dtaf13_total_fila}))

dtaf13_norm_col <- as.data.frame(sapply(1:ncol(dtaf13[, ]), function(i) {
  dtaf13[,i ] / dtaf13_total_col[i]}))

dtaf13__norm_total_fila <- dtaf13_norm_fila / dtaf13_total_general
dtaf13__norm_total_col <- dtaf13_norm_col / dtaf13_total_general

dtaf13i_n <- dtaf13_total_fila/dtaf13_total_general
dtaf13r_n <- dtaf13_total_col/dtaf13_total_general

tga13g <- c13 %>%
  select(AE, NOMGEO, tga) %>%
  pivot_wider(names_from = NOMGEO, values_from = tga)

tga13 <- as.data.frame(tga13g)
rownames(tga13) <- tga13g$AE
tga13 <- tga13 %>% select(-AE)

tga13_total_fila <- rowSums(tga13, na.rm = T)

tga13_total_col <- colSums(tga13, na.rm = T)

tga13_total_general <- sum(tga13_total_fila, na.rm = T)

tga13_norm_fila <- as.data.frame(sapply(1:ncol(tga13[,]), function(i) {
  tga13[,i] / tga13_total_fila}))

tga13_norm_col <- as.data.frame(sapply(1:ncol(tga13[, ]), function(i) {
  tga13[,i ] / tga13_total_col[i]}))

tga13__norm_total_fila <- tga13_norm_fila / tga13_total_general
tga13__norm_total_col <- tga13_norm_col / tga13_total_general

tga13i_n <- tga13_total_fila/tga13_total_general
tga13r_n <- tga13_total_col/tga13_total_general

tin13g <- c13 %>%
  select(AE, NOMGEO, tin) %>%
  pivot_wider(names_from = NOMGEO, values_from = tin)

tin13 <- as.data.frame(tin13g)
rownames(tin13) <- tin13g$AE
tin13 <- tin13 %>% select(-AE)

tin13_total_fila <- rowSums(tin13, na.rm = T)

tin13_total_col <- colSums(tin13, na.rm = T)

tin13_total_general <- sum(tin13_total_fila, na.rm = T)

tin13_norm_fila <- as.data.frame(sapply(1:ncol(tin13[,]), function(i) {
  tin13[,i] / tin13_total_fila}))

tin13_norm_col <- as.data.frame(sapply(1:ncol(tin13[, ]), function(i) {
  tin13[,i ] / tin13_total_col[i]}))

tin13__norm_total_fila <- tin13_norm_fila / tin13_total_general
tin13__norm_total_col <- tin13_norm_col / tin13_total_general

tin13i_n <- tin13_total_fila/tin13_total_general
tin13r_n <- tin13_total_col/tin13_total_general

pbt13g <- c13 %>%
  select(AE, NOMGEO, pbt) %>%
  pivot_wider(names_from = NOMGEO, values_from = pbt)

pbt13 <- as.data.frame(pbt13g)
rownames(pbt13) <- pbt13g$AE
pbt13 <- pbt13 %>% select(-AE)

pbt13_total_fila <- rowSums(pbt13, na.rm = T)

pbt13_total_col <- colSums(pbt13, na.rm = T)

pbt13_total_general <- sum(pbt13_total_fila, na.rm = T)

pbt13_norm_fila <- as.data.frame(sapply(1:ncol(pbt13[,]), function(i) {
  pbt13[,i] / pbt13_total_fila}))

pbt13_norm_col <- as.data.frame(sapply(1:ncol(pbt13[, ]), function(i) {
  pbt13[,i ] / pbt13_total_col[i]}))

pbt13__norm_total_fila <- pbt13_norm_fila / pbt13_total_general
pbt13__norm_total_col <- pbt13_norm_col / pbt13_total_general

pbt13i_n <- pbt13_total_fila/pbt13_total_general
pbt13r_n <- pbt13_total_col/pbt13_total_general

it13g <- c13 %>%
  select(AE, NOMGEO, it) %>%
  pivot_wider(names_from = NOMGEO, values_from = it)

it13 <- as.data.frame(it13g)
rownames(it13) <- it13g$AE
it13 <- it13 %>% select(-AE)

it13_total_fila <- rowSums(it13, na.rm = T)

it13_total_col <- colSums(it13, na.rm = T)

it13_total_general <- sum(it13_total_fila, na.rm = T)

it13_norm_fila <- as.data.frame(sapply(1:ncol(it13[,]), function(i) {
  it13[,i] / it13_total_fila}))

it13_norm_col <- as.data.frame(sapply(1:ncol(it13[, ]), function(i) {
  it13[,i ] / it13_total_col[i]}))

it13__norm_total_fila <- it13_norm_fila / it13_total_general
it13__norm_total_col <- it13_norm_col / it13_total_general

it13i_n <- it13_total_fila/it13_total_general
it13r_n <- it13_total_col/it13_total_general

pot13g <- c13 %>%
  select(AE, NOMGEO, pot) %>%
  pivot_wider(names_from = NOMGEO, values_from = pot)

pot13 <- as.data.frame(pot13g)
rownames(pot13) <- pot13g$AE
pot13 <- pot13 %>% select(-AE)

pot13_total_fila <- rowSums(pot13, na.rm = T)

pot13_total_col <- colSums(pot13, na.rm = T)

pot13_total_general <- sum(pot13_total_fila, na.rm = T)

pot13_norm_fila <- as.data.frame(sapply(1:ncol(pot13[,]), function(i) {
  pot13[,i] / pot13_total_fila}))

pot13_norm_col <- as.data.frame(sapply(1:ncol(pot13[, ]), function(i) {
  pot13[,i ] / pot13_total_col[i]}))

pot13__norm_total_fila <- pot13_norm_fila / pot13_total_general
pot13__norm_total_col <- pot13_norm_col / pot13_total_general

pot13i_n <- pot13_total_fila/pot13_total_general
pot13r_n <- pot13_total_col/pot13_total_general

htpot13g <- c13 %>%
  select(AE, NOMGEO, htpot) %>%
  pivot_wider(names_from = NOMGEO, values_from = htpot)

htpot13 <- as.data.frame(htpot13g)
rownames(htpot13) <- htpot13g$AE
htpot13 <- htpot13 %>% select(-AE)

htpot13_total_fila <- rowSums(htpot13, na.rm = T)

htpot13_total_col <- colSums(htpot13, na.rm = T)

htpot13_total_general <- sum(htpot13_total_fila, na.rm = T)

htpot13_norm_fila <- as.data.frame(sapply(1:ncol(htpot13[,]), function(i) {
  htpot13[,i] / htpot13_total_fila}))

htpot13_norm_col <- as.data.frame(sapply(1:ncol(htpot13[, ]), function(i) {
  htpot13[,i ] / htpot13_total_col[i]}))

htpot13__norm_total_fila <- htpot13_norm_fila / htpot13_total_general
htpot13__norm_total_col <- htpot13_norm_col / htpot13_total_general

htpot13i_n <- htpot13_total_fila/htpot13_total_general
htpot13r_n <- htpot13_total_col/htpot13_total_general

vtaf13g <- c13 %>%
  select(AE, NOMGEO, vtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = vtaf)
vtaf13 <- as.data.frame(vtaf13g)
rownames(vtaf13) <- vtaf13g$AE
vtaf13 <- vtaf13 %>% select(-AE)
vtaf13_total_fila <- rowSums(vtaf13, na.rm = T)
vtaf13_total_col <- colSums(vtaf13, na.rm = T)
vtaf13_total_general <- sum(vtaf13_total_fila, na.rm = T)
vtaf13_norm_fila <- as.data.frame(sapply(1:ncol(vtaf13[,]), function(i) {
  vtaf13[,i] / vtaf13_total_fila}))
vtaf13_norm_col <- as.data.frame(sapply(1:ncol(vtaf13[, ]), function(i) {
  vtaf13[,i ] / vtaf13_total_col[i]}))
vtaf13__norm_total_fila <- vtaf13_norm_fila / vtaf13_total_general
vtaf13__norm_total_col <- vtaf13_norm_col / vtaf13_total_general

vtaf13i_n <- vtaf13_total_fila/vtaf13_total_general
vtaf13r_n <- vtaf13_total_col/vtaf13_total_general

afpp13g <- c13 %>%
  select(AE, NOMGEO, afpp) %>%
  pivot_wider(names_from = NOMGEO, values_from = afpp)
afpp13 <- as.data.frame(afpp13g)
rownames(afpp13) <- afpp13g$AE
afpp13 <- afpp13 %>% select(-AE)
afpp13_total_fila <- rowSums(afpp13, na.rm = T)
afpp13_total_col <- colSums(afpp13, na.rm = T)
afpp13_total_general <- sum(afpp13_total_fila, na.rm = T)
afpp13_norm_fila <- as.data.frame(sapply(1:ncol(afpp13[,]), function(i) {
  afpp13[,i] / afpp13_total_fila}))
afpp13_norm_col <- as.data.frame(sapply(1:ncol(afpp13[, ]), function(i) {
  afpp13[,i ] / afpp13_total_col[i]}))
afpp13__norm_total_fila <- afpp13_norm_fila / afpp13_total_general
afpp13__norm_total_col <- afpp13_norm_col / afpp13_total_general

afpp13i_n <- afpp13_total_fila/afpp13_total_general
afpp13r_n <- afpp13_total_col/afpp13_total_general

atmep13g <- c13 %>%
  select(AE, NOMGEO, atmep) %>%
  pivot_wider(names_from = NOMGEO, values_from = atmep)
atmep13 <- as.data.frame(atmep13g)
rownames(atmep13) <- atmep13g$AE
atmep13 <- atmep13 %>% select(-AE)
atmep13_total_fila <- rowSums(atmep13, na.rm = T)
atmep13_total_col <- colSums(atmep13, na.rm = T)
atmep13_total_general <- sum(atmep13_total_fila, na.rm = T)
atmep13_norm_fila <- as.data.frame(sapply(1:ncol(atmep13[,]), function(i) {
  atmep13[,i] / atmep13_total_fila}))
atmep13_norm_col <- as.data.frame(sapply(1:ncol(atmep13[, ]), function(i) {
  atmep13[,i ] / atmep13_total_col[i]}))
atmep13__norm_total_fila <- atmep13_norm_fila / atmep13_total_general
atmep13__norm_total_col <- atmep13_norm_col / atmep13_total_general

atmep13i_n <- atmep13_total_fila/atmep13_total_general
atmep13r_n <- atmep13_total_col/atmep13_total_general

atbi13g <- c13 %>%
  select(AE, NOMGEO, atbi) %>%
  pivot_wider(names_from = NOMGEO, values_from = atbi)
atbi13 <- as.data.frame(atbi13g)
rownames(atbi13) <- atbi13g$AE
atbi13 <- atbi13 %>% select(-AE)
atbi13_total_fila <- rowSums(atbi13, na.rm = T)
atbi13_total_col <- colSums(atbi13, na.rm = T)
atbi13_total_general <- sum(atbi13_total_fila, na.rm = T)
atbi13_norm_fila <- as.data.frame(sapply(1:ncol(atbi13[,]), function(i) {
  atbi13[,i] / atbi13_total_fila}))
atbi13_norm_col <- as.data.frame(sapply(1:ncol(atbi13[, ]), function(i) {
  atbi13[,i ] / atbi13_total_col[i]}))
atbi13__norm_total_fila <- atbi13_norm_fila / atbi13_total_general
atbi13__norm_total_col <- atbi13_norm_col / atbi13_total_general

atbi13i_n <- atbi13_total_fila/atbi13_total_general
atbi13r_n <- atbi13_total_col/atbi13_total_general

atuet13g <- c13 %>%
  select(AE, NOMGEO, atuet) %>%
  pivot_wider(names_from = NOMGEO, values_from = atuet)
atuet13 <- as.data.frame(atuet13g)
rownames(atuet13) <- atuet13g$AE
atuet13 <- atuet13 %>% select(-AE)
atuet13_total_fila <- rowSums(atuet13, na.rm = T)
atuet13_total_col <- colSums(atuet13, na.rm = T)
atuet13_total_general <- sum(atuet13_total_fila, na.rm = T)
atuet13_norm_fila <- as.data.frame(sapply(1:ncol(atuet13[,]), function(i) {
  atuet13[,i] / atuet13_total_fila}))
atuet13_norm_col <- as.data.frame(sapply(1:ncol(atuet13[, ]), function(i) {
  atuet13[,i ] / atuet13_total_col[i]}))
atuet13__norm_total_fila <- atuet13_norm_fila / atuet13_total_general
atuet13__norm_total_col <- atuet13_norm_col / atuet13_total_general

atuet13i_n <- atuet13_total_fila/atuet13_total_general
atuet13r_n <- atuet13_total_col/atuet13_total_general

atecp13g <- c13 %>%
  select(AE, NOMGEO, atecp) %>%
  pivot_wider(names_from = NOMGEO, values_from = atecp)
atecp13 <- as.data.frame(atecp13g)
rownames(atecp13) <- atecp13g$AE
atecp13 <- atecp13 %>% select(-AE)
atecp13_total_fila <- rowSums(atecp13, na.rm = T)
atecp13_total_col <- colSums(atecp13, na.rm = T)
atecp13_total_general <- sum(atecp13_total_fila, na.rm = T)
atecp13_norm_fila <- as.data.frame(sapply(1:ncol(atecp13[,]), function(i) {
  atecp13[,i] / atecp13_total_fila}))
atecp13_norm_col <- as.data.frame(sapply(1:ncol(atecp13[, ]), function(i) {
  atecp13[,i ] / atecp13_total_col[i]}))
atecp13__norm_total_fila <- atecp13_norm_fila / atecp13_total_general
atecp13__norm_total_col <- atecp13_norm_col / atecp13_total_general

atecp13i_n <- atecp13_total_fila/atecp13_total_general
atecp13r_n <- atecp13_total_col/atecp13_total_general

atma13g <- c13 %>%
  select(AE, NOMGEO, atma) %>%
  pivot_wider(names_from = NOMGEO, values_from = atma)
atma13 <- as.data.frame(atma13g)
rownames(atma13) <- atma13g$AE
atma13 <- atma13 %>% select(-AE)
atma13_total_fila <- rowSums(atma13, na.rm = T)
atma13_total_col <- colSums(atma13, na.rm = T)
atma13_total_general <- sum(atma13_total_fila, na.rm = T)
atma13_norm_fila <- as.data.frame(sapply(1:ncol(atma13[,]), function(i) {
  atma13[,i] / atma13_total_fila}))
atma13_norm_col <- as.data.frame(sapply(1:ncol(atma13[, ]), function(i) {
  atma13[,i ] / atma13_total_col[i]}))
atma13__norm_total_fila <- atma13_norm_fila / atma13_total_general
atma13__norm_total_col <- atma13_norm_col / atma13_total_general

atma13i_n <- atma13_total_fila/atma13_total_general
atma13r_n <- atma13_total_col/atma13_total_general



ppvs18g <- c18 %>%
  select(AE, NOMGEO, ppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = ppvs)

ppvs18 <- as.data.frame(ppvs18g)
rownames(ppvs18) <- ppvs18g$AE
ppvs18 <- ppvs18 %>% select(-AE)


ppvs18_total_fila <- rowSums(ppvs18, na.rm = TRUE)

ppvs18_total_col <- colSums(ppvs18, na.rm = TRUE)

ppvs18_total_general <- sum(ppvs18_total_fila, na.rm = TRUE)

ppvs18_norm_fila <- as.data.frame(sapply(1:ncol(ppvs18[,]), function(i) {
  ppvs18[,i] / ppvs18_total_fila}))

ppvs18_norm_col <- as.data.frame(sapply(1:ncol(ppvs18[, ]), function(i) {
  ppvs18[,i ] / ppvs18_total_col[i]}))

ppvs18__norm_total_fila <- ppvs18_norm_fila / ppvs18_total_general
ppvs18__norm_total_col <- ppvs18_norm_col / ppvs18_total_general

ppvs18i_n <- ppvs18_total_fila/ppvs18_total_general
ppvs18r_n <- ppvs18_total_col/ppvs18_total_general

pacd18g <- c18 %>%
  select(AE, NOMGEO, pacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = pacd)

pacd18 <- as.data.frame(pacd18g)
rownames(pacd18) <- pacd18g$AE
pacd18 <- pacd18 %>% select(-AE)

pacd18_total_fila <- rowSums(pacd18, na.rm = TRUE)

pacd18_total_col <- colSums(pacd18, na.rm = TRUE)

pacd18_total_general <- sum(pacd18_total_fila, na.rm = TRUE)

pacd18_norm_fila <- as.data.frame(sapply(1:ncol(pacd18[,]), function(i) {
  pacd18[,i] / pacd18_total_fila}))

pacd18_norm_col <- as.data.frame(sapply(1:ncol(pacd18[, ]), function(i) {
  pacd18[,i ] / pacd18_total_col[i]}))

pacd18__norm_total_fila <- pacd18_norm_fila / pacd18_total_general
pacd18__norm_total_col <- pacd18_norm_col / pacd18_total_general

pacd18i_n <- pacd18_total_fila/pacd18_total_general
pacd18r_n <- pacd18_total_col/pacd18_total_general

vacb18g <- c18 %>%
  select(AE, NOMGEO, vacb) %>%
  pivot_wider(names_from = NOMGEO, values_from = vacb)

# Configurar AE como nombres de fila
vacb18 <- as.data.frame(vacb18g)
rownames(vacb18) <- vacb18g$AE
vacb18 <- vacb18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
vacb18_total_fila <- rowSums(vacb18, na.rm = TRUE)

# Calcular los totales de columnas
vacb18_total_col <- colSums(vacb18, na.rm = TRUE)

# Calcular el total general
vacb18_total_general <- sum(vacb18_total_fila, na.rm = TRUE)

# Dividir cada observación entre el total de las filas
vacb18_norm_fila <- as.data.frame(sapply(1:ncol(vacb18[,]), function(i) {
  vacb18[,i] / vacb18_total_fila}))

# Dividir cada observación entre el total de las columnas
vacb18_norm_col <- as.data.frame(sapply(1:ncol(vacb18[, ]), function(i) {
  vacb18[,i ] / vacb18_total_col[i]}))

# Dividir cada observación entre el total general
vacb18__norm_total_fila <- vacb18_norm_fila / vacb18_total_general
vacb18__norm_total_col <- vacb18_norm_col / vacb18_total_general

vacb18i_n <- vacb18_total_fila/vacb18_total_general
vacb18r_n <- vacb18_total_col/vacb18_total_general

ue18g <- c18 %>%
  select(AE, NOMGEO, ue) %>%
  pivot_wider(names_from = NOMGEO, values_from = ue)

# Configurar AE como nombres de fila
ue18 <- as.data.frame(ue18g)
rownames(ue18) <- ue18g$AE
ue18 <- ue18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
ue18_total_fila <- rowSums(ue18, na.rm = TRUE)

# Calcular los totales de columnas
ue18_total_col <- colSums(ue18, na.rm = TRUE)

# Calcular el total general
ue18_total_general <- sum(ue18_total_fila, na.rm = TRUE)

# Dividir cada observación entre el total de las filas
ue18_norm_fila <- as.data.frame(sapply(1:ncol(ue18[,]), function(i) {
  ue18[,i] / ue18_total_fila}))

# Dividir cada observación entre el total de las columnas
ue18_norm_col <- as.data.frame(sapply(1:ncol(ue18[, ]), function(i) {
  ue18[,i ] / ue18_total_col[i]}))

# Dividir cada observación entre el total general
ue18__norm_total_fila <- ue18_norm_fila / ue18_total_general
ue18__norm_total_col <- ue18_norm_col / ue18_total_general

ue18i_n <- ue18_total_fila/ue18_total_general
ue18r_n <- ue18_total_col/ue18_total_general

tsppvs18g <- c18 %>%
  select(AE, NOMGEO, tsppvs) %>%
  pivot_wider(names_from = NOMGEO, values_from = tsppvs)

# Configurar AE como nombres de fila
tsppvs18 <- as.data.frame(tsppvs18g)
rownames(tsppvs18) <- tsppvs18g$AE
tsppvs18 <- tsppvs18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
tsppvs18_total_fila <- rowSums(tsppvs18, na.rm = T)

# Calcular los totales de columnas
tsppvs18_total_col <- colSums(tsppvs18, na.rm = T)

# Calcular el total general
tsppvs18_total_general <- sum(tsppvs18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
tsppvs18_norm_fila <- as.data.frame(sapply(1:ncol(tsppvs18[,]), function(i) {
  tsppvs18[,i] / tsppvs18_total_fila}))

# Dividir cada observación entre el total de las columnas
tsppvs18_norm_col <- as.data.frame(sapply(1:ncol(tsppvs18[, ]), function(i) {
  tsppvs18[,i ] / tsppvs18_total_col[i]}))

# Dividir cada observación entre el total general
tsppvs18__norm_total_fila <- tsppvs18_norm_fila / tsppvs18_total_general
tsppvs18__norm_total_col <- tsppvs18_norm_col / tsppvs18_total_general

tsppvs18i_n <- tsppvs18_total_fila/tsppvs18_total_general
tsppvs18r_n <- tsppvs18_total_col/tsppvs18_total_general

tspacd18g <- c18 %>%
  select(AE, NOMGEO, tspacd) %>%
  pivot_wider(names_from = NOMGEO, values_from = tspacd)

# Configurar AE como nombres de fila
tspacd18 <- as.data.frame(tspacd18g)
rownames(tspacd18) <- tspacd18g$AE
tspacd18 <- tspacd18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
tspacd18_total_fila <- rowSums(tspacd18, na.rm = T)

# Calcular los totales de columnas
tspacd18_total_col <- colSums(tspacd18, na.rm = T)

# Calcular el total general
tspacd18_total_general <- sum(tspacd18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
tspacd18_norm_fila <- as.data.frame(sapply(1:ncol(tspacd18[,]), function(i) {
  tspacd18[,i] / tspacd18_total_fila}))

# Dividir cada observación entre el total de las columnas
tspacd18_norm_col <- as.data.frame(sapply(1:ncol(tspacd18[, ]), function(i) {
  tspacd18[,i ] / tspacd18_total_col[i]}))

# Dividir cada observación entre el total general
tspacd18__norm_total_fila <- tspacd18_norm_fila / tspacd18_total_general
tspacd18__norm_total_col <- tspacd18_norm_col / tspacd18_total_general

tspacd18i_n <- tspacd18_total_fila/tspacd18_total_general
tspacd18r_n <- tspacd18_total_col/tspacd18_total_general

ataf18g <- c18 %>%
  select(AE, NOMGEO, ataf) %>%
  pivot_wider(names_from = NOMGEO, values_from = ataf)

# Configurar AE como nombres de fila
ataf18 <- as.data.frame(ataf18g)
rownames(ataf18) <- ataf18g$AE
ataf18 <- ataf18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
ataf18_total_fila <- rowSums(ataf18, na.rm = T)

# Calcular los totales de columnas
ataf18_total_col <- colSums(ataf18, na.rm = T)

# Calcular el total general
ataf18_total_general <- sum(ataf18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
ataf18_norm_fila <- as.data.frame(sapply(1:ncol(ataf18[,]), function(i) {
  ataf18[,i] / ataf18_total_fila}))

# Dividir cada observación entre el total de las columnas
ataf18_norm_col <- as.data.frame(sapply(1:ncol(ataf18[, ]), function(i) {
  ataf18[,i ] / ataf18_total_col[i]}))

# Dividir cada observación entre el total general
ataf18__norm_total_fila <- ataf18_norm_fila / ataf18_total_general
ataf18__norm_total_col <- ataf18_norm_col / ataf18_total_general

ataf18i_n <- ataf18_total_fila/ataf18_total_general
ataf18r_n <- ataf18_total_col/ataf18_total_general

dtaf18g <- c18 %>%
  select(AE, NOMGEO, dtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = dtaf)

# Configurar AE como nombres de fila
dtaf18 <- as.data.frame(dtaf18g)
rownames(dtaf18) <- dtaf18g$AE
dtaf18 <- dtaf18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
dtaf18_total_fila <- rowSums(dtaf18, na.rm = T)

# Calcular los totales de columnas
dtaf18_total_col <- colSums(dtaf18, na.rm = T)

# Calcular el total general
dtaf18_total_general <- sum(dtaf18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
dtaf18_norm_fila <- as.data.frame(sapply(1:ncol(dtaf18[,]), function(i) {
  dtaf18[,i] / dtaf18_total_fila}))

# Dividir cada observación entre el total de las columnas
dtaf18_norm_col <- as.data.frame(sapply(1:ncol(dtaf18[, ]), function(i) {
  dtaf18[,i ] / dtaf18_total_col[i]}))

# Dividir cada observación entre el total general
dtaf18__norm_total_fila <- dtaf18_norm_fila / dtaf18_total_general
dtaf18__norm_total_col <- dtaf18_norm_col / dtaf18_total_general

dtaf18i_n <- dtaf18_total_fila/dtaf18_total_general
dtaf18r_n <- dtaf18_total_col/dtaf18_total_general

tga18g <- c18 %>%
  select(AE, NOMGEO, tga) %>%
  pivot_wider(names_from = NOMGEO, values_from = tga)

# Configurar AE como nombres de fila
tga18 <- as.data.frame(tga18g)
rownames(tga18) <- tga18g$AE
tga18 <- tga18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
tga18_total_fila <- rowSums(tga18, na.rm = T)

# Calcular los totales de columnas
tga18_total_col <- colSums(tga18, na.rm = T)

# Calcular el total general
tga18_total_general <- sum(tga18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
tga18_norm_fila <- as.data.frame(sapply(1:ncol(tga18[,]), function(i) {
  tga18[,i] / tga18_total_fila}))

# Dividir cada observación entre el total de las columnas
tga18_norm_col <- as.data.frame(sapply(1:ncol(tga18[, ]), function(i) {
  tga18[,i ] / tga18_total_col[i]}))

# Dividir cada observación entre el total general
tga18__norm_total_fila <- tga18_norm_fila / tga18_total_general
tga18__norm_total_col <- tga18_norm_col / tga18_total_general

tga18i_n <- tga18_total_fila/tga18_total_general
tga18r_n <- tga18_total_col/tga18_total_general

tin18g <- c18 %>%
  select(AE, NOMGEO, tin) %>%
  pivot_wider(names_from = NOMGEO, values_from = tin)

# Configurar AE como nombres de fila
tin18 <- as.data.frame(tin18g)
rownames(tin18) <- tin18g$AE
tin18 <- tin18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
tin18_total_fila <- rowSums(tin18, na.rm = T)

# Calcular los totales de columnas
tin18_total_col <- colSums(tin18, na.rm = T)

# Calcular el total general
tin18_total_general <- sum(tin18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
tin18_norm_fila <- as.data.frame(sapply(1:ncol(tin18[,]), function(i) {
  tin18[,i] / tin18_total_fila}))

# Dividir cada observación entre el total de las columnas
tin18_norm_col <- as.data.frame(sapply(1:ncol(tin18[, ]), function(i) {
  tin18[,i ] / tin18_total_col[i]}))

# Dividir cada observación entre el total general
tin18__norm_total_fila <- tin18_norm_fila / tin18_total_general
tin18__norm_total_col <- tin18_norm_col / tin18_total_general

tin18i_n <- tin18_total_fila/tin18_total_general
tin18r_n <- tin18_total_col/tin18_total_general

pbt18g <- c18 %>%
  select(AE, NOMGEO, pbt) %>%
  pivot_wider(names_from = NOMGEO, values_from = pbt)

# Configurar AE como nombres de fila
pbt18 <- as.data.frame(pbt18g)
rownames(pbt18) <- pbt18g$AE
pbt18 <- pbt18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
pbt18_total_fila <- rowSums(pbt18, na.rm = T)

# Calcular los totales de columnas
pbt18_total_col <- colSums(pbt18, na.rm = T)

# Calcular el total general
pbt18_total_general <- sum(pbt18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
pbt18_norm_fila <- as.data.frame(sapply(1:ncol(pbt18[,]), function(i) {
  pbt18[,i] / pbt18_total_fila}))

# Dividir cada observación entre el total de las columnas
pbt18_norm_col <- as.data.frame(sapply(1:ncol(pbt18[, ]), function(i) {
  pbt18[,i ] / pbt18_total_col[i]}))

# Dividir cada observación entre el total general
pbt18__norm_total_fila <- pbt18_norm_fila / pbt18_total_general
pbt18__norm_total_col <- pbt18_norm_col / pbt18_total_general

pbt18i_n <- pbt18_total_fila/pbt18_total_general
pbt18r_n <- pbt18_total_col/pbt18_total_general

it18g <- c18 %>%
  select(AE, NOMGEO, it) %>%
  pivot_wider(names_from = NOMGEO, values_from = it)

# Configurar AE como nombres de fila
it18 <- as.data.frame(it18g)
rownames(it18) <- it18g$AE
it18 <- it18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
it18_total_fila <- rowSums(it18, na.rm = T)

# Calcular los totales de columnas
it18_total_col <- colSums(it18, na.rm = T)

# Calcular el total general
it18_total_general <- sum(it18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
it18_norm_fila <- as.data.frame(sapply(1:ncol(it18[,]), function(i) {
  it18[,i] / it18_total_fila}))

# Dividir cada observación entre el total de las columnas
it18_norm_col <- as.data.frame(sapply(1:ncol(it18[, ]), function(i) {
  it18[,i ] / it18_total_col[i]}))

# Dividir cada observación entre el total general
it18__norm_total_fila <- it18_norm_fila / it18_total_general
it18__norm_total_col <- it18_norm_col / it18_total_general

it18i_n <- it18_total_fila/it18_total_general
it18r_n <- it18_total_col/it18_total_general

pot18g <- c18 %>%
  select(AE, NOMGEO, pot) %>%
  pivot_wider(names_from = NOMGEO, values_from = pot)

# Configurar AE como nombres de fila
pot18 <- as.data.frame(pot18g)
rownames(pot18) <- pot18g$AE
pot18 <- pot18 %>% select(-AE)

# Relaciones básicas filas y columnas

# Calcular los totales de filas
pot18_total_fila <- rowSums(pot18, na.rm = T)

# Calcular los totales de columnas
pot18_total_col <- colSums(pot18, na.rm = T)

# Calcular el total general
pot18_total_general <- sum(pot18_total_fila, na.rm = T)

# Dividir cada observación entre el total de las filas
pot18_norm_fila <- as.data.frame(sapply(1:ncol(pot18[,]), function(i) {
  pot18[,i] / pot18_total_fila}))

# Dividir cada observación entre el total de las columnas
pot18_norm_col <- as.data.frame(sapply(1:ncol(pot18[, ]), function(i) {
  pot18[,i ] / pot18_total_col[i]}))

# Dividir cada observación entre el total general
pot18__norm_total_fila <- pot18_norm_fila / pot18_total_general
pot18__norm_total_col <- pot18_norm_col / pot18_total_general

pot18i_n <- pot18_total_fila/pot18_total_general
pot18r_n <- pot18_total_col/pot18_total_general

htpot18g <- c18 %>%
  select(AE, NOMGEO, htpot) %>%
  pivot_wider(names_from = NOMGEO, values_from = htpot)


htpot18 <- as.data.frame(htpot18g)
rownames(htpot18) <- htpot18g$AE
htpot18 <- htpot18 %>% select(-AE)

htpot18_total_fila <- rowSums(htpot18, na.rm = T)

htpot18_total_col <- colSums(htpot18, na.rm = T)

htpot18_total_general <- sum(htpot18_total_fila, na.rm = T)

htpot18_norm_fila <- as.data.frame(sapply(1:ncol(htpot18[,]), function(i) {
  htpot18[,i] / htpot18_total_fila}))

htpot18_norm_col <- as.data.frame(sapply(1:ncol(htpot18[, ]), function(i) {
  htpot18[,i ] / htpot18_total_col[i]}))

htpot18__norm_total_fila <- htpot18_norm_fila / htpot18_total_general
htpot18__norm_total_col <- htpot18_norm_col / htpot18_total_general

htpot18i_n <- htpot18_total_fila/htpot18_total_general
htpot18r_n <- htpot18_total_col/htpot18_total_general

vtaf18g <- c18 %>%
  select(AE, NOMGEO, vtaf) %>%
  pivot_wider(names_from = NOMGEO, values_from = vtaf)
vtaf18 <- as.data.frame(vtaf18g)
rownames(vtaf18) <- vtaf18g$AE
vtaf18 <- vtaf18 %>% select(-AE)
vtaf18_total_fila <- rowSums(vtaf18, na.rm = T)
vtaf18_total_col <- colSums(vtaf18, na.rm = T)
vtaf18_total_general <- sum(vtaf18_total_fila, na.rm = T)
vtaf18_norm_fila <- as.data.frame(sapply(1:ncol(vtaf18[,]), function(i) {
  vtaf18[,i] / vtaf18_total_fila}))
vtaf18_norm_col <- as.data.frame(sapply(1:ncol(vtaf18[, ]), function(i) {
  vtaf18[,i ] / vtaf18_total_col[i]}))
vtaf18__norm_total_fila <- vtaf18_norm_fila / vtaf18_total_general
vtaf18__norm_total_col <- vtaf18_norm_col / vtaf18_total_general

vtaf18i_n <- vtaf18_total_fila/vtaf18_total_general
vtaf18r_n <- vtaf18_total_col/vtaf18_total_general

afpp18g <- c18 %>%
  select(AE, NOMGEO, afpp) %>%
  pivot_wider(names_from = NOMGEO, values_from = afpp)
afpp18 <- as.data.frame(afpp18g)
rownames(afpp18) <- afpp18g$AE
afpp18 <- afpp18 %>% select(-AE)
afpp18_total_fila <- rowSums(afpp18, na.rm = T)
afpp18_total_col <- colSums(afpp18, na.rm = T)
afpp18_total_general <- sum(afpp18_total_fila, na.rm = T)
afpp18_norm_fila <- as.data.frame(sapply(1:ncol(afpp18[,]), function(i) {
  afpp18[,i] / afpp18_total_fila}))
afpp18_norm_col <- as.data.frame(sapply(1:ncol(afpp18[, ]), function(i) {
  afpp18[,i ] / afpp18_total_col[i]}))
afpp18__norm_total_fila <- afpp18_norm_fila / afpp18_total_general
afpp18__norm_total_col <- afpp18_norm_col / afpp18_total_general

afpp18i_n <- afpp18_total_fila/afpp18_total_general
afpp18r_n <- afpp18_total_col/afpp18_total_general

atmep18g <- c18 %>%
  select(AE, NOMGEO, atmep) %>%
  pivot_wider(names_from = NOMGEO, values_from = atmep)
atmep18 <- as.data.frame(atmep18g)
rownames(atmep18) <- atmep18g$AE
atmep18 <- atmep18 %>% select(-AE)
atmep18_total_fila <- rowSums(atmep18, na.rm = T)
atmep18_total_col <- colSums(atmep18, na.rm = T)
atmep18_total_general <- sum(atmep18_total_fila, na.rm = T)
atmep18_norm_fila <- as.data.frame(sapply(1:ncol(atmep18[,]), function(i) {
  atmep18[,i] / atmep18_total_fila}))
atmep18_norm_col <- as.data.frame(sapply(1:ncol(atmep18[, ]), function(i) {
  atmep18[,i ] / atmep18_total_col[i]}))
atmep18__norm_total_fila <- atmep18_norm_fila / atmep18_total_general
atmep18__norm_total_col <- atmep18_norm_col / atmep18_total_general

atmep18i_n <- atmep18_total_fila/atmep18_total_general
atmep18r_n <- atmep18_total_col/atmep18_total_general

atbi18g <- c18 %>%
  select(AE, NOMGEO, atbi) %>%
  pivot_wider(names_from = NOMGEO, values_from = atbi)
atbi18 <- as.data.frame(atbi18g)
rownames(atbi18) <- atbi18g$AE
atbi18 <- atbi18 %>% select(-AE)
atbi18_total_fila <- rowSums(atbi18, na.rm = T)
atbi18_total_col <- colSums(atbi18, na.rm = T)
atbi18_total_general <- sum(atbi18_total_fila, na.rm = T)
atbi18_norm_fila <- as.data.frame(sapply(1:ncol(atbi18[,]), function(i) {
  atbi18[,i] / atbi18_total_fila}))
atbi18_norm_col <- as.data.frame(sapply(1:ncol(atbi18[, ]), function(i) {
  atbi18[,i ] / atbi18_total_col[i]}))
atbi18__norm_total_fila <- atbi18_norm_fila / atbi18_total_general
atbi18__norm_total_col <- atbi18_norm_col / atbi18_total_general

atbi18i_n <- atbi18_total_fila/atbi18_total_general
atbi18r_n <- atbi18_total_col/atbi18_total_general

atuet18g <- c18 %>%
  select(AE, NOMGEO, atuet) %>%
  pivot_wider(names_from = NOMGEO, values_from = atuet)
atuet18 <- as.data.frame(atuet18g)
rownames(atuet18) <- atuet18g$AE
atuet18 <- atuet18 %>% select(-AE)
atuet18_total_fila <- rowSums(atuet18, na.rm = T)
atuet18_total_col <- colSums(atuet18, na.rm = T)
atuet18_total_general <- sum(atuet18_total_fila, na.rm = T)
atuet18_norm_fila <- as.data.frame(sapply(1:ncol(atuet18[,]), function(i) {
  atuet18[,i] / atuet18_total_fila}))
atuet18_norm_col <- as.data.frame(sapply(1:ncol(atuet18[, ]), function(i) {
  atuet18[,i ] / atuet18_total_col[i]}))
atuet18__norm_total_fila <- atuet18_norm_fila / atuet18_total_general
atuet18__norm_total_col <- atuet18_norm_col / atuet18_total_general

atuet18i_n <- atuet18_total_fila/atuet18_total_general
atuet18r_n <- atuet18_total_col/atuet18_total_general

atecp18g <- c18 %>%
  select(AE, NOMGEO, atecp) %>%
  pivot_wider(names_from = NOMGEO, values_from = atecp)
atecp18 <- as.data.frame(atecp18g)
rownames(atecp18) <- atecp18g$AE
atecp18 <- atecp18 %>% select(-AE)
atecp18_total_fila <- rowSums(atecp18, na.rm = T)
atecp18_total_col <- colSums(atecp18, na.rm = T)
atecp18_total_general <- sum(atecp18_total_fila, na.rm = T)
atecp18_norm_fila <- as.data.frame(sapply(1:ncol(atecp18[,]), function(i) {
  atecp18[,i] / atecp18_total_fila}))
atecp18_norm_col <- as.data.frame(sapply(1:ncol(atecp18[, ]), function(i) {
  atecp18[,i ] / atecp18_total_col[i]}))
atecp18__norm_total_fila <- atecp18_norm_fila / atecp18_total_general
atecp18__norm_total_col <- atecp18_norm_col / atecp18_total_general

atecp18i_n <- atecp18_total_fila/atecp18_total_general
atecp18r_n <- atecp18_total_col/atecp18_total_general

atma18g <- c18 %>%
  select(AE, NOMGEO, atma) %>%
  pivot_wider(names_from = NOMGEO, values_from = atma)
atma18 <- as.data.frame(atma18g)
rownames(atma18) <- atma18g$AE
atma18 <- atma18 %>% select(-AE)
atma18_total_fila <- rowSums(atma18, na.rm = T)
atma18_total_col <- colSums(atma18, na.rm = T)
atma18_total_general <- sum(atma18_total_fila, na.rm = T)
atma18_norm_fila <- as.data.frame(sapply(1:ncol(atma18[,]), function(i) {
  atma18[,i] / atma18_total_fila}))
atma18_norm_col <- as.data.frame(sapply(1:ncol(atma18[, ]), function(i) {
  atma18[,i ] / atma18_total_col[i]}))
atma18__norm_total_fila <- atma18_norm_fila / atma18_total_general
atma18__norm_total_col <- atma18_norm_col / atma18_total_general

atma18i_n <- atma18_total_fila/atma18_total_general
atma18r_n <- atma18_total_col/atma18_total_general


######## Cálculo de los índices espaciales

eepacd03 <- (pacd03_norm_col/ue03_norm_col)/(pacd03_total_general/ue03_total_general)
eepacd03 <- eepacd03
rownames(eepacd03) <- pacd03g$AE
colnames(eepacd03) <- colnames(pacd03g[,-1])
i_eepacd03 <- colSums(eepacd03[,], na.rm = TRUE)
i_eepacd03 <- t(i_eepacd03)
it_eepacd03 <- t(i_eepacd03)

eeppvs03 <- (ppvs03_norm_col/ue03_norm_col)/(ppvs03_total_general/ue03_total_general)
eeppvs03 <- eeppvs03
rownames(eeppvs03) <- ppvs03g$AE
colnames(eeppvs03) <- colnames(ppvs03g[,-1])
i_eeppvs03 <- colSums(eeppvs03[,], na.rm = TRUE)
i_eeppvs03 <- t(i_eeppvs03)
it_eeppvs03 <- t(i_eeppvs03)


marpacd03 <- (pacd03_norm_col/pacd03i_n)
marpacd03 <- marpacd03
rownames(marpacd03) <- pacd03g$AE
colnames(marpacd03) <- colnames(pacd03g[,-1])
i_marpacd03 <- colSums(marpacd03[,], na.rm = TRUE)
i_marpacd03 <- t(i_marpacd03)
it_marpacd03 <- t(i_marpacd03)

marppvs03 <- (ppvs03_norm_col/ppvs03i_n)
marppvs03 <- marppvs03
rownames(marppvs03) <- ppvs03g$AE
colnames(marppvs03) <- colnames(ppvs03g[,-1])
i_marppvs03 <- colSums(marppvs03[,], na.rm = TRUE)
i_marppvs03 <- t(i_marppvs03)
it_marppvs03 <- t(i_marppvs03)

comppvs03 <- 1/(ppvs03_total_col/ppvs03_total_general)^2

i_comppvs03 <- t(comppvs03)
it_comppvs03 <- t(i_comppvs03)

compacd03 <- 1/(pacd03_total_col/pacd03_total_general)^2

i_compacd03 <- t(compacd03)
it_compacd03 <- t(i_compacd03)

divppvs03 <- apply(t(ppvs03), 1, function(x) diversity(x, index = "shannon"))

i_divppvs03 <- t(divppvs03)
it_divppvs03 <- t(i_divppvs03)

divpacd03 <- apply(t(pacd03), 1, function(x) diversity(x, index = "shannon"))

i_divpacd03 <- t(divpacd03)
it_divpacd03 <- t(i_divpacd03)

prppvs03 <- (((vacb03_total_col/ppvs03_total_col)/(vacb03_total_general/ppvs03_total_general))+
  ((pbt03_total_col/ppvs03_total_col)/(pbt03_total_general/ppvs03_total_general)))/2

i_prppvs03 <- t(prppvs03)
it_prppvs03 <- t(i_prppvs03)

prpacd03 <- (((vacb03_total_col/pacd03_total_col)/(vacb03_total_general/pacd03_total_general))+
               ((pbt03_total_col/pacd03_total_col)/(pbt03_total_general/pacd03_total_general)))/2

i_prpacd03 <- t(prpacd03)
it_prpacd03 <- t(i_prpacd03)

srppvs03 <- (tsppvs03/ppvs03)/(tsppvs03_total_general/ppvs03_total_general)

srppvs03 <- srppvs03

rownames(srppvs03) <- pacd03g$AE
colnames(srppvs03) <- colnames(pacd03g[,-1])

i_srppvs03 <- colSums(srppvs03[,], na.rm = TRUE)
i_srppvs03 <- t(i_srppvs03)
it_srppvs03 <- t(i_srppvs03)

srpacd03 <- (tspacd03/pacd03)/(tspacd03_total_general/pacd03_total_general)

srpacd03 <- srpacd03

rownames(srpacd03) <- pacd03g$AE
colnames(srpacd03) <- colnames(pacd03g[,-1])

i_srpacd03 <- colSums(srpacd03[,], na.rm = TRUE)
i_srpacd03 <- t(i_srpacd03)
it_srpacd03 <- t(i_srpacd03)

roa03 <- (tin03_total_col-tga03_total_col)/(ataf03_total_col-dtaf03_total_col)

roa03 <- roa03
i_roa03 <- t(roa03)
it_roa03 <- t(i_roa03)

rat03 <- (tin03_total_col)/(ataf03_total_col-dtaf03_total_col)

rat03 <- rat03
i_rat03 <- t(rat03)
it_rat03 <- t(i_rat03)

roic03 <- (vacb03_total_col)/(ataf03_total_col-dtaf03_total_col)

roic03 <- roic03
i_roic03 <- t(roic03)
it_roic03 <- t(i_roic03)

dca03 <- (dtaf03_total_col)/(ataf03_total_col)

dca03 <- dca03
i_dca03 <- t(dca03)
it_dca03 <- t(i_dca03)

iaf03 <- (ataf03_total_col-dtaf03_total_col)/(tin03_total_col)

iaf03 <- iaf03
i_iaf03 <- t(iaf03)
it_iaf03 <- t(i_iaf03)

mbi03 <- (tin03_total_col-tga03_total_col)/(tin03_total_col)

mbi03 <- mbi03
i_mbi03 <- t(mbi03)
it_mbi03 <- t(i_mbi03)

ptf03 <- (vacb03_total_col)/((pot03_total_col/htpot03_total_col)+ (ataf03_total_col-dtaf03_total_col))

ptf03 <- ptf03
i_ptf03 <- t(ptf03)
it_ptf03 <- t(i_ptf03)

roavtaf03 <- (tin03_total_col-tga03_total_col)/(vtaf03_total_col-dtaf03_total_col)
roavtaf03 <- roavtaf03
i_roavtaf03 <- t(roavtaf03)
it_roavtaf03 <- t(i_roavtaf03)
roaafpp03 <- (tin03_total_col-tga03_total_col)/(afpp03_total_col-dtaf03_total_col)
roaafpp03 <- roaafpp03
i_roaafpp03 <- t(roaafpp03)
it_roaafpp03 <- t(i_roaafpp03)
roaatmep03 <- (tin03_total_col-tga03_total_col)/(atmep03_total_col-dtaf03_total_col)
roaatmep03 <- roaatmep03
i_roaatmep03 <- t(roaatmep03)
it_roaatmep03 <- t(i_roaatmep03)
roaatbi03 <- (tin03_total_col-tga03_total_col)/(atbi03_total_col-dtaf03_total_col)
roaatbi03 <- roaatbi03
i_roaatbi03 <- t(roaatbi03)
it_roaatbi03 <- t(i_roaatbi03)
roaatuet03 <- (tin03_total_col-tga03_total_col)/(atuet03_total_col-dtaf03_total_col)
roaatuet03 <- roaatuet03
i_roaatuet03 <- t(roaatuet03)
it_roaatuet03 <- t(i_roaatuet03)
roaatecp03 <- (tin03_total_col-tga03_total_col)/(atecp03_total_col-dtaf03_total_col)
roaatecp03 <- roaatecp03
i_roaatecp03 <- t(roaatecp03)
it_roaatecp03 <- t(i_roaatecp03)
roaatma03 <- (tin03_total_col-tga03_total_col)/(atma03_total_col-dtaf03_total_col)
roaatma03 <- roaatma03
i_roaatma03 <- t(roaatma03)
it_roaatma03 <- t(i_roaatma03)

ratvtaf03 <- (tin03_total_col)/(vtaf03_total_col-dtaf03_total_col)
ratvtaf03 <- ratvtaf03
i_ratvtaf03 <- t(ratvtaf03)
it_ratvtaf03 <- t(i_ratvtaf03)
ratafpp03 <- (tin03_total_col)/(afpp03_total_col-dtaf03_total_col)
ratafpp03 <- ratafpp03
i_ratafpp03 <- t(ratafpp03)
it_ratafpp03 <- t(i_ratafpp03)
ratatmep03 <- (tin03_total_col)/(atmep03_total_col-dtaf03_total_col)
ratatmep03 <- ratatmep03
i_ratatmep03 <- t(ratatmep03)
it_ratatmep03 <- t(i_ratatmep03)
ratatbi03 <- (tin03_total_col)/(atbi03_total_col-dtaf03_total_col)
ratatbi03 <- ratatbi03
i_ratatbi03 <- t(ratatbi03)
it_ratatbi03 <- t(i_ratatbi03)
ratatuet03 <- (tin03_total_col)/(atuet03_total_col-dtaf03_total_col)
ratatuet03 <- ratatuet03
i_ratatuet03 <- t(ratatuet03)
it_ratatuet03 <- t(i_ratatuet03)
ratatecp03 <- (tin03_total_col)/(atecp03_total_col-dtaf03_total_col)
ratatecp03 <- ratatecp03
i_ratatecp03 <- t(ratatecp03)
it_ratatecp03 <- t(i_ratatecp03)
ratatma03 <- (tin03_total_col)/(atma03_total_col-dtaf03_total_col)
ratatma03 <- ratatma03
i_ratatma03 <- t(ratatma03)
it_ratatma03 <- t(i_ratatma03)

roicvtaf03 <- (vacb03_total_col)/(vtaf03_total_col-dtaf03_total_col)
roicvtaf03 <- roicvtaf03
i_roicvtaf03 <- t(roicvtaf03)
it_roicvtaf03 <- t(i_roicvtaf03)
roicafpp03 <- (vacb03_total_col)/(afpp03_total_col-dtaf03_total_col)
roicafpp03 <- roicafpp03
i_roicafpp03 <- t(roicafpp03)
it_roicafpp03 <- t(i_roicafpp03)
roicatmep03 <- (vacb03_total_col)/(atmep03_total_col-dtaf03_total_col)
roicatmep03 <- roicatmep03
i_roicatmep03 <- t(roicatmep03)
it_roicatmep03 <- t(i_roicatmep03)
roicatbi03 <- (vacb03_total_col)/(atbi03_total_col-dtaf03_total_col)
roicatbi03 <- roicatbi03
i_roicatbi03 <- t(roicatbi03)
it_roicatbi03 <- t(i_roicatbi03)
roicatuet03 <- (vacb03_total_col)/(atuet03_total_col-dtaf03_total_col)
roicatuet03 <- roicatuet03
i_roicatuet03 <- t(roicatuet03)
it_roicatuet03 <- t(i_roicatuet03)
roicatecp03 <- (vacb03_total_col)/(atecp03_total_col-dtaf03_total_col)
roicatecp03 <- roicatecp03
i_roicatecp03 <- t(roicatecp03)
it_roicatecp03 <- t(i_roicatecp03)
roicatma03 <- (vacb03_total_col)/(atma03_total_col-dtaf03_total_col)
roicatma03 <- roicatma03
i_roicatma03 <- t(roicatma03)
it_roicatma03 <- t(i_roicatma03)

dcavtaf03 <- (dtaf03_total_col)/(vtaf03_total_col)
dcavtaf03 <- dcavtaf03
i_dcavtaf03 <- t(dcavtaf03)
it_dcavtaf03 <- t(i_dcavtaf03)
dcaafpp03 <- (dtaf03_total_col)/(afpp03_total_col)
dcaafpp03 <- dcaafpp03
i_dcaafpp03 <- t(dcaafpp03)
it_dcaafpp03 <- t(i_dcaafpp03)
dcaatmep03 <- (dtaf03_total_col)/(atmep03_total_col)
dcaatmep03 <- dcaatmep03
i_dcaatmep03 <- t(dcaatmep03)
it_dcaatmep03 <- t(i_dcaatmep03)
dcaatbi03 <- (dtaf03_total_col)/(atbi03_total_col)
dcaatbi03 <- dcaatbi03
i_dcaatbi03 <- t(dcaatbi03)
it_dcaatbi03 <- t(i_dcaatbi03)
dcaatuet03 <- (dtaf03_total_col)/(atuet03_total_col)
dcaatuet03 <- dcaatuet03
i_dcaatuet03 <- t(dcaatuet03)
it_dcaatuet03 <- t(i_dcaatuet03)
dcaatecp03 <- (dtaf03_total_col)/(atecp03_total_col)
dcaatecp03 <- dcaatecp03
i_dcaatecp03 <- t(dcaatecp03)
it_dcaatecp03 <- t(i_dcaatecp03)
dcaatma03 <- (dtaf03_total_col)/(atma03_total_col)
dcaatma03 <- dcaatma03
i_dcaatma03 <- t(dcaatma03)
it_dcaatma03 <- t(i_dcaatma03)

iafvtaf03 <- (vtaf03_total_col-dtaf03_total_col)/(tin03_total_col)
iafvtaf03 <- iafvtaf03
i_iafvtaf03 <- t(iafvtaf03)
it_iafvtaf03 <- t(i_iafvtaf03)
iafafpp03 <- (afpp03_total_col-dtaf03_total_col)/(tin03_total_col)
iafafpp03 <- iafafpp03
i_iafafpp03 <- t(iafafpp03)
it_iafafpp03 <- t(i_iafafpp03)
iafatmep03 <- (atmep03_total_col-dtaf03_total_col)/(tin03_total_col)
iafatmep03 <- iafatmep03
i_iafatmep03 <- t(iafatmep03)
it_iafatmep03 <- t(i_iafatmep03)
iafatbi03 <- (atbi03_total_col-dtaf03_total_col)/(tin03_total_col)
iafatbi03 <- iafatbi03
i_iafatbi03 <- t(iafatbi03)
it_iafatbi03 <- t(i_iafatbi03)
iafatuet03 <- (atuet03_total_col-dtaf03_total_col)/(tin03_total_col)
iafatuet03 <- iafatuet03
i_iafatuet03 <- t(iafatuet03)
it_iafatuet03 <- t(i_iafatuet03)
iafatecp03 <- (atecp03_total_col-dtaf03_total_col)/(tin03_total_col)
iafatecp03 <- iafatecp03
i_iafatecp03 <- t(iafatecp03)
it_iafatecp03 <- t(i_iafatecp03)
iafatma03 <- (atma03_total_col-dtaf03_total_col)/(tin03_total_col)
iafatma03 <- iafatma03
i_iafatma03 <- t(iafatma03)
it_iafatma03 <- t(i_iafatma03)

eepacd08 <- (pacd08_norm_col/ue08_norm_col)/(pacd08_total_general/ue08_total_general)
eepacd08 <- eepacd08
rownames(eepacd08) <- pacd08g$AE
colnames(eepacd08) <- colnames(pacd08g[,-1])
i_eepacd08 <- colSums(eepacd08[,], na.rm = TRUE)
i_eepacd08 <- t(i_eepacd08)
it_eepacd08 <- t(i_eepacd08)

eeppvs08 <- (ppvs08_norm_col/ue08_norm_col)/(ppvs08_total_general/ue08_total_general)
eeppvs08 <- eeppvs08
rownames(eeppvs08) <- ppvs08g$AE
colnames(eeppvs08) <- colnames(ppvs08g[,-1])
i_eeppvs08 <- colSums(eeppvs08[,], na.rm = TRUE)
i_eeppvs08 <- t(i_eeppvs08)
it_eeppvs08 <- t(i_eeppvs08)

marppvs08 <- (ppvs08_norm_col/ppvs08i_n)
marppvs08 <- marppvs08
rownames(marppvs08) <- ppvs08g$AE
colnames(marppvs08) <- colnames(ppvs08g[,-1])
i_marppvs08 <- colSums(marppvs08[,], na.rm = TRUE)
i_marppvs08 <- t(i_marppvs08)
it_marppvs08 <- t(i_marppvs08)

marpacd08 <- (pacd08_norm_col/pacd08i_n)
marpacd08 <- marpacd08
rownames(marpacd08) <- pacd08g$AE
colnames(marpacd08) <- colnames(pacd08g[,-1])
i_marpacd08 <- colSums(marpacd08[,], na.rm = TRUE)
i_marpacd08 <- t(i_marpacd08)
it_marpacd08 <- t(i_marpacd08)

comppvs08 <- 1/(ppvs08_total_col/ppvs08_total_general)^2

i_comppvs08 <- t(comppvs08)
it_comppvs08 <- t(i_comppvs08)

compacd08 <- 1/(pacd08_total_col/pacd08_total_general)^2

i_compacd08 <- t(compacd08)
it_compacd08 <- t(i_compacd08)

divppvs08 <- apply(t(ppvs08), 1, function(x) diversity(x, index = "shannon"))

i_divppvs08 <- t(divppvs08)
it_divppvs08 <- t(i_divppvs08)

divpacd08 <- apply(t(pacd08), 1, function(x) diversity(x, index = "shannon"))

i_divpacd08 <- t(divpacd08)
it_divpacd08 <- t(i_divpacd08)

prppvs08 <- (((vacb08_total_col/ppvs08_total_col)/(vacb08_total_general/ppvs08_total_general))+
               ((pbt08_total_col/ppvs08_total_col)/(pbt08_total_general/ppvs08_total_general)))/2

i_prppvs08 <- t(prppvs08)
it_prppvs08 <- t(i_prppvs08)

prpacd08 <- (((vacb08_total_col/pacd08_total_col)/(vacb08_total_general/pacd08_total_general))+
               ((pbt08_total_col/pacd08_total_col)/(pbt08_total_general/pacd08_total_general)))/2

i_prpacd08 <- t(prpacd08)
it_prpacd08 <- t(i_prpacd08)

srppvs08 <- (tsppvs08/ppvs08)/(tsppvs08_total_general/ppvs08_total_general)

srppvs08 <- srppvs08

rownames(srppvs08) <- pacd08g$AE
colnames(srppvs08) <- colnames(pacd08g[,-1])

i_srppvs08 <- colSums(srppvs08[,], na.rm = TRUE)
i_srppvs08 <- t(i_srppvs08)
it_srppvs08 <- t(i_srppvs08)

srpacd08 <- (tspacd08/pacd08)/(tspacd08_total_general/pacd08_total_general)

srpacd08 <- srpacd08

rownames(srpacd08) <- pacd08g$AE
colnames(srpacd08) <- colnames(pacd08g[,-1])

i_srpacd08 <- colSums(srpacd08[,], na.rm = TRUE)
i_srpacd08 <- t(i_srpacd08)
it_srpacd08 <- t(i_srpacd08)

roa08 <- (tin08_total_col-tga08_total_col)/(ataf08_total_col-dtaf08_total_col)

roa08 <- roa08
i_roa08 <- t(roa08)
it_roa08 <- t(i_roa08)

rat08 <- (tin08_total_col)/(ataf08_total_col-dtaf08_total_col)

rat08 <- rat08
i_rat08 <- t(rat08)
it_rat08 <- t(i_rat08)

roic08 <- (vacb08_total_col)/(ataf08_total_col-dtaf08_total_col)

roic08 <- roic08
i_roic08 <- t(roic08)
it_roic08 <- t(i_roic08)

dca08 <- (dtaf08_total_col)/(ataf08_total_col)

dca08 <- dca08
i_dca08 <- t(dca08)
it_dca08 <- t(i_dca08)

iaf08 <- (ataf08_total_col-dtaf08_total_col)/(tin08_total_col)

iaf08 <- iaf08
i_iaf08 <- t(iaf08)
it_iaf08 <- t(i_iaf08)

mbi08 <- (tin08_total_col-tga08_total_col)/(tin08_total_col)

mbi08 <- mbi08
i_mbi08 <- t(mbi08)
it_mbi08 <- t(i_mbi08)

ptf08 <- (vacb08_total_col)/((pot08_total_col/htpot08_total_col)+ (ataf08_total_col-dtaf08_total_col))

ptf08 <- ptf08
i_ptf08 <- t(ptf08)
it_ptf08 <- t(i_ptf08)

roavtaf08 <- (tin08_total_col-tga08_total_col)/(vtaf08_total_col-dtaf08_total_col)
roavtaf08 <- roavtaf08
i_roavtaf08 <- t(roavtaf08)
it_roavtaf08 <- t(i_roavtaf08)
roaafpp08 <- (tin08_total_col-tga08_total_col)/(afpp08_total_col-dtaf08_total_col)
roaafpp08 <- roaafpp08
i_roaafpp08 <- t(roaafpp08)
it_roaafpp08 <- t(i_roaafpp08)
roaatmep08 <- (tin08_total_col-tga08_total_col)/(atmep08_total_col-dtaf08_total_col)
roaatmep08 <- roaatmep08
i_roaatmep08 <- t(roaatmep08)
it_roaatmep08 <- t(i_roaatmep08)
roaatbi08 <- (tin08_total_col-tga08_total_col)/(atbi08_total_col-dtaf08_total_col)
roaatbi08 <- roaatbi08
i_roaatbi08 <- t(roaatbi08)
it_roaatbi08 <- t(i_roaatbi08)
roaatuet08 <- (tin08_total_col-tga08_total_col)/(atuet08_total_col-dtaf08_total_col)
roaatuet08 <- roaatuet08
i_roaatuet08 <- t(roaatuet08)
it_roaatuet08 <- t(i_roaatuet08)
roaatecp08 <- (tin08_total_col-tga08_total_col)/(atecp08_total_col-dtaf08_total_col)
roaatecp08 <- roaatecp08
i_roaatecp08 <- t(roaatecp08)
it_roaatecp08 <- t(i_roaatecp08)
roaatma08 <- (tin08_total_col-tga08_total_col)/(atma08_total_col-dtaf08_total_col)
roaatma08 <- roaatma08
i_roaatma08 <- t(roaatma08)
it_roaatma08 <- t(i_roaatma08)

ratvtaf08 <- (tin08_total_col)/(vtaf08_total_col-dtaf08_total_col)
ratvtaf08 <- ratvtaf08
i_ratvtaf08 <- t(ratvtaf08)
it_ratvtaf08 <- t(i_ratvtaf08)
ratafpp08 <- (tin08_total_col)/(afpp08_total_col-dtaf08_total_col)
ratafpp08 <- ratafpp08
i_ratafpp08 <- t(ratafpp08)
it_ratafpp08 <- t(i_ratafpp08)
ratatmep08 <- (tin08_total_col)/(atmep08_total_col-dtaf08_total_col)
ratatmep08 <- ratatmep08
i_ratatmep08 <- t(ratatmep08)
it_ratatmep08 <- t(i_ratatmep08)
ratatbi08 <- (tin08_total_col)/(atbi08_total_col-dtaf08_total_col)
ratatbi08 <- ratatbi08
i_ratatbi08 <- t(ratatbi08)
it_ratatbi08 <- t(i_ratatbi08)
ratatuet08 <- (tin08_total_col)/(atuet08_total_col-dtaf08_total_col)
ratatuet08 <- ratatuet08
i_ratatuet08 <- t(ratatuet08)
it_ratatuet08 <- t(i_ratatuet08)
ratatecp08 <- (tin08_total_col)/(atecp08_total_col-dtaf08_total_col)
ratatecp08 <- ratatecp08
i_ratatecp08 <- t(ratatecp08)
it_ratatecp08 <- t(i_ratatecp08)
ratatma08 <- (tin08_total_col)/(atma08_total_col-dtaf08_total_col)
ratatma08 <- ratatma08
i_ratatma08 <- t(ratatma08)
it_ratatma08 <- t(i_ratatma08)

roicvtaf08 <- (vacb08_total_col)/(vtaf08_total_col-dtaf08_total_col)
roicvtaf08 <- roicvtaf08
i_roicvtaf08 <- t(roicvtaf08)
it_roicvtaf08 <- t(i_roicvtaf08)
roicafpp08 <- (vacb08_total_col)/(afpp08_total_col-dtaf08_total_col)
roicafpp08 <- roicafpp08
i_roicafpp08 <- t(roicafpp08)
it_roicafpp08 <- t(i_roicafpp08)
roicatmep08 <- (vacb08_total_col)/(atmep08_total_col-dtaf08_total_col)
roicatmep08 <- roicatmep08
i_roicatmep08 <- t(roicatmep08)
it_roicatmep08 <- t(i_roicatmep08)
roicatbi08 <- (vacb08_total_col)/(atbi08_total_col-dtaf08_total_col)
roicatbi08 <- roicatbi08
i_roicatbi08 <- t(roicatbi08)
it_roicatbi08 <- t(i_roicatbi08)
roicatuet08 <- (vacb08_total_col)/(atuet08_total_col-dtaf08_total_col)
roicatuet08 <- roicatuet08
i_roicatuet08 <- t(roicatuet08)
it_roicatuet08 <- t(i_roicatuet08)
roicatecp08 <- (vacb08_total_col)/(atecp08_total_col-dtaf08_total_col)
roicatecp08 <- roicatecp08
i_roicatecp08 <- t(roicatecp08)
it_roicatecp08 <- t(i_roicatecp08)
roicatma08 <- (vacb08_total_col)/(atma08_total_col-dtaf08_total_col)
roicatma08 <- roicatma08
i_roicatma08 <- t(roicatma08)
it_roicatma08 <- t(i_roicatma08)

dcavtaf08 <- (dtaf08_total_col)/(vtaf08_total_col)
dcavtaf08 <- dcavtaf08
i_dcavtaf08 <- t(dcavtaf08)
it_dcavtaf08 <- t(i_dcavtaf08)
dcaafpp08 <- (dtaf08_total_col)/(afpp08_total_col)
dcaafpp08 <- dcaafpp08
i_dcaafpp08 <- t(dcaafpp08)
it_dcaafpp08 <- t(i_dcaafpp08)
dcaatmep08 <- (dtaf08_total_col)/(atmep08_total_col)
dcaatmep08 <- dcaatmep08
i_dcaatmep08 <- t(dcaatmep08)
it_dcaatmep08 <- t(i_dcaatmep08)
dcaatbi08 <- (dtaf08_total_col)/(atbi08_total_col)
dcaatbi08 <- dcaatbi08
i_dcaatbi08 <- t(dcaatbi08)
it_dcaatbi08 <- t(i_dcaatbi08)
dcaatuet08 <- (dtaf08_total_col)/(atuet08_total_col)
dcaatuet08 <- dcaatuet08
i_dcaatuet08 <- t(dcaatuet08)
it_dcaatuet08 <- t(i_dcaatuet08)
dcaatecp08 <- (dtaf08_total_col)/(atecp08_total_col)
dcaatecp08 <- dcaatecp08
i_dcaatecp08 <- t(dcaatecp08)
it_dcaatecp08 <- t(i_dcaatecp08)
dcaatma08 <- (dtaf08_total_col)/(atma08_total_col)
dcaatma08 <- dcaatma08
i_dcaatma08 <- t(dcaatma08)
it_dcaatma08 <- t(i_dcaatma08)

iafvtaf08 <- (vtaf08_total_col-dtaf08_total_col)/(tin08_total_col)
iafvtaf08 <- iafvtaf08
i_iafvtaf08 <- t(iafvtaf08)
it_iafvtaf08 <- t(i_iafvtaf08)
iafafpp08 <- (afpp08_total_col-dtaf08_total_col)/(tin08_total_col)
iafafpp08 <- iafafpp08
i_iafafpp08 <- t(iafafpp08)
it_iafafpp08 <- t(i_iafafpp08)
iafatmep08 <- (atmep08_total_col-dtaf08_total_col)/(tin08_total_col)
iafatmep08 <- iafatmep08
i_iafatmep08 <- t(iafatmep08)
it_iafatmep08 <- t(i_iafatmep08)
iafatbi08 <- (atbi08_total_col-dtaf08_total_col)/(tin08_total_col)
iafatbi08 <- iafatbi08
i_iafatbi08 <- t(iafatbi08)
it_iafatbi08 <- t(i_iafatbi08)
iafatuet08 <- (atuet08_total_col-dtaf08_total_col)/(tin08_total_col)
iafatuet08 <- iafatuet08
i_iafatuet08 <- t(iafatuet08)
it_iafatuet08 <- t(i_iafatuet08)
iafatecp08 <- (atecp08_total_col-dtaf08_total_col)/(tin08_total_col)
iafatecp08 <- iafatecp08
i_iafatecp08 <- t(iafatecp08)
it_iafatecp08 <- t(i_iafatecp08)
iafatma08 <- (atma08_total_col-dtaf08_total_col)/(tin08_total_col)
iafatma08 <- iafatma08
i_iafatma08 <- t(iafatma08)
it_iafatma08 <- t(i_iafatma08)

eepacd13 <- (pacd13_norm_col/ue13_norm_col)/(pacd13_total_general/ue13_total_general)
eepacd13 <- eepacd13
rownames(eepacd13) <- pacd13g$AE
colnames(eepacd13) <- colnames(pacd13g[,-1])
i_eepacd13 <- colSums(eepacd13[,], na.rm = TRUE)
i_eepacd13 <- t(i_eepacd13)
it_eepacd13 <- t(i_eepacd13)

eeppvs13 <- (ppvs13_norm_col/ue13_norm_col)/(ppvs13_total_general/ue13_total_general)
eeppvs13 <- eeppvs13
rownames(eeppvs13) <- ppvs13g$AE
colnames(eeppvs13) <- colnames(ppvs13g[,-1])
i_eeppvs13 <- colSums(eeppvs13[,], na.rm = TRUE)
i_eeppvs13 <- t(i_eeppvs13)
it_eeppvs13 <- t(i_eeppvs13)

marppvs13 <- (ppvs13_norm_col/ppvs13i_n)
marppvs13 <- marppvs13
rownames(marppvs13) <- ppvs13g$AE
colnames(marppvs13) <- colnames(ppvs13g[,-1])
i_marppvs13 <- colSums(marppvs13[,], na.rm = TRUE)
i_marppvs13 <- t(i_marppvs13)
it_marppvs13 <- t(i_marppvs13)

marpacd13 <- (pacd13_norm_col/pacd13i_n)
marpacd13 <- marpacd13
rownames(marpacd13) <- pacd13g$AE
colnames(marpacd13) <- colnames(pacd13g[,-1])
i_marpacd13 <- colSums(marpacd13[,], na.rm = TRUE)
i_marpacd13 <- t(i_marpacd13)
it_marpacd13 <- t(i_marpacd13)

comppvs13 <- 1/(ppvs13_total_col/ppvs13_total_general)^2

i_comppvs13 <- t(comppvs13)
it_comppvs13 <- t(i_comppvs13)

compacd13 <- 1/(pacd13_total_col/pacd13_total_general)^2

i_compacd13 <- t(compacd13)
it_compacd13 <- t(i_compacd13)

divppvs13 <- apply(t(ppvs13), 1, function(x) diversity(x, index = "shannon"))

i_divppvs13 <- t(divppvs13)
it_divppvs13 <- t(i_divppvs13)

divpacd13 <- apply(t(pacd13), 1, function(x) diversity(x, index = "shannon"))

i_divpacd13 <- t(divpacd13)
it_divpacd13 <- t(i_divpacd13)

prppvs13 <- (((vacb13_total_col/ppvs13_total_col)/(vacb13_total_general/ppvs13_total_general))+
               ((pbt13_total_col/ppvs13_total_col)/(pbt13_total_general/ppvs13_total_general)))/2

i_prppvs13 <- t(prppvs13)
it_prppvs13 <- t(i_prppvs13)

prpacd13 <- (((vacb13_total_col/pacd13_total_col)/(vacb13_total_general/pacd13_total_general))+
               ((pbt13_total_col/pacd13_total_col)/(pbt13_total_general/pacd13_total_general)))/2

i_prpacd13 <- t(prpacd13)
it_prpacd13 <- t(i_prpacd13)

srppvs13 <- (tsppvs13/ppvs13)/(tsppvs13_total_general/ppvs13_total_general)

srppvs13 <- srppvs13

rownames(srppvs13) <- pacd13g$AE
colnames(srppvs13) <- colnames(pacd13g[,-1])

i_srppvs13 <- colSums(srppvs13[,], na.rm = TRUE)
i_srppvs13 <- t(i_srppvs13)
it_srppvs13 <- t(i_srppvs13)

srpacd13 <- (tspacd13/pacd13)/(tspacd13_total_general/pacd13_total_general)

srpacd13 <- srpacd13

rownames(srpacd13) <- pacd13g$AE
colnames(srpacd13) <- colnames(pacd13g[,-1])

i_srpacd13 <- colSums(srpacd13[,], na.rm = TRUE)
i_srpacd13 <- t(i_srpacd13)
it_srpacd13 <- t(i_srpacd13)

roa13 <- (tin13_total_col-tga13_total_col)/(ataf13_total_col-dtaf13_total_col)

roa13 <- roa13
i_roa13 <- t(roa13)
it_roa13 <- t(i_roa13)

rat13 <- (tin13_total_col)/(ataf13_total_col-dtaf13_total_col)

rat13 <- rat13
i_rat13 <- t(rat13)
it_rat13 <- t(i_rat13)

roic13 <- (vacb13_total_col)/(ataf13_total_col-dtaf13_total_col)

roic13 <- roic13
i_roic13 <- t(roic13)
it_roic13 <- t(i_roic13)

dca13 <- (dtaf13_total_col)/(ataf13_total_col)

dca13 <- dca13
i_dca13 <- t(dca13)
it_dca13 <- t(i_dca13)

iaf13 <- (ataf13_total_col-dtaf13_total_col)/(tin13_total_col)

iaf13 <- iaf13
i_iaf13 <- t(iaf13)
it_iaf13 <- t(i_iaf13)

mbi13 <- (tin13_total_col-tga13_total_col)/(tin13_total_col)

mbi13 <- mbi13
i_mbi13 <- t(mbi13)
it_mbi13 <- t(i_mbi13)

ptf13 <- (vacb13_total_col)/((pot13_total_col/htpot13_total_col)+ (ataf13_total_col-dtaf13_total_col))

ptf13 <- ptf13
i_ptf13 <- t(ptf13)
it_ptf13 <- t(i_ptf13)

roavtaf13 <- (tin13_total_col-tga13_total_col)/(vtaf13_total_col-dtaf13_total_col)
roavtaf13 <- roavtaf13
i_roavtaf13 <- t(roavtaf13)
it_roavtaf13 <- t(i_roavtaf13)
roaafpp13 <- (tin13_total_col-tga13_total_col)/(afpp13_total_col-dtaf13_total_col)
roaafpp13 <- roaafpp13
i_roaafpp13 <- t(roaafpp13)
it_roaafpp13 <- t(i_roaafpp13)
roaatmep13 <- (tin13_total_col-tga13_total_col)/(atmep13_total_col-dtaf13_total_col)
roaatmep13 <- roaatmep13
i_roaatmep13 <- t(roaatmep13)
it_roaatmep13 <- t(i_roaatmep13)
roaatbi13 <- (tin13_total_col-tga13_total_col)/(atbi13_total_col-dtaf13_total_col)
roaatbi13 <- roaatbi13
i_roaatbi13 <- t(roaatbi13)
it_roaatbi13 <- t(i_roaatbi13)
roaatuet13 <- (tin13_total_col-tga13_total_col)/(atuet13_total_col-dtaf13_total_col)
roaatuet13 <- roaatuet13
i_roaatuet13 <- t(roaatuet13)
it_roaatuet13 <- t(i_roaatuet13)
roaatecp13 <- (tin13_total_col-tga13_total_col)/(atecp13_total_col-dtaf13_total_col)
roaatecp13 <- roaatecp13
i_roaatecp13 <- t(roaatecp13)
it_roaatecp13 <- t(i_roaatecp13)
roaatma13 <- (tin13_total_col-tga13_total_col)/(atma13_total_col-dtaf13_total_col)
roaatma13 <- roaatma13
i_roaatma13 <- t(roaatma13)
it_roaatma13 <- t(i_roaatma13)

ratvtaf13 <- (tin13_total_col)/(vtaf13_total_col-dtaf13_total_col)
ratvtaf13 <- ratvtaf13
i_ratvtaf13 <- t(ratvtaf13)
it_ratvtaf13 <- t(i_ratvtaf13)
ratafpp13 <- (tin13_total_col)/(afpp13_total_col-dtaf13_total_col)
ratafpp13 <- ratafpp13
i_ratafpp13 <- t(ratafpp13)
it_ratafpp13 <- t(i_ratafpp13)
ratatmep13 <- (tin13_total_col)/(atmep13_total_col-dtaf13_total_col)
ratatmep13 <- ratatmep13
i_ratatmep13 <- t(ratatmep13)
it_ratatmep13 <- t(i_ratatmep13)
ratatbi13 <- (tin13_total_col)/(atbi13_total_col-dtaf13_total_col)
ratatbi13 <- ratatbi13
i_ratatbi13 <- t(ratatbi13)
it_ratatbi13 <- t(i_ratatbi13)
ratatuet13 <- (tin13_total_col)/(atuet13_total_col-dtaf13_total_col)
ratatuet13 <- ratatuet13
i_ratatuet13 <- t(ratatuet13)
it_ratatuet13 <- t(i_ratatuet13)
ratatecp13 <- (tin13_total_col)/(atecp13_total_col-dtaf13_total_col)
ratatecp13 <- ratatecp13
i_ratatecp13 <- t(ratatecp13)
it_ratatecp13 <- t(i_ratatecp13)
ratatma13 <- (tin13_total_col)/(atma13_total_col-dtaf13_total_col)
ratatma13 <- ratatma13
i_ratatma13 <- t(ratatma13)
it_ratatma13 <- t(i_ratatma13)

roicvtaf13 <- (vacb13_total_col)/(vtaf13_total_col-dtaf13_total_col)
roicvtaf13 <- roicvtaf13
i_roicvtaf13 <- t(roicvtaf13)
it_roicvtaf13 <- t(i_roicvtaf13)
roicafpp13 <- (vacb13_total_col)/(afpp13_total_col-dtaf13_total_col)
roicafpp13 <- roicafpp13
i_roicafpp13 <- t(roicafpp13)
it_roicafpp13 <- t(i_roicafpp13)
roicatmep13 <- (vacb13_total_col)/(atmep13_total_col-dtaf13_total_col)
roicatmep13 <- roicatmep13
i_roicatmep13 <- t(roicatmep13)
it_roicatmep13 <- t(i_roicatmep13)
roicatbi13 <- (vacb13_total_col)/(atbi13_total_col-dtaf13_total_col)
roicatbi13 <- roicatbi13
i_roicatbi13 <- t(roicatbi13)
it_roicatbi13 <- t(i_roicatbi13)
roicatuet13 <- (vacb13_total_col)/(atuet13_total_col-dtaf13_total_col)
roicatuet13 <- roicatuet13
i_roicatuet13 <- t(roicatuet13)
it_roicatuet13 <- t(i_roicatuet13)
roicatecp13 <- (vacb13_total_col)/(atecp13_total_col-dtaf13_total_col)
roicatecp13 <- roicatecp13
i_roicatecp13 <- t(roicatecp13)
it_roicatecp13 <- t(i_roicatecp13)
roicatma13 <- (vacb13_total_col)/(atma13_total_col-dtaf13_total_col)
roicatma13 <- roicatma13
i_roicatma13 <- t(roicatma13)
it_roicatma13 <- t(i_roicatma13)

dcavtaf13 <- (dtaf13_total_col)/(vtaf13_total_col)
dcavtaf13 <- dcavtaf13
i_dcavtaf13 <- t(dcavtaf13)
it_dcavtaf13 <- t(i_dcavtaf13)
dcaafpp13 <- (dtaf13_total_col)/(afpp13_total_col)
dcaafpp13 <- dcaafpp13
i_dcaafpp13 <- t(dcaafpp13)
it_dcaafpp13 <- t(i_dcaafpp13)
dcaatmep13 <- (dtaf13_total_col)/(atmep13_total_col)
dcaatmep13 <- dcaatmep13
i_dcaatmep13 <- t(dcaatmep13)
it_dcaatmep13 <- t(i_dcaatmep13)
dcaatbi13 <- (dtaf13_total_col)/(atbi13_total_col)
dcaatbi13 <- dcaatbi13
i_dcaatbi13 <- t(dcaatbi13)
it_dcaatbi13 <- t(i_dcaatbi13)
dcaatuet13 <- (dtaf13_total_col)/(atuet13_total_col)
dcaatuet13 <- dcaatuet13
i_dcaatuet13 <- t(dcaatuet13)
it_dcaatuet13 <- t(i_dcaatuet13)
dcaatecp13 <- (dtaf13_total_col)/(atecp13_total_col)
dcaatecp13 <- dcaatecp13
i_dcaatecp13 <- t(dcaatecp13)
it_dcaatecp13 <- t(i_dcaatecp13)
dcaatma13 <- (dtaf13_total_col)/(atma13_total_col)
dcaatma13 <- dcaatma13
i_dcaatma13 <- t(dcaatma13)
it_dcaatma13 <- t(i_dcaatma13)

iafvtaf13 <- (vtaf13_total_col-dtaf13_total_col)/(tin13_total_col)
iafvtaf13 <- iafvtaf13
i_iafvtaf13 <- t(iafvtaf13)
it_iafvtaf13 <- t(i_iafvtaf13)
iafafpp13 <- (afpp13_total_col-dtaf13_total_col)/(tin13_total_col)
iafafpp13 <- iafafpp13
i_iafafpp13 <- t(iafafpp13)
it_iafafpp13 <- t(i_iafafpp13)
iafatmep13 <- (atmep13_total_col-dtaf13_total_col)/(tin13_total_col)
iafatmep13 <- iafatmep13
i_iafatmep13 <- t(iafatmep13)
it_iafatmep13 <- t(i_iafatmep13)
iafatbi13 <- (atbi13_total_col-dtaf13_total_col)/(tin13_total_col)
iafatbi13 <- iafatbi13
i_iafatbi13 <- t(iafatbi13)
it_iafatbi13 <- t(i_iafatbi13)
iafatuet13 <- (atuet13_total_col-dtaf13_total_col)/(tin13_total_col)
iafatuet13 <- iafatuet13
i_iafatuet13 <- t(iafatuet13)
it_iafatuet13 <- t(i_iafatuet13)
iafatecp13 <- (atecp13_total_col-dtaf13_total_col)/(tin13_total_col)
iafatecp13 <- iafatecp13
i_iafatecp13 <- t(iafatecp13)
it_iafatecp13 <- t(i_iafatecp13)
iafatma13 <- (atma13_total_col-dtaf13_total_col)/(tin13_total_col)
iafatma13 <- iafatma13
i_iafatma13 <- t(iafatma13)
it_iafatma13 <- t(i_iafatma13)

eepacd18 <- (pacd18_norm_col/ue18_norm_col)/(pacd18_total_general/ue18_total_general)
eepacd18 <- eepacd18
rownames(eepacd18) <- pacd18g$AE
colnames(eepacd18) <- colnames(pacd18g[,-1])
i_eepacd18 <- colSums(eepacd18[,], na.rm = TRUE)
i_eepacd18 <- t(i_eepacd18)
it_eepacd18 <- t(i_eepacd18)

eeppvs18 <- (ppvs18_norm_col/ue18_norm_col)/(ppvs18_total_general/ue18_total_general)
eeppvs18 <- eeppvs18
rownames(eeppvs18) <- ppvs18g$AE
colnames(eeppvs18) <- colnames(ppvs18g[,-1])
i_eeppvs18 <- colSums(eeppvs18[,], na.rm = TRUE)
i_eeppvs18 <- t(i_eeppvs18)
it_eeppvs18 <- t(i_eeppvs18)

marppvs18 <- (ppvs18_norm_col/ppvs18i_n)
marppvs18 <- marppvs18
rownames(marppvs18) <- ppvs18g$AE
colnames(marppvs18) <- colnames(ppvs18g[,-1])
i_marppvs18 <- colSums(marppvs18[,], na.rm = TRUE)
i_marppvs18 <- t(i_marppvs18)
it_marppvs18 <- t(i_marppvs18)

marpacd18 <- (pacd18_norm_col/pacd18i_n)
marpacd18 <- marpacd18
rownames(marpacd18) <- pacd18g$AE
colnames(marpacd18) <- colnames(pacd18g[,-1])
i_marpacd18 <- colSums(marpacd18[,], na.rm = TRUE)
i_marpacd18 <- t(i_marpacd18)
it_marpacd18 <- t(i_marpacd18)

comppvs18 <- 1/(ppvs18_total_col/ppvs18_total_general)^2
i_comppvs18 <- t(comppvs18)
it_comppvs18 <- t(i_comppvs18)
compacd18 <- 1/(pacd18_total_col/pacd18_total_general)^2
i_compacd18 <- t(compacd18)
it_compacd18 <- t(i_compacd18)

divppvs18 <- apply(t(ppvs18), 1, function(x) diversity(x, index = "shannon"))
i_divppvs18 <- t(divppvs18)
it_divppvs18 <- t(i_divppvs18)

divpacd18 <- apply(t(pacd18), 1, function(x) diversity(x, index = "shannon"))
i_divpacd18 <- t(divpacd18)
it_divpacd18 <- t(i_divpacd18)

prppvs18 <- (((vacb18_total_col/ppvs18_total_col)/(vacb18_total_general/ppvs18_total_general))+
               ((pbt18_total_col/ppvs18_total_col)/(pbt18_total_general/ppvs18_total_general)))/2

i_prppvs18 <- t(prppvs18)
it_prppvs18 <- t(i_prppvs18)

prpacd18 <- (((vacb18_total_col/pacd18_total_col)/(vacb18_total_general/pacd18_total_general))+
               ((pbt18_total_col/pacd18_total_col)/(pbt18_total_general/pacd18_total_general)))/2

i_prpacd18 <- t(prpacd18)
it_prpacd18 <- t(i_prpacd18)

srppvs18 <- (tsppvs18/ppvs18)/(tsppvs18_total_general/ppvs18_total_general)

srppvs18 <- srppvs18

rownames(srppvs18) <- pacd18g$AE
colnames(srppvs18) <- colnames(pacd18g[,-1])

i_srppvs18 <- colSums(srppvs18[,], na.rm = TRUE)
i_srppvs18 <- t(i_srppvs18)
it_srppvs18 <- t(i_srppvs18)

srpacd18 <- (tspacd18/pacd18)/(tspacd18_total_general/pacd18_total_general)

srpacd18 <- srpacd18

rownames(srpacd18) <- pacd18g$AE
colnames(srpacd18) <- colnames(pacd18g[,-1])

i_srpacd18 <- colSums(srpacd18[,], na.rm = TRUE)
i_srpacd18 <- t(i_srpacd18)
it_srpacd18 <- t(i_srpacd18)

roa18 <- (tin18_total_col-tga18_total_col)/(ataf18_total_col-dtaf18_total_col)

roa18 <- roa18
i_roa18 <- t(roa18)
it_roa18 <- t(i_roa18)

rat18 <- (tin18_total_col)/(ataf18_total_col-dtaf18_total_col)

rat18 <- rat18
i_rat18 <- t(rat18)
it_rat18 <- t(i_rat18)

roic18 <- (vacb18_total_col)/(ataf18_total_col-dtaf18_total_col)

roic18 <- roic18
i_roic18 <- t(roic18)
it_roic18 <- t(i_roic18)

dca18 <- (dtaf18_total_col)/(ataf18_total_col)

dca18 <- dca18
i_dca18 <- t(dca18)
it_dca18 <- t(i_dca18)

iaf18 <- (ataf18_total_col-dtaf18_total_col)/(tin18_total_col)

iaf18 <- iaf18
i_iaf18 <- t(iaf18)
it_iaf18 <- t(i_iaf18)

mbi18 <- (tin18_total_col-tga18_total_col)/(tin18_total_col)

mbi18 <- mbi18
i_mbi18 <- t(mbi18)
it_mbi18 <- t(i_mbi18)

ptf18 <- (vacb18_total_col)/((pot18_total_col/htpot18_total_col)+ (ataf18_total_col-dtaf18_total_col))

ptf18 <- ptf18
i_ptf18 <- t(ptf18)
it_ptf18 <- t(i_ptf18)

roavtaf18 <- (tin18_total_col-tga18_total_col)/(vtaf18_total_col-dtaf18_total_col)
roavtaf18 <- roavtaf18
i_roavtaf18 <- t(roavtaf18)
it_roavtaf18 <- t(i_roavtaf18)
roaafpp18 <- (tin18_total_col-tga18_total_col)/(afpp18_total_col-dtaf18_total_col)
roaafpp18 <- roaafpp18
i_roaafpp18 <- t(roaafpp18)
it_roaafpp18 <- t(i_roaafpp18)
roaatmep18 <- (tin18_total_col-tga18_total_col)/(atmep18_total_col-dtaf18_total_col)
roaatmep18 <- roaatmep18
i_roaatmep18 <- t(roaatmep18)
it_roaatmep18 <- t(i_roaatmep18)
roaatbi18 <- (tin18_total_col-tga18_total_col)/(atbi18_total_col-dtaf18_total_col)
roaatbi18 <- roaatbi18
i_roaatbi18 <- t(roaatbi18)
it_roaatbi18 <- t(i_roaatbi18)
roaatuet18 <- (tin18_total_col-tga18_total_col)/(atuet18_total_col-dtaf18_total_col)
roaatuet18 <- roaatuet18
i_roaatuet18 <- t(roaatuet18)
it_roaatuet18 <- t(i_roaatuet18)
roaatecp18 <- (tin18_total_col-tga18_total_col)/(atecp18_total_col-dtaf18_total_col)
roaatecp18 <- roaatecp18
i_roaatecp18 <- t(roaatecp18)
it_roaatecp18 <- t(i_roaatecp18)
roaatma18 <- (tin18_total_col-tga18_total_col)/(atma18_total_col-dtaf18_total_col)
roaatma18 <- roaatma18
i_roaatma18 <- t(roaatma18)
it_roaatma18 <- t(i_roaatma18)

ratvtaf18 <- (tin18_total_col)/(vtaf18_total_col-dtaf18_total_col)
ratvtaf18 <- ratvtaf18
i_ratvtaf18 <- t(ratvtaf18)
it_ratvtaf18 <- t(i_ratvtaf18)
ratafpp18 <- (tin18_total_col)/(afpp18_total_col-dtaf18_total_col)
ratafpp18 <- ratafpp18
i_ratafpp18 <- t(ratafpp18)
it_ratafpp18 <- t(i_ratafpp18)
ratatmep18 <- (tin18_total_col)/(atmep18_total_col-dtaf18_total_col)
ratatmep18 <- ratatmep18
i_ratatmep18 <- t(ratatmep18)
it_ratatmep18 <- t(i_ratatmep18)
ratatbi18 <- (tin18_total_col)/(atbi18_total_col-dtaf18_total_col)
ratatbi18 <- ratatbi18
i_ratatbi18 <- t(ratatbi18)
it_ratatbi18 <- t(i_ratatbi18)
ratatuet18 <- (tin18_total_col)/(atuet18_total_col-dtaf18_total_col)
ratatuet18 <- ratatuet18
i_ratatuet18 <- t(ratatuet18)
it_ratatuet18 <- t(i_ratatuet18)
ratatecp18 <- (tin18_total_col)/(atecp18_total_col-dtaf18_total_col)
ratatecp18 <- ratatecp18
i_ratatecp18 <- t(ratatecp18)
it_ratatecp18 <- t(i_ratatecp18)
ratatma18 <- (tin18_total_col)/(atma18_total_col-dtaf18_total_col)
ratatma18 <- ratatma18
i_ratatma18 <- t(ratatma18)
it_ratatma18 <- t(i_ratatma18)

roicvtaf18 <- (vacb18_total_col)/(vtaf18_total_col-dtaf18_total_col)
roicvtaf18 <- roicvtaf18
i_roicvtaf18 <- t(roicvtaf18)
it_roicvtaf18 <- t(i_roicvtaf18)
roicafpp18 <- (vacb18_total_col)/(afpp18_total_col-dtaf18_total_col)
roicafpp18 <- roicafpp18
i_roicafpp18 <- t(roicafpp18)
it_roicafpp18 <- t(i_roicafpp18)
roicatmep18 <- (vacb18_total_col)/(atmep18_total_col-dtaf18_total_col)
roicatmep18 <- roicatmep18
i_roicatmep18 <- t(roicatmep18)
it_roicatmep18 <- t(i_roicatmep18)
roicatbi18 <- (vacb18_total_col)/(atbi18_total_col-dtaf18_total_col)
roicatbi18 <- roicatbi18
i_roicatbi18 <- t(roicatbi18)
it_roicatbi18 <- t(i_roicatbi18)
roicatuet18 <- (vacb18_total_col)/(atuet18_total_col-dtaf18_total_col)
roicatuet18 <- roicatuet18
i_roicatuet18 <- t(roicatuet18)
it_roicatuet18 <- t(i_roicatuet18)
roicatecp18 <- (vacb18_total_col)/(atecp18_total_col-dtaf18_total_col)
roicatecp18 <- roicatecp18
i_roicatecp18 <- t(roicatecp18)
it_roicatecp18 <- t(i_roicatecp18)
roicatma18 <- (vacb18_total_col)/(atma18_total_col-dtaf18_total_col)
roicatma18 <- roicatma18
i_roicatma18 <- t(roicatma18)
it_roicatma18 <- t(i_roicatma18)

dcavtaf18 <- (dtaf18_total_col)/(vtaf18_total_col)
dcavtaf18 <- dcavtaf18
i_dcavtaf18 <- t(dcavtaf18)
it_dcavtaf18 <- t(i_dcavtaf18)
dcaafpp18 <- (dtaf18_total_col)/(afpp18_total_col)
dcaafpp18 <- dcaafpp18
i_dcaafpp18 <- t(dcaafpp18)
it_dcaafpp18 <- t(i_dcaafpp18)
dcaatmep18 <- (dtaf18_total_col)/(atmep18_total_col)
dcaatmep18 <- dcaatmep18
i_dcaatmep18 <- t(dcaatmep18)
it_dcaatmep18 <- t(i_dcaatmep18)
dcaatbi18 <- (dtaf18_total_col)/(atbi18_total_col)
dcaatbi18 <- dcaatbi18
i_dcaatbi18 <- t(dcaatbi18)
it_dcaatbi18 <- t(i_dcaatbi18)
dcaatuet18 <- (dtaf18_total_col)/(atuet18_total_col)
dcaatuet18 <- dcaatuet18
i_dcaatuet18 <- t(dcaatuet18)
it_dcaatuet18 <- t(i_dcaatuet18)
dcaatecp18 <- (dtaf18_total_col)/(atecp18_total_col)
dcaatecp18 <- dcaatecp18
i_dcaatecp18 <- t(dcaatecp18)
it_dcaatecp18 <- t(i_dcaatecp18)
dcaatma18 <- (dtaf18_total_col)/(atma18_total_col)
dcaatma18 <- dcaatma18
i_dcaatma18 <- t(dcaatma18)
it_dcaatma18 <- t(i_dcaatma18)

iafvtaf18 <- (vtaf18_total_col-dtaf18_total_col)/(tin18_total_col)
iafvtaf18 <- iafvtaf18
i_iafvtaf18 <- t(iafvtaf18)
it_iafvtaf18 <- t(i_iafvtaf18)
iafafpp18 <- (afpp18_total_col-dtaf18_total_col)/(tin18_total_col)
iafafpp18 <- iafafpp18
i_iafafpp18 <- t(iafafpp18)
it_iafafpp18 <- t(i_iafafpp18)
iafatmep18 <- (atmep18_total_col-dtaf18_total_col)/(tin18_total_col)
iafatmep18 <- iafatmep18
i_iafatmep18 <- t(iafatmep18)
it_iafatmep18 <- t(i_iafatmep18)
iafatbi18 <- (atbi18_total_col-dtaf18_total_col)/(tin18_total_col)
iafatbi18 <- iafatbi18
i_iafatbi18 <- t(iafatbi18)
it_iafatbi18 <- t(i_iafatbi18)
iafatuet18 <- (atuet18_total_col-dtaf18_total_col)/(tin18_total_col)
iafatuet18 <- iafatuet18
i_iafatuet18 <- t(iafatuet18)
it_iafatuet18 <- t(i_iafatuet18)
iafatecp18 <- (atecp18_total_col-dtaf18_total_col)/(tin18_total_col)
iafatecp18 <- iafatecp18
i_iafatecp18 <- t(iafatecp18)
it_iafatecp18 <- t(i_iafatecp18)
iafatma18 <- (atma18_total_col-dtaf18_total_col)/(tin18_total_col)
iafatma18 <- iafatma18
i_iafatma18 <- t(iafatma18)
it_iafatma18 <- t(i_iafatma18)


### Base de datos

ind03 <- data.frame(Vector1 = it_marppvs03, Vector2 = it_marpacd03, 
                    Vector3 = it_comppvs03, Vector4 = it_compacd03,
                    Vector5 = it_divppvs03, Vector6 = it_divpacd03,
                    Vector7 = it_prppvs03, Vector8 = it_prpacd03,
                    Vector9 = it_srppvs03, Vector10 = it_srpacd03,
                    Vector11 = it_roa03, Vector12 = it_rat03,
                    Vector13 = it_roic03, Vector14 = it_dca03,
                    Vector15 = it_iaf03, Vector16 = it_mbi03,
                    Vector17 = it_ptf03,  Vector18 = it_roavtaf03,
                    Vector19 = it_roaafpp03, Vector20 = it_roaatmep03,
                    Vector21 = it_roaatbi03, Vector22 = it_roaatuet03,
                    Vector23 = it_roaatecp03, Vector24 = it_roaatma03,
                    Vector25 = it_ratvtaf03, Vector26 = it_ratafpp03,
                    Vector27 = it_ratatmep03, Vector28 = it_ratatbi03,
                    Vector29 = it_ratatuet03, Vector30 = it_ratatecp03,
                    Vector31 = it_ratatma03, Vector32 = it_roicvtaf03,
                    Vector33 = it_roicafpp03, Vector34 = it_roicatmep03,
                    Vector35 = it_roicatbi03, Vector36 = it_roicatuet03,
                    Vector37 = it_roicatecp03, Vector38 = it_roicatma03,
                    Vector39 = it_dcavtaf03, Vector40 = it_dcaafpp03,
                    Vector41 = it_dcaatmep03,  Vector42 = it_dcaatbi03,
                    Vector43 = it_dcaatuet03,  Vector44 = it_dcaatecp03,
                    Vector45 = it_dcaatma03, Vector46 = it_iafvtaf03,
                    Vector47 = it_iafafpp03, Vector48 = it_iafatmep03,
                    Vector49 = it_iafatbi03, Vector50 = it_iafatuet03,
                    Vector51 = it_iafatecp03, Vector52 = it_iafatma03,
                    Vector53 = it_eeppvs03, Vector54 = it_eepacd03)


colnames(ind03) <- c("marppvs", "marpacd", 
                     "comppvs", "compacd",
                     "divppvs", "divpacd",
                     "prppvs", "prpacd",
                     "srppvs", "srpacd",
                     "roa", "rat",
                     "roic", "dca",
                     "iaf", "mbi",
                     "ptf", "roavtaf", "roaafpp", "roaatmep", "roaatbi",
                     "roaatuet", "roaatecp", "roaatma", "ratvtaf", "ratafpp", "ratatmep", "ratatbi",
                     "ratatuet", "ratatecp", "ratatma", "roicvtaf", "roicafpp", "roicatmep", "roicatbi",
                     "roicatuet", "roicatecp", "roicatma", "dcavtaf", "dcaafpp", "dcaatmep", "dcaatbi",
                     "dcaatuet", "dcaatecp", "dcaatma", "iafvtaf", "iafafpp", "iafatmep", "iafatbi",
                     "iafatuet", "iafatecp", "iafatma", "eeppvs", "eepacd")


ind08 <- data.frame(Vector1 = it_marppvs08, Vector2 = it_marpacd08, 
                    Vector3 = it_comppvs08, Vector4 = it_compacd08,
                    Vector5 = it_divppvs08, Vector6 = it_divpacd08,
                    Vector7 = it_prppvs08, Vector8 = it_prpacd08,
                    Vector9 = it_srppvs08, Vector10 = it_srpacd08,
                    Vector11 = it_roa08, Vector12 = it_rat08,
                    Vector13 = it_roic08, Vector14 = it_dca08,
                    Vector15 = it_iaf08, Vector16 = it_mbi08,
                    Vector17 = it_ptf08,  Vector18 = it_roavtaf08,
                    Vector19 = it_roaafpp08, Vector20 = it_roaatmep08,
                    Vector21 = it_roaatbi08, Vector22 = it_roaatuet08,
                    Vector23 = it_roaatecp08, Vector24 = it_roaatma08,
                    Vector25 = it_ratvtaf08, Vector26 = it_ratafpp08,
                    Vector27 = it_ratatmep08, Vector28 = it_ratatbi08,
                    Vector29 = it_ratatuet08, Vector30 = it_ratatecp08,
                    Vector31 = it_ratatma08, Vector32 = it_roicvtaf08,
                    Vector33 = it_roicafpp08, Vector34 = it_roicatmep08,
                    Vector35 = it_roicatbi08, Vector36 = it_roicatuet08,
                    Vector37 = it_roicatecp08, Vector38 = it_roicatma08,
                    Vector39 = it_dcavtaf08, Vector40 = it_dcaafpp08,
                    Vector41 = it_dcaatmep08,  Vector42 = it_dcaatbi08,
                    Vector43 = it_dcaatuet08,  Vector44 = it_dcaatecp08,
                    Vector45 = it_dcaatma08, Vector46 = it_iafvtaf08,
                    Vector47 = it_iafafpp08, Vector48 = it_iafatmep08,
                    Vector49 = it_iafatbi08, Vector50 = it_iafatuet08,
                    Vector51 = it_iafatecp08, Vector52 = it_iafatma08,
                    Vector53 = it_eeppvs08, Vector54 = it_eepacd08)


colnames(ind08) <- c("marppvs", "marpacd", 
                     "comppvs", "compacd",
                     "divppvs", "divpacd",
                     "prppvs", "prpacd",
                     "srppvs", "srpacd",
                     "roa", "rat",
                     "roic", "dca",
                     "iaf", "mbi",
                     "ptf", "roavtaf", "roaafpp", "roaatmep", "roaatbi",
                     "roaatuet", "roaatecp", "roaatma", "ratvtaf", "ratafpp", "ratatmep", "ratatbi",
                     "ratatuet", "ratatecp", "ratatma", "roicvtaf", "roicafpp", "roicatmep", "roicatbi",
                     "roicatuet", "roicatecp", "roicatma", "dcavtaf", "dcaafpp", "dcaatmep", "dcaatbi",
                     "dcaatuet", "dcaatecp", "dcaatma", "iafvtaf", "iafafpp", "iafatmep", "iafatbi",
                     "iafatuet", "iafatecp", "iafatma", "eeppvs", "eepacd")


ind13 <- data.frame(Vector1 = it_marppvs13, Vector2 = it_marpacd13, 
                    Vector3 = it_comppvs13, Vector4 = it_compacd13,
                    Vector5 = it_divppvs13, Vector6 = it_divpacd13,
                    Vector7 = it_prppvs13, Vector8 = it_prpacd13,
                    Vector9 = it_srppvs13, Vector10 = it_srpacd13,
                    Vector11 = it_roa13, Vector12 = it_rat13,
                    Vector13 = it_roic13, Vector14 = it_dca13,
                    Vector15 = it_iaf13, Vector16 = it_mbi13,
                    Vector17 = it_ptf13,  Vector18 = it_roavtaf13,
                    Vector19 = it_roaafpp13, Vector20 = it_roaatmep13,
                    Vector21 = it_roaatbi13, Vector22 = it_roaatuet13,
                    Vector23 = it_roaatecp13, Vector24 = it_roaatma13,
                    Vector25 = it_ratvtaf13, Vector26 = it_ratafpp13,
                    Vector27 = it_ratatmep13, Vector28 = it_ratatbi13,
                    Vector29 = it_ratatuet13, Vector30 = it_ratatecp13,
                    Vector31 = it_ratatma13, Vector32 = it_roicvtaf13,
                    Vector33 = it_roicafpp13, Vector34 = it_roicatmep13,
                    Vector35 = it_roicatbi13, Vector36 = it_roicatuet13,
                    Vector37 = it_roicatecp13, Vector38 = it_roicatma13,
                    Vector39 = it_dcavtaf13, Vector40 = it_dcaafpp13,
                    Vector41 = it_dcaatmep13,  Vector42 = it_dcaatbi13,
                    Vector43 = it_dcaatuet13,  Vector44 = it_dcaatecp13,
                    Vector45 = it_dcaatma13, Vector46 = it_iafvtaf13,
                    Vector47 = it_iafafpp13, Vector48 = it_iafatmep13,
                    Vector49 = it_iafatbi13, Vector50 = it_iafatuet13,
                    Vector51 = it_iafatecp13, Vector52 = it_iafatma13,
                    Vector53 = it_eeppvs13, Vector54 = it_eepacd13)


colnames(ind13) <- c("marppvs", "marpacd", 
                     "comppvs", "compacd",
                     "divppvs", "divpacd",
                     "prppvs", "prpacd",
                     "srppvs", "srpacd",
                     "roa", "rat",
                     "roic", "dca",
                     "iaf", "mbi",
                     "ptf", "roavtaf", "roaafpp", "roaatmep", "roaatbi",
                     "roaatuet", "roaatecp", "roaatma", "ratvtaf", "ratafpp", "ratatmep", "ratatbi",
                     "ratatuet", "ratatecp", "ratatma", "roicvtaf", "roicafpp", "roicatmep", "roicatbi",
                     "roicatuet", "roicatecp", "roicatma", "dcavtaf", "dcaafpp", "dcaatmep", "dcaatbi",
                     "dcaatuet", "dcaatecp", "dcaatma", "iafvtaf", "iafafpp", "iafatmep", "iafatbi",
                     "iafatuet", "iafatecp", "iafatma", "eeppvs", "eepacd")


ind18 <- data.frame(Vector1 = it_marppvs18, Vector2 = it_marpacd18, 
                    Vector3 = it_comppvs18, Vector4 = it_compacd18,
                    Vector5 = it_divppvs18, Vector6 = it_divpacd18,
                    Vector7 = it_prppvs18, Vector8 = it_prpacd18,
                    Vector9 = it_srppvs18, Vector10 = it_srpacd18,
                    Vector11 = it_roa18, Vector12 = it_rat18,
                    Vector18 = it_roic18, Vector14 = it_dca18,
                    Vector15 = it_iaf18, Vector16 = it_mbi18,
                    Vector17 = it_ptf18,  Vector18 = it_roavtaf18,
                    Vector19 = it_roaafpp18, Vector20 = it_roaatmep18,
                    Vector21 = it_roaatbi18, Vector22 = it_roaatuet18,
                    Vector23 = it_roaatecp18, Vector24 = it_roaatma18,
                    Vector25 = it_ratvtaf18, Vector26 = it_ratafpp18,
                    Vector27 = it_ratatmep18, Vector28 = it_ratatbi18,
                    Vector29 = it_ratatuet18, Vector30 = it_ratatecp18,
                    Vector31 = it_ratatma18, Vector32 = it_roicvtaf18,
                    Vector33 = it_roicafpp18, Vector34 = it_roicatmep18,
                    Vector35 = it_roicatbi18, Vector36 = it_roicatuet18,
                    Vector37 = it_roicatecp18, Vector38 = it_roicatma18,
                    Vector39 = it_dcavtaf18, Vector40 = it_dcaafpp18,
                    Vector41 = it_dcaatmep18,  Vector42 = it_dcaatbi18,
                    Vector43 = it_dcaatuet18,  Vector44 = it_dcaatecp18,
                    Vector45 = it_dcaatma18, Vector46 = it_iafvtaf18,
                    Vector47 = it_iafafpp18, Vector48 = it_iafatmep18,
                    Vector49 = it_iafatbi18, Vector50 = it_iafatuet18,
                    Vector51 = it_iafatecp18, Vector52 = it_iafatma18,
                    Vector53 = it_eeppvs18, Vector54 = it_eepacd18)


colnames(ind18) <- c("marppvs", "marpacd", 
                     "comppvs", "compacd",
                     "divppvs", "divpacd",
                     "prppvs", "prpacd",
                     "srppvs", "srpacd",
                     "roa", "rat",
                     "roic", "dca",
                     "iaf", "mbi",
                     "ptf", "roavtaf", "roaafpp", "roaatmep", "roaatbi",
                     "roaatuet", "roaatecp", "roaatma", "ratvtaf", "ratafpp", "ratatmep", "ratatbi",
                     "ratatuet", "ratatecp", "ratatma", "roicvtaf", "roicafpp", "roicatmep", "roicatbi",
                     "roicatuet", "roicatecp", "roicatma", "dcavtaf", "dcaafpp", "dcaatmep", "dcaatbi",
                     "dcaatuet", "dcaatecp", "dcaatma", "iafvtaf", "iafafpp", "iafatmep", "iafatbi",
                     "iafatuet", "iafatecp", "iafatma", "eeppvs", "eepacd")


indgen <- rbind(ind03, ind08, ind13, ind18)

col_con <- names(indgen)[sapply(indgen, is.numeric)]

print(col_con)

data.s <- bin_01

indgen <- cbind(tcode = data.s$tcode, indgen)
indgen$NOMGEO <- data.s$NOMGEO
indgen$CVE_GEO <- data.s$CVEGEO
indgen$CVE_ENT <- data.s$CVE_ENT

# Se crean dummies para cada año
indgen <- indgen %>%
  fastDummies::dummy_cols("tcode")

# Se genera la base de datos en excel

write_xlsx(indgen, "baseind.xlsx")

########### Análisis espacial

### Constructing the spatial weights matrix

d01 <- coordenadas 
coord_sf1 <- cbind(d01$long, d01$lat)
vecinos <- knearneigh(coord_sf1, k = 4)
nb_vecinos <- knn2nb(vecinos)
pesos <- nb2listw(knn2nb(vecinos))
pesose <- nb2listw(nb_vecinos, style = "W")
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

joined_df_cuali <- left_join(data.s, mx, by = c("NOMGEO" = "NOMGEO", "CVEGEO"="CVEGEO"))

cuali <- st_sf(joined_df_cuali)
cua03 <- cuali %>% filter(tcode == "2003")
cua08 <- cuali %>% filter(tcode == "2008")
cua13 <- cuali %>% filter(tcode == "2013")
cua18 <- cuali %>% filter(tcode == "2018")

esp03 <- ggplot(data = cua03) +  
  geom_sf(aes(fill = esp)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2003")

esp03

esp08 <- ggplot(data = cua08) +  
  geom_sf(aes(fill = esp)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2008")

esp08

esp13 <- ggplot(data = cua13) +  
  geom_sf(aes(fill = esp)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2013")

esp13

esp18 <- ggplot(data = cua18) +  
  geom_sf(aes(fill = esp)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2018")

esp18

esp <- plot_grid(esp03, esp08, esp13, esp18,  ncol = 2,
                  align = "h", rel_widths = c(1, 1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

esp_titled <- plot_grid(
  ggdraw() + draw_label("Preeminencia Sectorial Especialización", fontface = 'bold', x = 0.5, hjust = 0.5),
  esp,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(esp_titled)



div03 <- ggplot(data = cua03) +  
  geom_sf(aes(fill = div)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2003")

div03

div08 <- ggplot(data = cua08) +  
  geom_sf(aes(fill = div)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2008")

div08

div13 <- ggplot(data = cua13) +  
  geom_sf(aes(fill = div)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2013")

div13

div18 <- ggplot(data = cua18) +  
  geom_sf(aes(fill = div)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2018")

div18

div <- plot_grid(div03, div08, div13, div18,  ncol = 2,
                 align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

div_titled <- plot_grid(
  ggdraw() + draw_label("Preeminencia Sectorial Diversidad", fontface = 'bold', x = 0.5, hjust = 0.5),
  div,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(div_titled)


com03 <- ggplot(data = cua03) +  
  geom_sf(aes(fill = com)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2003")

com03

com08 <- ggplot(data = cua08) +  
  geom_sf(aes(fill = com)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2008")

com08

com13 <- ggplot(data = cua13) +  
  geom_sf(aes(fill = com)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2013")

com13

com18 <- ggplot(data = cua18) +  
  geom_sf(aes(fill = com)) +  
  scale_fill_viridis_d(direction = 1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2018")

com18

com <- plot_grid(com03, com08, com13, com18,  ncol = 2,
                 align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

com_titled <- plot_grid(
  ggdraw() + draw_label("Preeminencia Sectorial Competencia", fontface = 'bold', x = 0.5, hjust = 0.5),
  com,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(com_titled)

cre08 <- ggplot(data = cua08) +  
  geom_sf(aes(fill = crec)) +  
  scale_fill_viridis_d(direction = -1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2008 vs 2003")

cre08

cre13 <- ggplot(data = cua13) +  
  geom_sf(aes(fill = crec)) +  
  scale_fill_viridis_d(direction = -1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2013 vs 2008")

cre13

cre18 <- ggplot(data = cua18) +  
  geom_sf(aes(fill = crec)) +  
  scale_fill_viridis_d(direction = -1) +  
  theme_minimal() +  
  guides(fill = "none") +  
  labs(title = "2018 vs 2013")

cre18

cre <- plot_grid(cre08, cre13, cre18,  nrow = 1,
                 align = "h", rel_widths = c(1, 1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

cre_titled <- plot_grid(
  ggdraw() + draw_label("Preeminencia Sectorial Crecimiento de la Productividad Relativa", fontface = 'bold', x = 0.5, hjust = 0.5),
  cre,
  ncol = 1,
  rel_heights = c(0.1, 1))

print(cre_titled)



########## Análisis descriptivo



######## Spatial panel data model specification

ecu <- ptf ~ ptfr+marppvs+marpacd+marppvsr+marpacdr+divppvsr+divpacdr+
  divppvs+divpacd+comppvsr+compacdr+comppvs+compacd+prppvs+prpacd+prppvsr+prpacdr+
  roar+roicr+iafr+ratr+dcar+mbir+roa+roic+iaf+rat+dca+mbi+
  tmppvs+tmpacd+tmppvsr+tmpacdr+edppvs+edpacd+eeppvs+eepacd+
  eippvs+eipacd
alias(lm(ecu, data = dpsf01))

vif(lm( ptf ~ ptfr+marppvs+marpacd+marppvsr+marpacdr+divppvsr+divpacdr+
          divppvs+divpacd+comppvsr+compacdr+comppvs+compacd+
          roar+roicr+iafr+ratr+dcar+mbir+roa+roic+iaf+rat+dca+mbi+
          tmppvs+tmpacd+tmppvsr+tmpacdr+edppvs+edpacd+eeppvs+eepacd+
          eippvs+eipacd, data = dpsf01))


# workout model

ecu1 <- ptf ~ ptfr+ marppvs+compacd+divppvs+prppvs+
              pspl(marpacdr, nknots = 20) + pspl(comppvs, nknots = 20) + 
              pspl(roic, nknots = 20) + pspl(prpacdr, nknots = 20) 


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
mod3 <- pspatfit(ecu1, data = dpsf001, 
                eff_demean = "twoways", na.action = T)
summary(mod3)


ecu2 <- ptf ~ ptfr+ marppvs+compacd+divppvs+prppvs+
  pspl(marpacdr, nknots = 20) + pspl(comppvs, nknots = 20) + 
  pspl(roic, nknots = 20) + pspl(prpacdr, nknots = 20) +
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

ecu3 <- ptf ~ ptfr+ marppvs+compacd+divppvs+prppvs+
  pspl(marpacdr, nknots = 20) + pspl(comppvs, nknots = 20) + 
  pspl(roic, nknots = 20) + pspl(prpacdr, nknots = 20) +
  pspt(lat, long, tcode, nknots = c(10, 10,10), psanova = T)


### Models with psanova 3d spatial trend
mod6 <- pspatfit(ecu3, data = dpsf01, eff_demean = "twoways", 
                             na.action = T, type = "sar", 
                             listw = pesos)
summary(mod6)

## Comparison between models
anova(mod1, mod2, mod3, mod4, mod5, mod6,lrtest = FALSE)

### plot spatial trend for spatial point coordinate
plot_sp3d(mod6, data = dpsf01, 
          time_var = "tcode", time_index = c(2003, 2008, 2013, 2018),
          addmain = FALSE, addint = FALSE)

str(mod2)

### Non-Parametric Total, Direct and Indirect impacts
nparimpacts <- impactsnopar(mod2, listw = pesos, viewplot = FALSE)
plot_impactsnopar(nparimpacts, data = ames_sf1, smooth = TRUE)

### Parametric Total, Direct and Indirect impacts
impactspar(mod2, listw = W)

fitted(mod2)

residuals(mod2)

vcov(mod2, bayesian = T)

print(mod2)



