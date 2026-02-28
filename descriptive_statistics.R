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



data_ds <- bin

data_ds[is.na(data_ds)] <- 0

summary_table_ptf <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(ptfr),
            Desviación_Estándar = sd(ptfr),
            Minímo = min(ptfr),
            Q1 = quantile(ptfr, 0.25),
            Mediana = median(ptfr),
            Q3 = quantile(ptfr, 0.75),
            Máximo = max(ptfr),
            RIQ = IQR(ptfr), n = n())

summary_total_ptf <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(ptfr),
            Desviación_Estándar = sd(ptfr),
            Minímo = min(ptfr),
            Q1 = quantile(ptfr, 0.25),
            Mediana = median(ptfr),
            Q3 = quantile(ptfr, 0.75),
            Máximo = max(ptfr),
            RIQ = IQR(ptfr), 
            n = n())

summary_table_ptf <- bind_rows(summary_table_ptf, summary_total_ptf)

summary_table_ptf <- as_tibble(summary_table_ptf)

formatted_table_ptf <- summary_table_ptf %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ptf)

summary_table_ptf_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(ptfr),
            Desviación_Estándar = sd(ptfr),
            Minímo = min(ptfr),
            Q1 = quantile(ptfr, 0.25),
            Mediana = median(ptfr),
            Q3 = quantile(ptfr, 0.75),
            Máximo = max(ptfr),
            RIQ = IQR(ptfr), n = n())

summary_total_ptfae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(ptfr),
            Desviación_Estándar = sd(ptfr),
            Minímo = min(ptfr),
            Q1 = quantile(ptfr, 0.25),
            Mediana = median(ptfr),
            Q3 = quantile(ptfr, 0.75),
            Máximo = max(ptfr),
            RIQ = IQR(ptfr), 
            n = n())

summary_table_ptf_ae <- bind_rows(summary_table_ptf_ae, summary_total_ptfae)

summary_table_ptf_ae <- as_tibble(summary_table_ptf_ae)

formatted_table_ptf_ae <- summary_table_ptf_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ptf_ae)

summary_table_prpacd <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(prpacd),
            Desviación_Estándar = sd(prpacd),
            Minímo = min(prpacd),
            Q1 = quantile(prpacd, 0.25),
            Mediana = median(prpacd),
            Q3 = quantile(prpacd, 0.75),
            Máximo = max(prpacd),
            RIQ = IQR(prpacd), n = n())

summary_total_prpacd <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(prpacd),
            Desviación_Estándar = sd(prpacd),
            Minímo = min(prpacd),
            Q1 = quantile(prpacd, 0.25),
            Mediana = median(prpacd),
            Q3 = quantile(prpacd, 0.75),
            Máximo = max(prpacd),
            RIQ = IQR(prpacd), 
            n = n())

summary_table_prpacd <- bind_rows(summary_table_prpacd, summary_total_prpacd)

summary_table_prpacd <- as_tibble(summary_table_prpacd)

formatted_table_prpacd <- summary_table_prpacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prpacd)

summary_table_prpacd_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(prpacd),
            Desviación_Estándar = sd(prpacd),
            Minímo = min(prpacd),
            Q1 = quantile(prpacd, 0.25),
            Mediana = median(prpacd),
            Q3 = quantile(prpacd, 0.75),
            Máximo = max(prpacd),
            RIQ = IQR(prpacd), n = n())

summary_total_prpacdae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(prpacd),
            Desviación_Estándar = sd(prpacd),
            Minímo = min(prpacd),
            Q1 = quantile(prpacd, 0.25),
            Mediana = median(prpacd),
            Q3 = quantile(prpacd, 0.75),
            Máximo = max(prpacd),
            RIQ = IQR(prpacd), 
            n = n())

summary_table_prpacd_ae <- bind_rows(summary_table_prpacd_ae, summary_total_prpacdae)

summary_table_prpacd_ae <- as_tibble(summary_table_prpacd_ae)

formatted_table_prpacd_ae <- summary_table_prpacd_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prpacd_ae)

summary_table_prpacdr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(prpacdr),
            Desviación_Estándar = sd(prpacdr),
            Minímo = min(prpacdr),
            Q1 = quantile(prpacdr, 0.25),
            Mediana = median(prpacdr),
            Q3 = quantile(prpacdr, 0.75),
            Máximo = max(prpacdr),
            RIQ = IQR(prpacdr), n = n())

summary_total_prpacdr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(prpacdr),
            Desviación_Estándar = sd(prpacdr),
            Minímo = min(prpacdr),
            Q1 = quantile(prpacdr, 0.25),
            Mediana = median(prpacdr),
            Q3 = quantile(prpacdr, 0.75),
            Máximo = max(prpacdr),
            RIQ = IQR(prpacdr), 
            n = n())

summary_table_prpacdr <- bind_rows(summary_table_prpacdr, summary_total_prpacdr)

summary_table_prpacdr <- as_tibble(summary_table_prpacdr)

formatted_table_prpacdr <- summary_table_prpacdr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prpacdr)

summary_table_prpacdr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(prpacdr),
            Desviación_Estándar = sd(prpacdr),
            Minímo = min(prpacdr),
            Q1 = quantile(prpacdr, 0.25),
            Mediana = median(prpacdr),
            Q3 = quantile(prpacdr, 0.75),
            Máximo = max(prpacdr),
            RIQ = IQR(prpacdr), n = n())

summary_total_prpacdrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(prpacdr),
            Desviación_Estándar = sd(prpacdr),
            Minímo = min(prpacdr),
            Q1 = quantile(prpacdr, 0.25),
            Mediana = median(prpacdr),
            Q3 = quantile(prpacdr, 0.75),
            Máximo = max(prpacdr),
            RIQ = IQR(prpacdr), 
            n = n())

summary_table_prpacdr_ae <- bind_rows(summary_table_prpacdr_ae, summary_total_prpacdrae)

summary_table_prpacdr_ae <- as_tibble(summary_table_prpacdr_ae)

formatted_table_prpacdr_ae <- summary_table_prpacdr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prpacdr_ae)

summary_table_prppvs <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(prppvs),
            Desviación_Estándar = sd(prppvs),
            Minímo = min(prppvs),
            Q1 = quantile(prppvs, 0.25),
            Mediana = median(prppvs),
            Q3 = quantile(prppvs, 0.75),
            Máximo = max(prppvs),
            RIQ = IQR(prppvs), n = n())

summary_total_prppvs <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(prppvs),
            Desviación_Estándar = sd(prppvs),
            Minímo = min(prppvs),
            Q1 = quantile(prppvs, 0.25),
            Mediana = median(prppvs),
            Q3 = quantile(prppvs, 0.75),
            Máximo = max(prppvs),
            RIQ = IQR(prppvs), 
            n = n())

summary_table_prppvs <- bind_rows(summary_table_prppvs, summary_total_prppvs)

summary_table_prppvs <- as_tibble(summary_table_prppvs)

formatted_table_prppvs <- summary_table_prppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prppvs)

summary_table_prppvs_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(prppvs),
            Desviación_Estándar = sd(prppvs),
            Minímo = min(prppvs),
            Q1 = quantile(prppvs, 0.25),
            Mediana = median(prppvs),
            Q3 = quantile(prppvs, 0.75),
            Máximo = max(prppvs),
            RIQ = IQR(prppvs), n = n())

summary_total_prppvsae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(prppvs),
            Desviación_Estándar = sd(prppvs),
            Minímo = min(prppvs),
            Q1 = quantile(prppvs, 0.25),
            Mediana = median(prppvs),
            Q3 = quantile(prppvs, 0.75),
            Máximo = max(prppvs),
            RIQ = IQR(prppvs), 
            n = n())

summary_table_prppvs_ae <- bind_rows(summary_table_prppvs_ae, summary_total_prppvsae)

summary_table_prppvs_ae <- as_tibble(summary_table_prppvs_ae)

formatted_table_prppvs_ae <- summary_table_prppvs_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prppvs_ae)

summary_table_prppvsr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(prppvsr),
            Desviación_Estándar = sd(prppvsr),
            Minímo = min(prppvsr),
            Q1 = quantile(prppvsr, 0.25),
            Mediana = median(prppvsr),
            Q3 = quantile(prppvsr, 0.75),
            Máximo = max(prppvsr),
            RIQ = IQR(prppvsr), n = n())

summary_total_prppvsr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(prppvsr),
            Desviación_Estándar = sd(prppvsr),
            Minímo = min(prppvsr),
            Q1 = quantile(prppvsr, 0.25),
            Mediana = median(prppvsr),
            Q3 = quantile(prppvsr, 0.75),
            Máximo = max(prppvsr),
            RIQ = IQR(prppvsr), 
            n = n())

summary_table_prppvsr <- bind_rows(summary_table_prppvsr, summary_total_prppvsr)

summary_table_prppvsr <- as_tibble(summary_table_prppvsr)

formatted_table_prppvsr <- summary_table_prppvsr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prppvsr)

summary_table_prppvsr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(prppvsr),
            Desviación_Estándar = sd(prppvsr),
            Minímo = min(prppvsr),
            Q1 = quantile(prppvsr, 0.25),
            Mediana = median(prppvsr),
            Q3 = quantile(prppvsr, 0.75),
            Máximo = max(prppvsr),
            RIQ = IQR(prppvsr), n = n())

summary_total_prppvsrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(prppvsr),
            Desviación_Estándar = sd(prppvsr),
            Minímo = min(prppvsr),
            Q1 = quantile(prppvsr, 0.25),
            Mediana = median(prppvsr),
            Q3 = quantile(prppvsr, 0.75),
            Máximo = max(prppvsr),
            RIQ = IQR(prppvsr), 
            n = n())

summary_table_prppvsr_ae <- bind_rows(summary_table_prppvsr_ae, summary_total_prppvsrae)

summary_table_prppvsr_ae <- as_tibble(summary_table_prppvsr_ae)

formatted_table_prppvsr_ae <- summary_table_prppvsr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_prppvsr_ae)


summary_table_marpacd <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(marpacd),
            Desviación_Estándar = sd(marpacd),
            Minímo = min(marpacd),
            Q1 = quantile(marpacd, 0.25),
            Mediana = median(marpacd),
            Q3 = quantile(marpacd, 0.75),
            Máximo = max(marpacd),
            RIQ = IQR(marpacd), n = n())

summary_total_marpacd <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(marpacd),
            Desviación_Estándar = sd(marpacd),
            Minímo = min(marpacd),
            Q1 = quantile(marpacd, 0.25),
            Mediana = median(marpacd),
            Q3 = quantile(marpacd, 0.75),
            Máximo = max(marpacd),
            RIQ = IQR(marpacd), 
            n = n())

summary_table_marpacd <- bind_rows(summary_table_marpacd, summary_total_marpacd)

summary_table_marpacd <- as_tibble(summary_table_marpacd)

formatted_table_marpacd <- summary_table_marpacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marpacd)

summary_table_marpacd_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(marpacd),
            Desviación_Estándar = sd(marpacd),
            Minímo = min(marpacd),
            Q1 = quantile(marpacd, 0.25),
            Mediana = median(marpacd),
            Q3 = quantile(marpacd, 0.75),
            Máximo = max(marpacd),
            RIQ = IQR(marpacd), n = n())

summary_total_marpacdae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(marpacd),
            Desviación_Estándar = sd(marpacd),
            Minímo = min(marpacd),
            Q1 = quantile(marpacd, 0.25),
            Mediana = median(marpacd),
            Q3 = quantile(marpacd, 0.75),
            Máximo = max(marpacd),
            RIQ = IQR(marpacd), 
            n = n())

summary_table_marpacd_ae <- bind_rows(summary_table_marpacd_ae, summary_total_marpacdae)

summary_table_marpacd_ae <- as_tibble(summary_table_marpacd_ae)

formatted_table_marpacd_ae <- summary_table_marpacd_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marpacd_ae)

summary_table_marpacdr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(marpacdr),
            Desviación_Estándar = sd(marpacdr),
            Minímo = min(marpacdr),
            Q1 = quantile(marpacdr, 0.25),
            Mediana = median(marpacdr),
            Q3 = quantile(marpacdr, 0.75),
            Máximo = max(marpacdr),
            RIQ = IQR(marpacdr), n = n())

summary_total_marpacdr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(marpacdr),
            Desviación_Estándar = sd(marpacdr),
            Minímo = min(marpacdr),
            Q1 = quantile(marpacdr, 0.25),
            Mediana = median(marpacdr),
            Q3 = quantile(marpacdr, 0.75),
            Máximo = max(marpacdr),
            RIQ = IQR(marpacdr), 
            n = n())

summary_table_marpacdr <- bind_rows(summary_table_marpacdr, summary_total_marpacdr)

summary_table_marpacdr <- as_tibble(summary_table_marpacdr)

formatted_table_marpacdr <- summary_table_marpacdr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marpacdr)

summary_table_marpacdr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(marpacdr),
            Desviación_Estándar = sd(marpacdr),
            Minímo = min(marpacdr),
            Q1 = quantile(marpacdr, 0.25),
            Mediana = median(marpacdr),
            Q3 = quantile(marpacdr, 0.75),
            Máximo = max(marpacdr),
            RIQ = IQR(marpacdr), n = n())

summary_total_marpacdrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(marpacdr),
            Desviación_Estándar = sd(marpacdr),
            Minímo = min(marpacdr),
            Q1 = quantile(marpacdr, 0.25),
            Mediana = median(marpacdr),
            Q3 = quantile(marpacdr, 0.75),
            Máximo = max(marpacdr),
            RIQ = IQR(marpacdr), 
            n = n())

summary_table_marpacdr_ae <- bind_rows(summary_table_marpacdr_ae, summary_total_marpacdrae)

summary_table_marpacdr_ae <- as_tibble(summary_table_marpacdr_ae)

formatted_table_marpacdr_ae <- summary_table_marpacdr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marpacdr_ae)

summary_table_marppvs <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(marppvs),
            Desviación_Estándar = sd(marppvs),
            Minímo = min(marppvs),
            Q1 = quantile(marppvs, 0.25),
            Mediana = median(marppvs),
            Q3 = quantile(marppvs, 0.75),
            Máximo = max(marppvs),
            RIQ = IQR(marppvs), n = n())

summary_total_marppvs <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(marppvs),
            Desviación_Estándar = sd(marppvs),
            Minímo = min(marppvs),
            Q1 = quantile(marppvs, 0.25),
            Mediana = median(marppvs),
            Q3 = quantile(marppvs, 0.75),
            Máximo = max(marppvs),
            RIQ = IQR(marppvs), 
            n = n())

summary_table_marppvs <- bind_rows(summary_table_marppvs, summary_total_marppvs)

summary_table_marppvs <- as_tibble(summary_table_marppvs)

formatted_table_marppvs <- summary_table_marppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marppvs)

summary_table_marppvs_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(marppvs),
            Desviación_Estándar = sd(marppvs),
            Minímo = min(marppvs),
            Q1 = quantile(marppvs, 0.25),
            Mediana = median(marppvs),
            Q3 = quantile(marppvs, 0.75),
            Máximo = max(marppvs),
            RIQ = IQR(marppvs), n = n())

summary_total_marppvsae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(marppvs),
            Desviación_Estándar = sd(marppvs),
            Minímo = min(marppvs),
            Q1 = quantile(marppvs, 0.25),
            Mediana = median(marppvs),
            Q3 = quantile(marppvs, 0.75),
            Máximo = max(marppvs),
            RIQ = IQR(marppvs), 
            n = n())

summary_table_marppvs_ae <- bind_rows(summary_table_marppvs_ae, summary_total_marppvsae)

summary_table_marppvs_ae <- as_tibble(summary_table_marppvs_ae)

formatted_table_marppvs_ae <- summary_table_marppvs_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marppvs_ae)

summary_table_marppvsr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(marppvsr),
            Desviación_Estándar = sd(marppvsr),
            Minímo = min(marppvsr),
            Q1 = quantile(marppvsr, 0.25),
            Mediana = median(marppvsr),
            Q3 = quantile(marppvsr, 0.75),
            Máximo = max(marppvsr),
            RIQ = IQR(marppvsr), n = n())

summary_total_marppvsr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(marppvsr),
            Desviación_Estándar = sd(marppvsr),
            Minímo = min(marppvsr),
            Q1 = quantile(marppvsr, 0.25),
            Mediana = median(marppvsr),
            Q3 = quantile(marppvsr, 0.75),
            Máximo = max(marppvsr),
            RIQ = IQR(marppvsr), 
            n = n())

summary_table_marppvsr <- bind_rows(summary_table_marppvsr, summary_total_marppvsr)

summary_table_marppvsr <- as_tibble(summary_table_marppvsr)

formatted_table_marppvsr <- summary_table_marppvsr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marppvsr)

summary_table_marppvsr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(marppvsr),
            Desviación_Estándar = sd(marppvsr),
            Minímo = min(marppvsr),
            Q1 = quantile(marppvsr, 0.25),
            Mediana = median(marppvsr),
            Q3 = quantile(marppvsr, 0.75),
            Máximo = max(marppvsr),
            RIQ = IQR(marppvsr), n = n())

summary_total_marppvsrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(marppvsr),
            Desviación_Estándar = sd(marppvsr),
            Minímo = min(marppvsr),
            Q1 = quantile(marppvsr, 0.25),
            Mediana = median(marppvsr),
            Q3 = quantile(marppvsr, 0.75),
            Máximo = max(marppvsr),
            RIQ = IQR(marppvsr), 
            n = n())

summary_table_marppvsr_ae <- bind_rows(summary_table_marppvsr_ae, summary_total_marppvsrae)

summary_table_marppvsr_ae <- as_tibble(summary_table_marppvsr_ae)

formatted_table_marppvsr_ae <- summary_table_marppvsr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_marppvsr_ae)

summary_table_divpacd <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(divpacd),
            Desviación_Estándar = sd(divpacd),
            Minímo = min(divpacd),
            Q1 = quantile(divpacd, 0.25),
            Mediana = median(divpacd),
            Q3 = quantile(divpacd, 0.75),
            Máximo = max(divpacd),
            RIQ = IQR(divpacd), n = n())

summary_total_divpacd <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(divpacd),
            Desviación_Estándar = sd(divpacd),
            Minímo = min(divpacd),
            Q1 = quantile(divpacd, 0.25),
            Mediana = median(divpacd),
            Q3 = quantile(divpacd, 0.75),
            Máximo = max(divpacd),
            RIQ = IQR(divpacd), 
            n = n())

summary_table_divpacd <- bind_rows(summary_table_divpacd, summary_total_divpacd)

summary_table_divpacd <- as_tibble(summary_table_divpacd)

formatted_table_divpacd <- summary_table_divpacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divpacd)

summary_table_divpacd_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(divpacd),
            Desviación_Estándar = sd(divpacd),
            Minímo = min(divpacd),
            Q1 = quantile(divpacd, 0.25),
            Mediana = median(divpacd),
            Q3 = quantile(divpacd, 0.75),
            Máximo = max(divpacd),
            RIQ = IQR(divpacd), n = n())

summary_total_divpacdae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(divpacd),
            Desviación_Estándar = sd(divpacd),
            Minímo = min(divpacd),
            Q1 = quantile(divpacd, 0.25),
            Mediana = median(divpacd),
            Q3 = quantile(divpacd, 0.75),
            Máximo = max(divpacd),
            RIQ = IQR(divpacd), 
            n = n())

summary_table_divpacd_ae <- bind_rows(summary_table_divpacd_ae, summary_total_divpacdae)

summary_table_divpacd_ae <- as_tibble(summary_table_divpacd_ae)

formatted_table_divpacd_ae <- summary_table_divpacd_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divpacd_ae)

summary_table_divpacdr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(divpacdr),
            Desviación_Estándar = sd(divpacdr),
            Minímo = min(divpacdr),
            Q1 = quantile(divpacdr, 0.25),
            Mediana = median(divpacdr),
            Q3 = quantile(divpacdr, 0.75),
            Máximo = max(divpacdr),
            RIQ = IQR(divpacdr), n = n())

summary_total_divpacdr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(divpacdr),
            Desviación_Estándar = sd(divpacdr),
            Minímo = min(divpacdr),
            Q1 = quantile(divpacdr, 0.25),
            Mediana = median(divpacdr),
            Q3 = quantile(divpacdr, 0.75),
            Máximo = max(divpacdr),
            RIQ = IQR(divpacdr), 
            n = n())

summary_table_divpacdr <- bind_rows(summary_table_divpacdr, summary_total_divpacdr)

summary_table_divpacdr <- as_tibble(summary_table_divpacdr)

formatted_table_divpacdr <- summary_table_divpacdr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divpacdr)

summary_table_divpacdr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(divpacdr),
            Desviación_Estándar = sd(divpacdr),
            Minímo = min(divpacdr),
            Q1 = quantile(divpacdr, 0.25),
            Mediana = median(divpacdr),
            Q3 = quantile(divpacdr, 0.75),
            Máximo = max(divpacdr),
            RIQ = IQR(divpacdr), n = n())

summary_total_divpacdrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(divpacdr),
            Desviación_Estándar = sd(divpacdr),
            Minímo = min(divpacdr),
            Q1 = quantile(divpacdr, 0.25),
            Mediana = median(divpacdr),
            Q3 = quantile(divpacdr, 0.75),
            Máximo = max(divpacdr),
            RIQ = IQR(divpacdr), 
            n = n())

summary_table_divpacdr_ae <- bind_rows(summary_table_divpacdr_ae, summary_total_divpacdrae)

summary_table_divpacdr_ae <- as_tibble(summary_table_divpacdr_ae)

formatted_table_divpacdr_ae <- summary_table_divpacdr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divpacdr_ae)


summary_table_divppvs <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(divppvs),
            Desviación_Estándar = sd(divppvs),
            Minímo = min(divppvs),
            Q1 = quantile(divppvs, 0.25),
            Mediana = median(divppvs),
            Q3 = quantile(divppvs, 0.75),
            Máximo = max(divppvs),
            RIQ = IQR(divppvs), n = n())

summary_total_divppvs <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(divppvs),
            Desviación_Estándar = sd(divppvs),
            Minímo = min(divppvs),
            Q1 = quantile(divppvs, 0.25),
            Mediana = median(divppvs),
            Q3 = quantile(divppvs, 0.75),
            Máximo = max(divppvs),
            RIQ = IQR(divppvs), 
            n = n())

summary_table_divppvs <- bind_rows(summary_table_divppvs, summary_total_divppvs)

summary_table_divppvs <- as_tibble(summary_table_divppvs)

formatted_table_divppvs <- summary_table_divppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divppvs)

summary_table_divppvs_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(divppvs),
            Desviación_Estándar = sd(divppvs),
            Minímo = min(divppvs),
            Q1 = quantile(divppvs, 0.25),
            Mediana = median(divppvs),
            Q3 = quantile(divppvs, 0.75),
            Máximo = max(divppvs),
            RIQ = IQR(divppvs), n = n())

summary_total_divppvsae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(divppvs),
            Desviación_Estándar = sd(divppvs),
            Minímo = min(divppvs),
            Q1 = quantile(divppvs, 0.25),
            Mediana = median(divppvs),
            Q3 = quantile(divppvs, 0.75),
            Máximo = max(divppvs),
            RIQ = IQR(divppvs), 
            n = n())

summary_table_divppvs_ae <- bind_rows(summary_table_divppvs_ae, summary_total_divppvsae)

summary_table_divppvs_ae <- as_tibble(summary_table_divppvs_ae)

formatted_table_divppvs_ae <- summary_table_divppvs_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divppvs_ae)

summary_table_divppvsr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(divppvsr),
            Desviación_Estándar = sd(divppvsr),
            Minímo = min(divppvsr),
            Q1 = quantile(divppvsr, 0.25),
            Mediana = median(divppvsr),
            Q3 = quantile(divppvsr, 0.75),
            Máximo = max(divppvsr),
            RIQ = IQR(divppvsr), n = n())

summary_total_divppvsr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(divppvsr),
            Desviación_Estándar = sd(divppvsr),
            Minímo = min(divppvsr),
            Q1 = quantile(divppvsr, 0.25),
            Mediana = median(divppvsr),
            Q3 = quantile(divppvsr, 0.75),
            Máximo = max(divppvsr),
            RIQ = IQR(divppvsr), 
            n = n())

summary_table_divppvsr <- bind_rows(summary_table_divppvsr, summary_total_divppvsr)

summary_table_divppvsr <- as_tibble(summary_table_divppvsr)

formatted_table_divppvsr <- summary_table_divppvsr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divppvsr)

summary_table_divppvsr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(divppvsr),
            Desviación_Estándar = sd(divppvsr),
            Minímo = min(divppvsr),
            Q1 = quantile(divppvsr, 0.25),
            Mediana = median(divppvsr),
            Q3 = quantile(divppvsr, 0.75),
            Máximo = max(divppvsr),
            RIQ = IQR(divppvsr), n = n())

summary_total_divppvsrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(divppvsr),
            Desviación_Estándar = sd(divppvsr),
            Minímo = min(divppvsr),
            Q1 = quantile(divppvsr, 0.25),
            Mediana = median(divppvsr),
            Q3 = quantile(divppvsr, 0.75),
            Máximo = max(divppvsr),
            RIQ = IQR(divppvsr), 
            n = n())

summary_table_divppvsr_ae <- bind_rows(summary_table_divppvsr_ae, summary_total_divppvsrae)

summary_table_divppvsr_ae <- as_tibble(summary_table_divppvsr_ae)

formatted_table_divppvsr_ae <- summary_table_divppvsr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_divppvsr_ae)

summary_table_compacd <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(compacd),
            Desviación_Estándar = sd(compacd),
            Minímo = min(compacd),
            Q1 = quantile(compacd, 0.25),
            Mediana = median(compacd),
            Q3 = quantile(compacd, 0.75),
            Máximo = max(compacd),
            RIQ = IQR(compacd), n = n())

summary_total_compacd <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(compacd),
            Desviación_Estándar = sd(compacd),
            Minímo = min(compacd),
            Q1 = quantile(compacd, 0.25),
            Mediana = median(compacd),
            Q3 = quantile(compacd, 0.75),
            Máximo = max(compacd),
            RIQ = IQR(compacd), 
            n = n())

summary_table_compacd <- bind_rows(summary_table_compacd, summary_total_compacd)

summary_table_compacd <- as_tibble(summary_table_compacd)

formatted_table_compacd <- summary_table_compacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_compacd)

summary_table_compacd_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(compacd),
            Desviación_Estándar = sd(compacd),
            Minímo = min(compacd),
            Q1 = quantile(compacd, 0.25),
            Mediana = median(compacd),
            Q3 = quantile(compacd, 0.75),
            Máximo = max(compacd),
            RIQ = IQR(compacd), n = n())

summary_total_compacdae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(compacd),
            Desviación_Estándar = sd(compacd),
            Minímo = min(compacd),
            Q1 = quantile(compacd, 0.25),
            Mediana = median(compacd),
            Q3 = quantile(compacd, 0.75),
            Máximo = max(compacd),
            RIQ = IQR(compacd), 
            n = n())

summary_table_compacd_ae <- bind_rows(summary_table_compacd_ae, summary_total_compacdae)

summary_table_compacd_ae <- as_tibble(summary_table_compacd_ae)

formatted_table_compacd_ae <- summary_table_compacd_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_compacd_ae)

summary_table_comppvs <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(comppvs),
            Desviación_Estándar = sd(comppvs),
            Minímo = min(comppvs),
            Q1 = quantile(comppvs, 0.25),
            Mediana = median(comppvs),
            Q3 = quantile(comppvs, 0.75),
            Máximo = max(comppvs),
            RIQ = IQR(comppvs), n = n())

summary_table_compacdr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(compacdr),
            Desviación_Estándar = sd(compacdr),
            Minímo = min(compacdr),
            Q1 = quantile(compacdr, 0.25),
            Mediana = median(compacdr),
            Q3 = quantile(compacdr, 0.75),
            Máximo = max(compacdr),
            RIQ = IQR(compacdr), n = n())

summary_total_compacdr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(compacdr),
            Desviación_Estándar = sd(compacdr),
            Minímo = min(compacdr),
            Q1 = quantile(compacdr, 0.25),
            Mediana = median(compacdr),
            Q3 = quantile(compacdr, 0.75),
            Máximo = max(compacdr),
            RIQ = IQR(compacdr), 
            n = n())

summary_table_compacdr <- bind_rows(summary_table_compacdr, summary_total_compacdr)

summary_table_compacdr <- as_tibble(summary_table_compacdr)

formatted_table_compacdr <- summary_table_compacdr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_compacdr)

summary_table_compacdr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(compacdr),
            Desviación_Estándar = sd(compacdr),
            Minímo = min(compacdr),
            Q1 = quantile(compacdr, 0.25),
            Mediana = median(compacdr),
            Q3 = quantile(compacdr, 0.75),
            Máximo = max(compacdr),
            RIQ = IQR(compacdr), n = n())

summary_total_compacdrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(compacdr),
            Desviación_Estándar = sd(compacdr),
            Minímo = min(compacdr),
            Q1 = quantile(compacdr, 0.25),
            Mediana = median(compacdr),
            Q3 = quantile(compacdr, 0.75),
            Máximo = max(compacdr),
            RIQ = IQR(compacdr), 
            n = n())

summary_table_compacdr_ae <- bind_rows(summary_table_compacdr_ae, summary_total_compacdrae)

summary_table_compacdr_ae <- as_tibble(summary_table_compacdr_ae)

formatted_table_compacdr_ae <- summary_table_compacdr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_compacdr_ae)

summary_total_comppvs <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(comppvs),
            Desviación_Estándar = sd(comppvs),
            Minímo = min(comppvs),
            Q1 = quantile(comppvs, 0.25),
            Mediana = median(comppvs),
            Q3 = quantile(comppvs, 0.75),
            Máximo = max(comppvs),
            RIQ = IQR(comppvs), 
            n = n())

summary_table_comppvs <- bind_rows(summary_table_comppvs, summary_total_comppvs)

summary_table_comppvs <- as_tibble(summary_table_comppvs)

formatted_table_comppvs <- summary_table_comppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_comppvs)

summary_table_comppvs_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(comppvs),
            Desviación_Estándar = sd(comppvs),
            Minímo = min(comppvs),
            Q1 = quantile(comppvs, 0.25),
            Mediana = median(comppvs),
            Q3 = quantile(comppvs, 0.75),
            Máximo = max(comppvs),
            RIQ = IQR(comppvs), n = n())

summary_total_comppvsae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(comppvs),
            Desviación_Estándar = sd(comppvs),
            Minímo = min(comppvs),
            Q1 = quantile(comppvs, 0.25),
            Mediana = median(comppvs),
            Q3 = quantile(comppvs, 0.75),
            Máximo = max(comppvs),
            RIQ = IQR(comppvs), 
            n = n())

summary_table_comppvs_ae <- bind_rows(summary_table_comppvs_ae, summary_total_comppvsae)

summary_table_comppvs_ae <- as_tibble(summary_table_comppvs_ae)

formatted_table_comppvs_ae <- summary_table_comppvs_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_comppvs_ae)

summary_table_comppvsr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(comppvsr),
            Desviación_Estándar = sd(comppvsr),
            Minímo = min(comppvsr),
            Q1 = quantile(comppvsr, 0.25),
            Mediana = median(comppvsr),
            Q3 = quantile(comppvsr, 0.75),
            Máximo = max(comppvsr),
            RIQ = IQR(comppvsr), n = n())

summary_total_comppvsr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(comppvsr),
            Desviación_Estándar = sd(comppvsr),
            Minímo = min(comppvsr),
            Q1 = quantile(comppvsr, 0.25),
            Mediana = median(comppvsr),
            Q3 = quantile(comppvsr, 0.75),
            Máximo = max(comppvsr),
            RIQ = IQR(comppvsr), 
            n = n())

summary_table_comppvsr <- bind_rows(summary_table_comppvsr, summary_total_comppvsr)

summary_table_comppvsr <- as_tibble(summary_table_comppvsr)

formatted_table_comppvsr <- summary_table_comppvsr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_comppvsr)

summary_table_comppvsr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(comppvsr),
            Desviación_Estándar = sd(comppvsr),
            Minímo = min(comppvsr),
            Q1 = quantile(comppvsr, 0.25),
            Mediana = median(comppvsr),
            Q3 = quantile(comppvsr, 0.75),
            Máximo = max(comppvsr),
            RIQ = IQR(comppvsr), n = n())

summary_total_comppvsrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(comppvsr),
            Desviación_Estándar = sd(comppvsr),
            Minímo = min(comppvsr),
            Q1 = quantile(comppvsr, 0.25),
            Mediana = median(comppvsr),
            Q3 = quantile(comppvsr, 0.75),
            Máximo = max(comppvsr),
            RIQ = IQR(comppvsr), 
            n = n())

summary_table_comppvsr_ae <- bind_rows(summary_table_comppvsr_ae, summary_total_comppvsrae)

summary_table_comppvsr_ae <- as_tibble(summary_table_comppvsr_ae)

formatted_table_comppvsr_ae <- summary_table_comppvsr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_comppvsr_ae)

summary_table_roa <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(roa),
            Desviación_Estándar = sd(roa),
            Minímo = min(roa),
            Q1 = quantile(roa, 0.25),
            Mediana = median(roa),
            Q3 = quantile(roa, 0.75),
            Máximo = max(roa),
            RIQ = IQR(roa), n = n())

summary_total_roa <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(roa),
            Desviación_Estándar = sd(roa),
            Minímo = min(roa),
            Q1 = quantile(roa, 0.25),
            Mediana = median(roa),
            Q3 = quantile(roa, 0.75),
            Máximo = max(roa),
            RIQ = IQR(roa), 
            n = n())

summary_table_roa <- bind_rows(summary_table_roa, summary_total_roa)

summary_table_roa <- as_tibble(summary_table_roa)

formatted_table_roa <- summary_table_roa %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roa)

summary_table_roa_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(roa),
            Desviación_Estándar = sd(roa),
            Minímo = min(roa),
            Q1 = quantile(roa, 0.25),
            Mediana = median(roa),
            Q3 = quantile(roa, 0.75),
            Máximo = max(roa),
            RIQ = IQR(roa), n = n())

summary_total_roaae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(roa),
            Desviación_Estándar = sd(roa),
            Minímo = min(roa),
            Q1 = quantile(roa, 0.25),
            Mediana = median(roa),
            Q3 = quantile(roa, 0.75),
            Máximo = max(roa),
            RIQ = IQR(roa), 
            n = n())

summary_table_roa_ae <- bind_rows(summary_table_roa_ae, summary_total_roaae)

summary_table_roa_ae <- as_tibble(summary_table_roa_ae)

formatted_table_roa_ae <- summary_table_roa_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roa_ae)

summary_table_roar <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(roar),
            Desviación_Estándar = sd(roar),
            Minímo = min(roar),
            Q1 = quantile(roar, 0.25),
            Mediana = median(roar),
            Q3 = quantile(roar, 0.75),
            Máximo = max(roar),
            RIQ = IQR(roar), n = n())

summary_total_roar <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(roar),
            Desviación_Estándar = sd(roar),
            Minímo = min(roar),
            Q1 = quantile(roar, 0.25),
            Mediana = median(roar),
            Q3 = quantile(roar, 0.75),
            Máximo = max(roar),
            RIQ = IQR(roar), 
            n = n())

summary_table_roar <- bind_rows(summary_table_roar, summary_total_roar)

summary_table_roar <- as_tibble(summary_table_roar)

formatted_table_roar <- summary_table_roar %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roar)

summary_table_roar_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(roar),
            Desviación_Estándar = sd(roar),
            Minímo = min(roar),
            Q1 = quantile(roar, 0.25),
            Mediana = median(roar),
            Q3 = quantile(roar, 0.75),
            Máximo = max(roar),
            RIQ = IQR(roar), n = n())

summary_total_roarae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(roar),
            Desviación_Estándar = sd(roar),
            Minímo = min(roar),
            Q1 = quantile(roar, 0.25),
            Mediana = median(roar),
            Q3 = quantile(roar, 0.75),
            Máximo = max(roar),
            RIQ = IQR(roar), 
            n = n())

summary_table_roar_ae <- bind_rows(summary_table_roar_ae, summary_total_roarae)

summary_table_roar_ae <- as_tibble(summary_table_roar_ae)

formatted_table_roar_ae <- summary_table_roar_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roar_ae)

summary_table_roic <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(roic),
            Desviación_Estándar = sd(roic),
            Minímo = min(roic),
            Q1 = quantile(roic, 0.25),
            Mediana = median(roic),
            Q3 = quantile(roic, 0.75),
            Máximo = max(roic),
            RIQ = IQR(roic), n = n())

summary_total_roic <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(roic),
            Desviación_Estándar = sd(roic),
            Minímo = min(roic),
            Q1 = quantile(roic, 0.25),
            Mediana = median(roic),
            Q3 = quantile(roic, 0.75),
            Máximo = max(roic),
            RIQ = IQR(roic), 
            n = n())

summary_table_roic <- bind_rows(summary_table_roic, summary_total_roic)

summary_table_roic <- as_tibble(summary_table_roic)

formatted_table_roic <- summary_table_roic %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roic)

summary_table_roic_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(roic),
            Desviación_Estándar = sd(roic),
            Minímo = min(roic),
            Q1 = quantile(roic, 0.25),
            Mediana = median(roic),
            Q3 = quantile(roic, 0.75),
            Máximo = max(roic),
            RIQ = IQR(roic), n = n())

summary_total_roicae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(roic),
            Desviación_Estándar = sd(roic),
            Minímo = min(roic),
            Q1 = quantile(roic, 0.25),
            Mediana = median(roic),
            Q3 = quantile(roic, 0.75),
            Máximo = max(roic),
            RIQ = IQR(roic), 
            n = n())

summary_table_roic_ae <- bind_rows(summary_table_roic_ae, summary_total_roicae)

summary_table_roic_ae <- as_tibble(summary_table_roic_ae)

formatted_table_roic_ae <- summary_table_roic_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roic_ae)

summary_table_roicr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(roicr),
            Desviación_Estándar = sd(roicr),
            Minímo = min(roicr),
            Q1 = quantile(roicr, 0.25),
            Mediana = median(roicr),
            Q3 = quantile(roicr, 0.75),
            Máximo = max(roicr),
            RIQ = IQR(roicr), n = n())

summary_total_roicr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(roicr),
            Desviación_Estándar = sd(roicr),
            Minímo = min(roicr),
            Q1 = quantile(roicr, 0.25),
            Mediana = median(roicr),
            Q3 = quantile(roicr, 0.75),
            Máximo = max(roicr),
            RIQ = IQR(roicr), 
            n = n())

summary_table_roicr <- bind_rows(summary_table_roicr, summary_total_roicr)

summary_table_roicr <- as_tibble(summary_table_roicr)

formatted_table_roicr <- summary_table_roicr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roicr)

summary_table_roicr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(roicr),
            Desviación_Estándar = sd(roicr),
            Minímo = min(roicr),
            Q1 = quantile(roicr, 0.25),
            Mediana = median(roicr),
            Q3 = quantile(roicr, 0.75),
            Máximo = max(roicr),
            RIQ = IQR(roicr), n = n())

summary_total_roicrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(roicr),
            Desviación_Estándar = sd(roicr),
            Minímo = min(roicr),
            Q1 = quantile(roicr, 0.25),
            Mediana = median(roicr),
            Q3 = quantile(roicr, 0.75),
            Máximo = max(roicr),
            RIQ = IQR(roicr), 
            n = n())

summary_table_roicr_ae <- bind_rows(summary_table_roicr_ae, summary_total_roicrae)

summary_table_roicr_ae <- as_tibble(summary_table_roicr_ae)

formatted_table_roicr_ae <- summary_table_roicr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_roicr_ae)

summary_table_rat <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(rat),
            Desviación_Estándar = sd(rat),
            Minímo = min(rat),
            Q1 = quantile(rat, 0.25),
            Mediana = median(rat),
            Q3 = quantile(rat, 0.75),
            Máximo = max(rat),
            RIQ = IQR(rat), n = n())

summary_total_rat <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(rat),
            Desviación_Estándar = sd(rat),
            Minímo = min(rat),
            Q1 = quantile(rat, 0.25),
            Mediana = median(rat),
            Q3 = quantile(rat, 0.75),
            Máximo = max(rat),
            RIQ = IQR(rat), 
            n = n())

summary_table_rat <- bind_rows(summary_table_rat, summary_total_rat)

summary_table_rat <- as_tibble(summary_table_rat)

formatted_table_rat <- summary_table_rat %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_rat)

summary_table_rat_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(rat),
            Desviación_Estándar = sd(rat),
            Minímo = min(rat),
            Q1 = quantile(rat, 0.25),
            Mediana = median(rat),
            Q3 = quantile(rat, 0.75),
            Máximo = max(rat),
            RIQ = IQR(rat), n = n())

summary_total_ratae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(rat),
            Desviación_Estándar = sd(rat),
            Minímo = min(rat),
            Q1 = quantile(rat, 0.25),
            Mediana = median(rat),
            Q3 = quantile(rat, 0.75),
            Máximo = max(rat),
            RIQ = IQR(rat), 
            n = n())

summary_table_rat_ae <- bind_rows(summary_table_rat_ae, summary_total_ratae)

summary_table_rat_ae <- as_tibble(summary_table_rat_ae)

formatted_table_rat_ae <- summary_table_rat_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_rat_ae)

summary_table_ratr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(ratr),
            Desviación_Estándar = sd(ratr),
            Minímo = min(ratr),
            Q1 = quantile(ratr, 0.25),
            Mediana = median(ratr),
            Q3 = quantile(ratr, 0.75),
            Máximo = max(ratr),
            RIQ = IQR(ratr), n = n())

summary_total_ratr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(ratr),
            Desviación_Estándar = sd(ratr),
            Minímo = min(ratr),
            Q1 = quantile(ratr, 0.25),
            Mediana = median(ratr),
            Q3 = quantile(ratr, 0.75),
            Máximo = max(ratr),
            RIQ = IQR(ratr), 
            n = n())

summary_table_ratr <- bind_rows(summary_table_ratr, summary_total_ratr)

summary_table_ratr <- as_tibble(summary_table_ratr)

formatted_table_ratr <- summary_table_ratr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ratr)

summary_table_ratr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(ratr),
            Desviación_Estándar = sd(ratr),
            Minímo = min(ratr),
            Q1 = quantile(ratr, 0.25),
            Mediana = median(ratr),
            Q3 = quantile(ratr, 0.75),
            Máximo = max(ratr),
            RIQ = IQR(ratr), n = n())

summary_total_ratrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(ratr),
            Desviación_Estándar = sd(ratr),
            Minímo = min(ratr),
            Q1 = quantile(ratr, 0.25),
            Mediana = median(ratr),
            Q3 = quantile(ratr, 0.75),
            Máximo = max(ratr),
            RIQ = IQR(ratr), 
            n = n())

summary_table_ratr_ae <- bind_rows(summary_table_ratr_ae, summary_total_ratrae)

summary_table_ratr_ae <- as_tibble(summary_table_ratr_ae)

formatted_table_ratr_ae <- summary_table_ratr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ratr_ae)

summary_table_iaf <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(iaf),
            Desviación_Estándar = sd(iaf),
            Minímo = min(iaf),
            Q1 = quantile(iaf, 0.25),
            Mediana = median(iaf),
            Q3 = quantile(iaf, 0.75),
            Máximo = max(iaf),
            RIQ = IQR(iaf), n = n())

summary_total_iaf <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(iaf),
            Desviación_Estándar = sd(iaf),
            Minímo = min(iaf),
            Q1 = quantile(iaf, 0.25),
            Mediana = median(iaf),
            Q3 = quantile(iaf, 0.75),
            Máximo = max(iaf),
            RIQ = IQR(iaf), 
            n = n())

summary_table_iaf <- bind_rows(summary_table_iaf, summary_total_iaf)

summary_table_iaf <- as_tibble(summary_table_iaf)

formatted_table_iaf <- summary_table_iaf %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_iaf)

summary_table_iaf_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(iaf),
            Desviación_Estándar = sd(iaf),
            Minímo = min(iaf),
            Q1 = quantile(iaf, 0.25),
            Mediana = median(iaf),
            Q3 = quantile(iaf, 0.75),
            Máximo = max(iaf),
            RIQ = IQR(iaf), n = n())

summary_total_iafae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(iaf),
            Desviación_Estándar = sd(iaf),
            Minímo = min(iaf),
            Q1 = quantile(iaf, 0.25),
            Mediana = median(iaf),
            Q3 = quantile(iaf, 0.75),
            Máximo = max(iaf),
            RIQ = IQR(iaf), 
            n = n())

summary_table_iaf_ae <- bind_rows(summary_table_iaf_ae, summary_total_iafae)

summary_table_iaf_ae <- as_tibble(summary_table_iaf_ae)

formatted_table_iaf_ae <- summary_table_iaf_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_iaf_ae)

summary_table_iafr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(iafr),
            Desviación_Estándar = sd(iafr),
            Minímo = min(iafr),
            Q1 = quantile(iafr, 0.25),
            Mediana = median(iafr),
            Q3 = quantile(iafr, 0.75),
            Máximo = max(iafr),
            RIQ = IQR(iafr), n = n())

summary_total_iafr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(iafr),
            Desviación_Estándar = sd(iafr),
            Minímo = min(iafr),
            Q1 = quantile(iafr, 0.25),
            Mediana = median(iafr),
            Q3 = quantile(iafr, 0.75),
            Máximo = max(iafr),
            RIQ = IQR(iafr), 
            n = n())

summary_table_iafr <- bind_rows(summary_table_iafr, summary_total_iafr)

summary_table_iafr <- as_tibble(summary_table_iafr)

formatted_table_iafr <- summary_table_iafr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_iafr)

summary_table_iafr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(iafr),
            Desviación_Estándar = sd(iafr),
            Minímo = min(iafr),
            Q1 = quantile(iafr, 0.25),
            Mediana = median(iafr),
            Q3 = quantile(iafr, 0.75),
            Máximo = max(iafr),
            RIQ = IQR(iafr), n = n())

summary_total_iafrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(iafr),
            Desviación_Estándar = sd(iafr),
            Minímo = min(iafr),
            Q1 = quantile(iafr, 0.25),
            Mediana = median(iafr),
            Q3 = quantile(iafr, 0.75),
            Máximo = max(iafr),
            RIQ = IQR(iafr), 
            n = n())

summary_table_iafr_ae <- bind_rows(summary_table_iafr_ae, summary_total_iafrae)

summary_table_iafr_ae <- as_tibble(summary_table_iafr_ae)

formatted_table_iafr_ae <- summary_table_iafr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_iafr_ae)

summary_table_dca <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(dca),
            Desviación_Estándar = sd(dca),
            Minímo = min(dca),
            Q1 = quantile(dca, 0.25),
            Mediana = median(dca),
            Q3 = quantile(dca, 0.75),
            Máximo = max(dca),
            RIQ = IQR(dca), n = n())

summary_total_dca <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(dca),
            Desviación_Estándar = sd(dca),
            Minímo = min(dca),
            Q1 = quantile(dca, 0.25),
            Mediana = median(dca),
            Q3 = quantile(dca, 0.75),
            Máximo = max(dca),
            RIQ = IQR(dca), 
            n = n())

summary_table_dca <- bind_rows(summary_table_dca, summary_total_dca)

summary_table_dca <- as_tibble(summary_table_dca)

formatted_table_dca <- summary_table_dca %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_dca)

summary_table_dca_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(dca),
            Desviación_Estándar = sd(dca),
            Minímo = min(dca),
            Q1 = quantile(dca, 0.25),
            Mediana = median(dca),
            Q3 = quantile(dca, 0.75),
            Máximo = max(dca),
            RIQ = IQR(dca), n = n())

summary_total_dcaae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(dca),
            Desviación_Estándar = sd(dca),
            Minímo = min(dca),
            Q1 = quantile(dca, 0.25),
            Mediana = median(dca),
            Q3 = quantile(dca, 0.75),
            Máximo = max(dca),
            RIQ = IQR(dca), 
            n = n())

summary_table_dca_ae <- bind_rows(summary_table_dca_ae, summary_total_dcaae)

summary_table_dca_ae <- as_tibble(summary_table_dca_ae)

formatted_table_dca_ae <- summary_table_dca_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_dca_ae)

summary_table_dcar <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(dcar),
            Desviación_Estándar = sd(dcar),
            Minímo = min(dcar),
            Q1 = quantile(dcar, 0.25),
            Mediana = median(dcar),
            Q3 = quantile(dcar, 0.75),
            Máximo = max(dcar),
            RIQ = IQR(dcar), n = n())

summary_total_dcar <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(dcar),
            Desviación_Estándar = sd(dcar),
            Minímo = min(dcar),
            Q1 = quantile(dcar, 0.25),
            Mediana = median(dcar),
            Q3 = quantile(dcar, 0.75),
            Máximo = max(dcar),
            RIQ = IQR(dcar), 
            n = n())

summary_table_dcar <- bind_rows(summary_table_dcar, summary_total_dcar)

summary_table_dcar <- as_tibble(summary_table_dcar)

formatted_table_dcar <- summary_table_dcar %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_dcar)

summary_table_dcar_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(dcar),
            Desviación_Estándar = sd(dcar),
            Minímo = min(dcar),
            Q1 = quantile(dcar, 0.25),
            Mediana = median(dcar),
            Q3 = quantile(dcar, 0.75),
            Máximo = max(dcar),
            RIQ = IQR(dcar), n = n())

summary_total_dcarae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(dcar),
            Desviación_Estándar = sd(dcar),
            Minímo = min(dcar),
            Q1 = quantile(dcar, 0.25),
            Mediana = median(dcar),
            Q3 = quantile(dcar, 0.75),
            Máximo = max(dcar),
            RIQ = IQR(dcar), 
            n = n())

summary_table_dcar_ae <- bind_rows(summary_table_dcar_ae, summary_total_dcarae)

summary_table_dcar_ae <- as_tibble(summary_table_dcar_ae)

formatted_table_dcar_ae <- summary_table_dcar_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_dcar_ae)

summary_table_mbi <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(mbi),
            Desviación_Estándar = sd(mbi),
            Minímo = min(mbi),
            Q1 = quantile(mbi, 0.25),
            Mediana = median(mbi),
            Q3 = quantile(mbi, 0.75),
            Máximo = max(mbi),
            RIQ = IQR(mbi), n = n())

summary_total_mbi <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(mbi),
            Desviación_Estándar = sd(mbi),
            Minímo = min(mbi),
            Q1 = quantile(mbi, 0.25),
            Mediana = median(mbi),
            Q3 = quantile(mbi, 0.75),
            Máximo = max(mbi),
            RIQ = IQR(mbi), 
            n = n())

summary_table_mbi <- bind_rows(summary_table_mbi, summary_total_mbi)

summary_table_mbi <- as_tibble(summary_table_mbi)

formatted_table_mbi <- summary_table_mbi %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_mbi)

summary_table_mbi_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(mbi),
            Desviación_Estándar = sd(mbi),
            Minímo = min(mbi),
            Q1 = quantile(mbi, 0.25),
            Mediana = median(mbi),
            Q3 = quantile(mbi, 0.75),
            Máximo = max(mbi),
            RIQ = IQR(mbi), n = n())

summary_total_mbiae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(mbi),
            Desviación_Estándar = sd(mbi),
            Minímo = min(mbi),
            Q1 = quantile(mbi, 0.25),
            Mediana = median(mbi),
            Q3 = quantile(mbi, 0.75),
            Máximo = max(mbi),
            RIQ = IQR(mbi), 
            n = n())

summary_table_mbi_ae <- bind_rows(summary_table_mbi_ae, summary_total_mbiae)

summary_table_mbi_ae <- as_tibble(summary_table_mbi_ae)

formatted_table_mbi_ae <- summary_table_mbi_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_mbi_ae)

summary_table_mbir <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(mbir),
            Desviación_Estándar = sd(mbir),
            Minímo = min(mbir),
            Q1 = quantile(mbir, 0.25),
            Mediana = median(mbir),
            Q3 = quantile(mbir, 0.75),
            Máximo = max(mbir),
            RIQ = IQR(mbir), n = n())

summary_total_mbir <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(mbir),
            Desviación_Estándar = sd(mbir),
            Minímo = min(mbir),
            Q1 = quantile(mbir, 0.25),
            Mediana = median(mbir),
            Q3 = quantile(mbir, 0.75),
            Máximo = max(mbir),
            RIQ = IQR(mbir), 
            n = n())

summary_table_mbir <- bind_rows(summary_table_mbir, summary_total_mbir)

summary_table_mbir <- as_tibble(summary_table_mbir)

formatted_table_mbir <- summary_table_mbir %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_mbir)

summary_table_mbir_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(mbir),
            Desviación_Estándar = sd(mbir),
            Minímo = min(mbir),
            Q1 = quantile(mbir, 0.25),
            Mediana = median(mbir),
            Q3 = quantile(mbir, 0.75),
            Máximo = max(mbir),
            RIQ = IQR(mbir), n = n())

summary_total_mbirae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(mbir),
            Desviación_Estándar = sd(mbir),
            Minímo = min(mbir),
            Q1 = quantile(mbir, 0.25),
            Mediana = median(mbir),
            Q3 = quantile(mbir, 0.75),
            Máximo = max(mbir),
            RIQ = IQR(mbir), 
            n = n())

summary_table_mbir_ae <- bind_rows(summary_table_mbir_ae, summary_total_mbirae)

summary_table_mbir_ae <- as_tibble(summary_table_mbir_ae)

formatted_table_mbir_ae <- summary_table_mbir_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_mbir_ae)

summary_table_pacd <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(pacd),
            Desviación_Estándar = sd(pacd),
            Minímo = min(pacd),
            Q1 = quantile(pacd, 0.25),
            Mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            Máximo = max(pacd),
            RIQ = IQR(pacd), n = n())

summary_total_pacd <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(pacd),
            Desviación_Estándar = sd(pacd),
            Minímo = min(pacd),
            Q1 = quantile(pacd, 0.25),
            Mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            Máximo = max(pacd),
            RIQ = IQR(pacd), 
            n = n())

summary_table_pacd <- bind_rows(summary_table_pacd, summary_total_pacd)

summary_table_pacd <- as_tibble(summary_table_pacd)

formatted_table_pacd <- summary_table_pacd %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pacd)

summary_table_pacd_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(pacd),
            Desviación_Estándar = sd(pacd),
            Minímo = min(pacd),
            Q1 = quantile(pacd, 0.25),
            Mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            Máximo = max(pacd),
            RIQ = IQR(pacd), n = n())

summary_total_pacdae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(pacd),
            Desviación_Estándar = sd(pacd),
            Minímo = min(pacd),
            Q1 = quantile(pacd, 0.25),
            Mediana = median(pacd),
            Q3 = quantile(pacd, 0.75),
            Máximo = max(pacd),
            RIQ = IQR(pacd), 
            n = n())

summary_table_pacd_ae <- bind_rows(summary_table_pacd_ae, summary_total_pacdae)

summary_table_pacd_ae <- as_tibble(summary_table_pacd_ae)

formatted_table_pacd_ae <- summary_table_pacd_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pacd_ae)

summary_table_pacdr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(pacdr),
            Desviación_Estándar = sd(pacdr),
            Minímo = min(pacdr),
            Q1 = quantile(pacdr, 0.25),
            Mediana = median(pacdr),
            Q3 = quantile(pacdr, 0.75),
            Máximo = max(pacdr),
            RIQ = IQR(pacdr), n = n())

summary_total_pacdr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(pacdr),
            Desviación_Estándar = sd(pacdr),
            Minímo = min(pacdr),
            Q1 = quantile(pacdr, 0.25),
            Mediana = median(pacdr),
            Q3 = quantile(pacdr, 0.75),
            Máximo = max(pacdr),
            RIQ = IQR(pacdr), 
            n = n())

summary_table_pacdr <- bind_rows(summary_table_pacdr, summary_total_pacdr)

summary_table_pacdr <- as_tibble(summary_table_pacdr)

formatted_table_pacdr <- summary_table_pacdr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pacdr)

summary_table_pacdr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(pacdr),
            Desviación_Estándar = sd(pacdr),
            Minímo = min(pacdr),
            Q1 = quantile(pacdr, 0.25),
            Mediana = median(pacdr),
            Q3 = quantile(pacdr, 0.75),
            Máximo = max(pacdr),
            RIQ = IQR(pacdr), n = n())

summary_total_pacdrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(pacdr),
            Desviación_Estándar = sd(pacdr),
            Minímo = min(pacdr),
            Q1 = quantile(pacdr, 0.25),
            Mediana = median(pacdr),
            Q3 = quantile(pacdr, 0.75),
            Máximo = max(pacdr),
            RIQ = IQR(pacdr), 
            n = n())

summary_table_pacdr_ae <- bind_rows(summary_table_pacdr_ae, summary_total_pacdrae)

summary_table_pacdr_ae <- as_tibble(summary_table_pacdr_ae)

formatted_table_pacdr_ae <- summary_table_pacdr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pacdr_ae)

summary_table_ppvs <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(ppvs),
            Desviación_Estándar = sd(ppvs),
            Minímo = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            Mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            Máximo = max(ppvs),
            RIQ = IQR(ppvs), n = n())

summary_total_ppvs <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(ppvs),
            Desviación_Estándar = sd(ppvs),
            Minímo = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            Mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            Máximo = max(ppvs),
            RIQ = IQR(ppvs), 
            n = n())

summary_table_ppvs <- bind_rows(summary_table_ppvs, summary_total_ppvs)

summary_table_ppvs <- as_tibble(summary_table_ppvs)

formatted_table_ppvs <- summary_table_ppvs %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ppvs)

summary_table_ppvs_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(ppvs),
            Desviación_Estándar = sd(ppvs),
            Minímo = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            Mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            Máximo = max(ppvs),
            RIQ = IQR(ppvs), n = n())

summary_total_ppvsae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(ppvs),
            Desviación_Estándar = sd(ppvs),
            Minímo = min(ppvs),
            Q1 = quantile(ppvs, 0.25),
            Mediana = median(ppvs),
            Q3 = quantile(ppvs, 0.75),
            Máximo = max(ppvs),
            RIQ = IQR(ppvs), 
            n = n())

summary_table_ppvs_ae <- bind_rows(summary_table_ppvs_ae, summary_total_ppvsae)

summary_table_ppvs_ae <- as_tibble(summary_table_ppvs_ae)

formatted_table_ppvs_ae <- summary_table_ppvs_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ppvs_ae)

summary_table_ppvsr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(ppvsr),
            Desviación_Estándar = sd(ppvsr),
            Minímo = min(ppvsr),
            Q1 = quantile(ppvsr, 0.25),
            Mediana = median(ppvsr),
            Q3 = quantile(ppvsr, 0.75),
            Máximo = max(ppvsr),
            RIQ = IQR(ppvsr), n = n())

summary_total_ppvsr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(ppvsr),
            Desviación_Estándar = sd(ppvsr),
            Minímo = min(ppvsr),
            Q1 = quantile(ppvsr, 0.25),
            Mediana = median(ppvsr),
            Q3 = quantile(ppvsr, 0.75),
            Máximo = max(ppvsr),
            RIQ = IQR(ppvsr), 
            n = n())

summary_table_ppvsr <- bind_rows(summary_table_ppvsr, summary_total_ppvsr)

summary_table_ppvsr <- as_tibble(summary_table_ppvsr)

formatted_table_ppvsr <- summary_table_ppvsr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ppvsr)

summary_table_ppvsr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(ppvsr),
            Desviación_Estándar = sd(ppvsr),
            Minímo = min(ppvsr),
            Q1 = quantile(ppvsr, 0.25),
            Mediana = median(ppvsr),
            Q3 = quantile(ppvsr, 0.75),
            Máximo = max(ppvsr),
            RIQ = IQR(ppvsr), n = n())

summary_total_ppvsrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(ppvsr),
            Desviación_Estándar = sd(ppvsr),
            Minímo = min(ppvsr),
            Q1 = quantile(ppvsr, 0.25),
            Mediana = median(ppvsr),
            Q3 = quantile(ppvsr, 0.75),
            Máximo = max(ppvsr),
            RIQ = IQR(ppvsr), 
            n = n())

summary_table_ppvsr_ae <- bind_rows(summary_table_ppvsr_ae, summary_total_ppvsrae)

summary_table_ppvsr_ae <- as_tibble(summary_table_ppvsr_ae)

formatted_table_ppvsr_ae <- summary_table_ppvsr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_ppvsr_ae)

summary_table_vacb <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(vacb),
            Desviación_Estándar = sd(vacb),
            Minímo = min(vacb),
            Q1 = quantile(vacb, 0.25),
            Mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            Máximo = max(vacb),
            RIQ = IQR(vacb), n = n())

summary_total_vacb <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(vacb),
            Desviación_Estándar = sd(vacb),
            Minímo = min(vacb),
            Q1 = quantile(vacb, 0.25),
            Mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            Máximo = max(vacb),
            RIQ = IQR(vacb), 
            n = n())

summary_table_vacb <- bind_rows(summary_table_vacb, summary_total_vacb)

summary_table_vacb <- as_tibble(summary_table_vacb)

formatted_table_vacb <- summary_table_vacb %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_vacb)

summary_table_vacb_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(vacb),
            Desviación_Estándar = sd(vacb),
            Minímo = min(vacb),
            Q1 = quantile(vacb, 0.25),
            Mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            Máximo = max(vacb),
            RIQ = IQR(vacb), n = n())

summary_total_vacbae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(vacb),
            Desviación_Estándar = sd(vacb),
            Minímo = min(vacb),
            Q1 = quantile(vacb, 0.25),
            Mediana = median(vacb),
            Q3 = quantile(vacb, 0.75),
            Máximo = max(vacb),
            RIQ = IQR(vacb), 
            n = n())

summary_table_vacb_ae <- bind_rows(summary_table_vacb_ae, summary_total_vacbae)

summary_table_vacb_ae <- as_tibble(summary_table_vacb_ae)

formatted_table_vacb_ae <- summary_table_vacb_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_vacb_ae)

summary_table_vacbr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(vacbr),
            Desviación_Estándar = sd(vacbr),
            Minímo = min(vacbr),
            Q1 = quantile(vacbr, 0.25),
            Mediana = median(vacbr),
            Q3 = quantile(vacbr, 0.75),
            Máximo = max(vacbr),
            RIQ = IQR(vacbr), n = n())

summary_total_vacbr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(vacbr),
            Desviación_Estándar = sd(vacbr),
            Minímo = min(vacbr),
            Q1 = quantile(vacbr, 0.25),
            Mediana = median(vacbr),
            Q3 = quantile(vacbr, 0.75),
            Máximo = max(vacbr),
            RIQ = IQR(vacbr), 
            n = n())

summary_table_vacbr <- bind_rows(summary_table_vacbr, summary_total_vacbr)

summary_table_vacbr <- as_tibble(summary_table_vacbr)

formatted_table_vacbr <- summary_table_vacbr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_vacbr)

summary_table_vacbr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(vacbr),
            Desviación_Estándar = sd(vacbr),
            Minímo = min(vacbr),
            Q1 = quantile(vacbr, 0.25),
            Mediana = median(vacbr),
            Q3 = quantile(vacbr, 0.75),
            Máximo = max(vacbr),
            RIQ = IQR(vacbr), n = n())

summary_total_vacbrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(vacbr),
            Desviación_Estándar = sd(vacbr),
            Minímo = min(vacbr),
            Q1 = quantile(vacbr, 0.25),
            Mediana = median(vacbr),
            Q3 = quantile(vacbr, 0.75),
            Máximo = max(vacbr),
            RIQ = IQR(vacbr), 
            n = n())

summary_table_vacbr_ae <- bind_rows(summary_table_vacbr_ae, summary_total_vacbrae)

summary_table_vacbr_ae <- as_tibble(summary_table_vacbr_ae)

formatted_table_vacbr_ae <- summary_table_vacbr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_vacbr_ae)

summary_table_pbt <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(pbt),
            Desviación_Estándar = sd(pbt),
            Minímo = min(pbt),
            Q1 = quantile(pbt, 0.25),
            Mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            Máximo = max(pbt),
            RIQ = IQR(pbt), n = n())

summary_total_pbt <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(pbt),
            Desviación_Estándar = sd(pbt),
            Minímo = min(pbt),
            Q1 = quantile(pbt, 0.25),
            Mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            Máximo = max(pbt),
            RIQ = IQR(pbt), 
            n = n())

summary_table_pbt <- bind_rows(summary_table_pbt, summary_total_pbt)

summary_table_pbt <- as_tibble(summary_table_pbt)

formatted_table_pbt <- summary_table_pbt %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pbt)

summary_table_pbt_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(pbt),
            Desviación_Estándar = sd(pbt),
            Minímo = min(pbt),
            Q1 = quantile(pbt, 0.25),
            Mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            Máximo = max(pbt),
            RIQ = IQR(pbt), n = n())

summary_total_pbtae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(pbt),
            Desviación_Estándar = sd(pbt),
            Minímo = min(pbt),
            Q1 = quantile(pbt, 0.25),
            Mediana = median(pbt),
            Q3 = quantile(pbt, 0.75),
            Máximo = max(pbt),
            RIQ = IQR(pbt), 
            n = n())

summary_table_pbt_ae <- bind_rows(summary_table_pbt_ae, summary_total_pbtae)

summary_table_pbt_ae <- as_tibble(summary_table_pbt_ae)

formatted_table_pbt_ae <- summary_table_pbt_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pbt_ae)

summary_table_pbtr <- data_ds %>%
  group_by(NOMGEO) %>%
  summarise(Media = mean(pbtr),
            Desviación_Estándar = sd(pbtr),
            Minímo = min(pbtr),
            Q1 = quantile(pbtr, 0.25),
            Mediana = median(pbtr),
            Q3 = quantile(pbtr, 0.75),
            Máximo = max(pbtr),
            RIQ = IQR(pbtr), n = n())

summary_total_pbtr <- data_ds %>%
  summarise(NOMGEO = "Total",
            Media = mean(pbtr),
            Desviación_Estándar = sd(pbtr),
            Minímo = min(pbtr),
            Q1 = quantile(pbtr, 0.25),
            Mediana = median(pbtr),
            Q3 = quantile(pbtr, 0.75),
            Máximo = max(pbtr),
            RIQ = IQR(pbtr), 
            n = n())

summary_table_pbtr <- bind_rows(summary_table_pbtr, summary_total_pbtr)

summary_table_pbtr <- as_tibble(summary_table_pbtr)

formatted_table_pbtr <- summary_table_pbtr %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pbtr)

summary_table_pbtr_ae <- data_ds %>%
  group_by(AE) %>%
  summarise(Media = mean(pbtr),
            Desviación_Estándar = sd(pbtr),
            Minímo = min(pbtr),
            Q1 = quantile(pbtr, 0.25),
            Mediana = median(pbtr),
            Q3 = quantile(pbtr, 0.75),
            Máximo = max(pbtr),
            RIQ = IQR(pbtr), n = n())

summary_total_pbtrae <- data_ds %>%
  summarise(AE = "Total",
            Media = mean(pbtr),
            Desviación_Estándar = sd(pbtr),
            Minímo = min(pbtr),
            Q1 = quantile(pbtr, 0.25),
            Mediana = median(pbtr),
            Q3 = quantile(pbtr, 0.75),
            Máximo = max(pbtr),
            RIQ = IQR(pbtr), 
            n = n())

summary_table_pbtr_ae <- bind_rows(summary_table_pbtr_ae, summary_total_pbtrae)

summary_table_pbtr_ae <- as_tibble(summary_table_pbtr_ae)

formatted_table_pbtr_ae <- summary_table_pbtr_ae %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

print(formatted_table_pbtr_ae)

####### Análisis economías de localización y urbanización


cor_matrix <- data_ds %>%
  select(marpacdr, marppvsr, compacdr, comppvsr,
         divpacdr, divppvsr, prpacdr, prppvsr,
         ptfr, ptfm, ctm, cem, vacbr, pbtr) %>%
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

### Constructing the spatial weights matrix

d01 <- coordenadas 
set.seed(123)
coord_sf1$long <- coord_sf1$long + rnorm(nrow(coord_sf1), mean = 0, sd = 0.0001)
coord_sf1$lat <- coord_sf1$lat + rnorm(nrow(coord_sf1), mean = 0, sd = 0.0001)
coord_sf1 <- d01[rep(1:nrow(d01), each = 19), ]
vecinos <- knearneigh(coord_sf1[, c("long", "lat")], k = 4)
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



joined_df_ds <- left_join(data_ds, mx, by = c("NOMGEO" = "NOMGEO", "tent"="CVEGEO"))
dpsf_ds <- st_sf(joined_df_ds)
dpsf01_ds <- merge(dpsf_ds, d01, by = "NOMGEO")

### Segmentation

dp03 <- dpsf01_ds %>% filter(tcode == "2003")
dp08 <- dpsf01_ds %>% filter(tcode == "2008")
dp13 <- dpsf01_ds %>% filter(tcode == "2013")
dp18 <- dpsf01_ds %>% filter(tcode == "2018")

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
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  labs(title = "2008",
       x = "PRPPVSR",
       y = "COMPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs08

prodvscompacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2008",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd08

prodvsdivppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2008",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs08

prodvsdivpacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2008",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd08

prodvsespppvs08 <- ggplot(dp08, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2008",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs08

prodvsesppacd08 <- ggplot(dp08, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2008",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsesppacd08

prodvscomppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(comppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPPVSR", y = "COMPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs13

prodvscompacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd13

prodvsdivppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs13

prodvsdivpacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd13

prodvsespppvs13 <- ggplot(dp13, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs13

prodvsesppacd13 <- ggplot(dp13, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2013",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsesppacd13

prodvscomppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(comppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPPVSR", y = "COMPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvscomppvs18

prodvscompacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(compacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPACDR", y = "COMPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvscompacd18

prodvsdivppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(divppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPPVSR", y = "DIVPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivppvs18

prodvsdivpacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(divpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPACDR", y = "DIVPACD") +
  theme_minimal()+ 
  guides(color = "none")

prodvsdivpacd18

prodvsespppvs18 <- ggplot(dp18, aes(x = scale(prppvsr), y = scale(marppvs), color = clusterppvs)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPPVSR", y = "MARPPVS") +
  theme_minimal()+ 
  guides(color = "none")

prodvsespppvs18

prodvsesppacd18 <- ggplot(dp18, aes(x = scale(prpacdr), y = scale(marpacd), color = clusterpacd)) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 3000) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "2018",
       x = "PRPACDR", y = "MARPACD") +
  theme_minimal() + 
  guides(color = "none")

prodvsesppacd18

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

prodvsextppvs <- plot_grid(prodvsespppvs08, prodvsespppvs13, prodvsespppvs18,  
                           prodvsdivppvs08, prodvsdivppvs13, prodvsdivppvs18,
                           prodvscomppvs08, prodvscomppvs13, prodvscomppvs18,
                           ncol = 3,
                           align = "h", rel_widths = c(1, 1)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

print(prodvsextppvs)

prodvsextpacd <- plot_grid(prodvsesppacd08, prodvsesppacd13, prodvsesppacd18,  
                           prodvsdivpacd08, prodvsdivpacd13, prodvsdivpacd18,
                           prodvscompacd08, prodvscompacd13, prodvscompacd18,
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
  labs(title = "PRPACDR 2018-2003",
       fill = "PRPACDR")

clupacd03

cluppvs03 <- ggplot(data = dp03) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPPVSR 2018-2003",
       fill = "PRPPVSR")

cluppvs03

clupacd08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPACDR 2008-2003",
       fill = "PRPACDR")

clupacd08

cluppvs08 <- ggplot(data = dp08) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPPVSR 2008-2003",
       fill = "PRPPVSR")

cluppvs08

clupacd13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPACDR 2013-2008",
       fill = "PRPACDR")

clupacd13

cluppvs13 <- ggplot(data = dp13) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPPVSR 2013-2008",
       fill = "PRPPVSR")

cluppvs13

clupacd18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = clusterpacd)) +
  scale_fill_viridis_d(option = "plasma", direction= 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPACDR 2018-2013",
       fill = "PRPACDR")

clupacd18

cluppvs18 <- ggplot(data = dp18) +
  geom_sf(aes(fill = clusterppvs)) +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "PRPPVSR 2018-2013",
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


### Indicadores de rentabilidad

roappvs08 <- ggplot(dp08, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 30) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs ROA 2008",
       x = "MARPPVSR", y = "ROA") +
  theme_minimal()

roapacd08 <- ggplot(dp08, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 30) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs ROA 2008", 
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

roappvs13 <- ggplot(dp13, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs ROA 2013",
       x = "MARPPVSR", y = "ROA") +
  theme_minimal()

roapacd13 <- ggplot(dp13, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 30) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs ROA 2013", 
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

roappvs18 <- ggplot(dp18, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 30) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs ROA 2018",
       x = "MARPPVS", y = "ROA") +
  theme_minimal()

roapacd18 <- ggplot(dp18, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 30) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs ROA 2018", 
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

mbippvs08 <- ggplot(dp08, aes(x = scale(marppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs MBI 2008", 
       x = "MARPPVSR", y = "MBI") +
  theme_minimal()

mbipacd08 <- ggplot(dp08, aes(x = scale(marpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs MBI 2008", 
       x = "MARPACDR", y = "MBI") +
  theme_minimal()

mbippvs13 <- ggplot(dp13, aes(x = scale(marppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs MBI 2013", 
       x = "MARPPVSR", y = "MBI") +
  theme_minimal()

mbipacd13 <- ggplot(dp13, aes(x = scale(marpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs MBI 2013", 
       x = "MARPACDR", y = "MBI") +
  theme_minimal()

mbippvs18 <- ggplot(dp18, aes(x = scale(marppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPPVSR vs MBI 2018", 
       x = "MARPPVSR", y = "MBI") +
  theme_minimal()

mbipacd18 <- ggplot(dp18, aes(x = scale(marpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MARPACDR vs MBI 2018", 
       x = "MARPACDR", y = "MBI") +
  theme_minimal()

rentppvs <- plot_grid(roappvs08, roappvs13, roappvs18, mbippvs08, mbippvs13, mbippvs18, ncol = 3,
                      align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentppvs)

rentpacd <- plot_grid(roapacd08, roapacd13, roapacd18, mbipacd08, mbipacd13, mbipacd18, ncol = 3,
                      align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentpacd)


roappvs08 <- ggplot(dp08, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs ROA 2008",
       x = "DIVPPVSR", y = "ROA") +
  theme_minimal()

roapacd08 <- ggplot(dp08, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs ROA 2008", 
       x = "DIVPACDR", y = "ROA") +
  theme_minimal()

roappvs13 <- ggplot(dp13, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs ROA 2013",
       x = "DIVPPVSR", y = "ROA") +
  theme_minimal()

roapacd13 <- ggplot(dp13, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs ROA 2013", 
       x = "DIVPACDR", y = "ROA") +
  theme_minimal()

roappvs18 <- ggplot(dp18, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs ROA 2018",
       x = "DIVPPVS", y = "ROA") +
  theme_minimal()

roapacd18 <- ggplot(dp18, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs ROA 2018", 
       x = "DIVPACDR", y = "ROA") +
  theme_minimal()

mbippvs08 <- ggplot(dp08, aes(x = scale(divppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs MBI 2008", 
       x = "DIVPPVSR", y = "MBI") +
  theme_minimal()

mbipacd08 <- ggplot(dp08, aes(x = scale(divpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs MBI 2008", 
       x = "DIVPACDR", y = "MBI") +
  theme_minimal()

mbippvs13 <- ggplot(dp13, aes(x = scale(divppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs MBI 2013", 
       x = "DIVPPVSR", y = "MBI") +
  theme_minimal()

mbipacd13 <- ggplot(dp13, aes(x = scale(divpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs MBI 2013", 
       x = "DIVPACDR", y = "MBI") +
  theme_minimal()

mbippvs18 <- ggplot(dp18, aes(x = scale(divppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPPVSR vs MBI 2018", 
       x = "DIVPPVSR", y = "MBI") +
  theme_minimal()

mbipacd18 <- ggplot(dp18, aes(x = scale(divpacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "DIVPACDR vs MBI 2018", 
       x = "DIVPACDR", y = "MBI") +
  theme_minimal()

rentdivppvs <- plot_grid(roappvs08, roappvs13, roappvs18, mbippvs08, mbippvs13, mbippvs18, ncol = 3,
                         align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentdivppvs)

rentdivpacd <- plot_grid(roapacd08, roapacd13, roapacd18, mbipacd08, mbipacd13, mbipacd18, ncol = 3,
                         align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentdivpacd)

roappvs08 <- ggplot(dp08, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs ROA 2008",
       x = "COMPPVSR", y = "ROA") +
  theme_minimal()

roapacd08 <- ggplot(dp08, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs ROA 2008", 
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

roappvs13 <- ggplot(dp13, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs ROA 2013",
       x = "COMPPVSR", y = "ROA") +
  theme_minimal()

roapacd13 <- ggplot(dp13, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs ROA 2013", 
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

roappvs18 <- ggplot(dp18, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs ROA 2018",
       x = "COMPPVS", y = "ROA") +
  theme_minimal()

roapacd18 <- ggplot(dp18, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs ROA 2018", 
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

mbippvs08 <- ggplot(dp08, aes(x = scale(comppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs MBI 2008", 
       x = "COMPPVSR", y = "MBI") +
  theme_minimal()

mbipacd08 <- ggplot(dp08, aes(x = scale(compacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs MBI 2008", 
       x = "COMPACDR", y = "MBI") +
  theme_minimal()

mbippvs13 <- ggplot(dp13, aes(x = scale(comppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs MBI 2013", 
       x = "COMPPVSR", y = "MBI") +
  theme_minimal()

mbipacd13 <- ggplot(dp13, aes(x = scale(compacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs MBI 2013", 
       x = "COMPACDR", y = "MBI") +
  theme_minimal()

mbippvs18 <- ggplot(dp18, aes(x = scale(comppvsr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPPVSR vs MBI 2018", 
       x = "COMPPVSR", y = "MBI") +
  theme_minimal()

mbipacd18 <- ggplot(dp18, aes(x = scale(compacdr), y = scale(mbi))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "COMPACDR vs MBI 2018", 
       x = "COMPACDR", y = "MBI") +
  theme_minimal()

rentcomppvs <- plot_grid(roappvs08, roappvs13, roappvs18, mbippvs08, mbippvs13, mbippvs18, ncol = 3,
                         align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentcomppvs)

rentcompacd <- plot_grid(roapacd08, roapacd13, roapacd18, mbipacd08, mbipacd13, mbipacd18, ncol = 3,
                         align = "h", rel_widths = c(1, 1, 1)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

print(rentcompacd)

ggplot(dp08, aes(x = scale(ppvs), y = scale(ermjppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 50) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Shift & Share PPVS 2008",
       x = "PPVS", y = "ERMJPPVS") +
  theme_minimal()

ggplot(dp13, aes(x = scale(ppvs), y = scale(ermjppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 100) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Shift & Share PPVS 2013",
       x = "PPVS", y = "ERMJPPVS") +
  theme_minimal()

ggplot(dp18, aes(x = scale(ppvs), y = scale(ermjppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Shift & Share PPVS 2018",
       x = "PPVS", y = "ERMJPPVS") +
  theme_minimal()

ggplot(dp08, aes(x = scale(marpacdr), y = scale(compacd))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y competencia PACD 2008",
       x = "MARPACD", y = "COMPACD") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marppvsr), y = scale(divppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y diversificación PPVS 2013",
       x = "MARPPVSR", y = "DIVPPVSR") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marppvsr), y = scale(comppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y competencia PPVS 2013",
       x = "MARPPVS", y = "COMPPVS") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marpacdr), y = scale(divpacd))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y diversificación PACD 2013",
       x = "MARPACDR", y = "DIVPACDR") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marpacdr), y = scale(compacd))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y competencia PACD 2013",
       x = "MARPACD", y = "COMPACD") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marppvsr), y = scale(divppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y diversificación PPVS 2018",
       x = "MARPPVSR", y = "DIVPPVSR") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marppvsr), y = scale(comppvs))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y competencia PPVS 2018",
       x = "MARPPVS", y = "COMPPVS") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marpacdr), y = scale(divpacd))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y diversificación PACD 2018",
       x = "MARPACDR", y = "DIVPACDR") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marpacdr), y = scale(compacd))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y competencia PACD 2018",
       x = "MARPACD", y = "COMPACD") +
  theme_minimal()

ggplot(dp08, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2008",
       x = "MARPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(marppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2008",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp08, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2008",
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(marpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2008",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2013",
       x = "MARPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2013",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2013",
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp13, aes(x = scale(marpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2013",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2018",
       x = "MARPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PPVS 2018",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2018",
       x = "MARPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp18, aes(x = scale(marpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la especialización y rentabilidad PACD 2018",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp08, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2008",
       x = "COMPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(comppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2008",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp08, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2008",
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(compacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2008",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2013",
       x = "COMPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp13, aes(x = scale(comppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2013",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2013",
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp13, aes(x = scale(compacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2013",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(comppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2018",
       x = "COMPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp18, aes(x = scale(comppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PPVS 2018",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(compacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2018",
       x = "COMPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp18, aes(x = scale(compacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la competencia y rentabilidad PACD 2018",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp08, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2008",
       x = "DIVPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(divppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2008",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp08, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2008",
       x = "DIVPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp08, aes(x = scale(divpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2008",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2013",
       x = "DIVPPVSR", y = "ROA") +
  theme_minimal()

ggplot(dp13, aes(x = scale(divppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2013",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp13, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2013",
       x = "DIVPACDR", y = "ROAR") +
  theme_minimal()

ggplot(dp13, aes(x = scale(divpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2013",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(divppvsr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2018",
       x = "DIVPPVSR", y = "ROAR") +
  theme_minimal()

ggplot(dp18, aes(x = scale(divppvsr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PPVS 2018",
       x = "MARPPVS", y = "ROIC") +
  theme_minimal()

ggplot(dp18, aes(x = scale(divpacdr), y = scale(roa))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2018",
       x = "DIVPACDR", y = "ROA") +
  theme_minimal()

ggplot(dp18, aes(x = scale(divpacdr), y = scale(roic))) +
  geom_point(color = "blue", size = 0.5) +
  geom_text_repel(aes(label = NOMGEO), max.overlaps = 40) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre el crecimiento de la diversificación y rentabilidad PACD 2018",
       x = "MARPACD", y = "ROIC") +
  theme_minimal()

### Gráficos de red círcular

pecpp03 <- ind03 %>% select(marppvs:prpacd)
pecpp03 <- as.data.frame(scale(pecpp03))
pecpp03 <- as.matrix(pecpp03)
pecpp08 <- ind08 %>% select(marppvs:prpacd)
pecpp08 <- as.data.frame(scale(pecpp08))
pecpp08 <- as.matrix(pecpp08)
pecpp13 <- ind13 %>% select(marppvs:prpacd)
pecpp13 <- as.data.frame(scale(pecpp13))
pecpp13 <- as.matrix(pecpp13)
pecpp18 <- ind18 %>% select(marppvs:prpacd)
pecpp18 <- as.data.frame(scale(pecpp18))
pecpp18 <- as.matrix(pecpp18)


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

title(main = "2003")

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

title(main = "2008")

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

title(main = "2013")

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

title(main = "2018")

circos.clear()
