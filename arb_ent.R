# install.packages("h2o", repos = "https://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")

library(devtools)
library(rpivotTable)
library(psych)
library(recipes)
library(modeldata)
library(funModeling)
library(pastecs)
library(PerformanceAnalytics)
library(treemapify)
library(treemap)
library(reshape2)
library(ggplot2)
library(patchwork)
library(gplots)
library(ggcorrplot)
library(ggpubr)
library(magick)
library(rattle)
library(pheatmap)
library(vcd)
library(plyr)
library(gridExtra)
library(car)
library(corrr)
library(caret)
library(knitr)
library(kableExtra)
library(skimr)
library(qcc)
library(dplyr)
library(datos)
library(tidyverse)
library(knitr)
library(evir)
library(reshape2)
library(lattice)
library(GGally)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(randomForest)
library(xgboost)
library(mice)
library(VIM)
library(DMwR2)
library(smotefamily)
library(ROSE)
library(Boruta)
library(h2o)
library(shapper)
library(DALEX)
library(reticulate)
use_condaenv("myenv", required = TRUE)
py_config()
library(lime)
library(iml)

data <- bin

tablaman<- rpivotTable(data, rows="NOMGEO", col="AE", aggregatorName="Average", 
                     vals="marppvs")
tablaman

hist(data$pacd, breaks = 30, main = "Distribución pacd", xlab = "marppvs")
hist(data$ppvs, breaks = 30, main = "Distribución ppvs", xlab = "divppvs")
hist(data$pot, breaks = 30, main = "Distribución pot", xlab = "ptf")

data[["pacd"]] <- replace(data[["pacd"]], is.na(data[["pacd"]]), 0)
data[["ppvs"]] <- replace(data[["ppvs"]], is.na(data[["ppvs"]]), 0)
data[["pot"]] <- replace(data[["pot"]], is.na(data[["pot"]]), 0)


lower_bound_pacd <- quantile(data$pacd, 0.01)
upper_bound_pacd <- quantile(data$pacd, 0.99)

lower_bound_ppvs <- quantile(data$ppvs, 0.01)
upper_bound_ppvs <- quantile(data$ppvs, 0.99)

lower_bound_pot <- quantile(data$pot, 0.01)
upper_bound_pot <- quantile(data$pot, 0.99)

data <- data %>%
  filter(pacd >= lower_bound_pacd & pacd <= upper_bound_pacd) %>%
  filter(ppvs >= lower_bound_ppvs & ppvs <= upper_bound_ppvs) %>%
  filter(pot >= lower_bound_pot & pot <= upper_bound_pot)

# Data frame para modelos

cao <- select(data, NOMGEO, tcode, AE, intec:pot) 

ca2003 <- filter(cao, tcode == 2003)  
ca2008 <- filter(cao, tcode == 2008)  
ca2013 <- filter(cao, tcode == 2013)  
ca2018 <- filter(cao, tcode == 2018)

ca2003$tcode <- NULL
ca2008$tcode <- NULL
ca2013$tcode <- NULL
ca2018$tcode <- NULL

rec <- recipe(~ ., data = ca2003) %>%
  step_range(all_numeric(), -all_outcomes()) %>%  
  prep()

ca2003 <- bake(rec, new_data = ca2003)

print(ca2003)

rec <- recipe(~ ., data = ca2008) %>%
  step_range(all_numeric(), -all_outcomes()) %>%  
  prep()

ca2008 <- bake(rec, new_data = ca2008)

print(ca2008)

rec <- recipe(~ ., data = ca2013) %>%
  step_range(all_numeric(), -all_outcomes()) %>%  
  prep()

ca2013 <- bake(rec, new_data = ca2013)

print(ca2013)

rec <- recipe(~ ., data = ca2018) %>%
  step_range(all_numeric(), -all_outcomes()) %>%  
  prep()

ca2018 <- bake(rec, new_data = ca2018)

print(ca2018)

percentiles <- seq(0, 1, by = 0.1)

global_limits_pacd <- quantile(ca2003$pacd, probs = percentiles)

global_limits_ppvs <- quantile(ca2003$ppvs, probs = percentiles)

global_limits_pot <- quantile(ca2003$pot, probs = percentiles)

ca2003 <- ca2003 %>%
  mutate(
    segpacd = cut(pacd, breaks = global_limits_pacd, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segppvs = cut(ppvs, breaks = global_limits_ppvs, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segpot = cut(pot, breaks = global_limits_pot, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE)
  )


table(ca2003$segpacd)

table(ca2003$segppvs)

table(ca2003$segpot)

global_limits_pacd <- quantile(ca2008$pacd, probs = percentiles)

global_limits_ppvs <- quantile(ca2008$ppvs, probs = percentiles)

global_limits_pot <- quantile(ca2008$pot, probs = percentiles)

ca2008 <- ca2008 %>%
  mutate(
    segpacd = cut(pacd, breaks = global_limits_pacd, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segppvs = cut(ppvs, breaks = global_limits_ppvs, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segpot = cut(pot, breaks = global_limits_pot, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE)
  )


table(ca2008$segpacd)

table(ca2008$segppvs)

table(ca2008$segpot)

global_limits_pacd <- quantile(ca2013$pacd, probs = percentiles)

global_limits_ppvs <- quantile(ca2013$ppvs, probs = percentiles)

global_limits_pot <- quantile(ca2013$pot, probs = percentiles)

ca2013 <- ca2013 %>%
  mutate(
    segpacd = cut(pacd, breaks = global_limits_pacd, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segppvs = cut(ppvs, breaks = global_limits_ppvs, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segpot = cut(pot, breaks = global_limits_pot, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE)
  )


table(ca2013$segpacd)

table(ca2013$segppvs)

table(ca2013$segpot)

global_limits_pacd <- quantile(ca2018$pacd, probs = percentiles)

global_limits_ppvs <- quantile(ca2018$ppvs, probs = percentiles)

global_limits_pot <- quantile(ca2018$pot, probs = percentiles)

ca2018 <- ca2018 %>%
  mutate(
    segpacd = cut(pacd, breaks = global_limits_pacd, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segppvs = cut(ppvs, breaks = global_limits_ppvs, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE),
    segpot = cut(pot, breaks = global_limits_pot, labels = paste0("P", seq(10, 100, by = 10)), include.lowest = TRUE)
  )


table(ca2018$segpacd)

table(ca2018$segppvs)

table(ca2018$segpot)


set.seed(987654321)

trainindex = createDataPartition(cao$AE, p=0.75)$Resample1
cao_train= cao[trainindex, ]
cao_test= cao[-trainindex, ]

trainindex = createDataPartition(ca2003$AE, p=0.75)$Resample1
ca2003_train= ca2003[trainindex, ]
ca2003_test= ca2003[-trainindex, ]

trainindex = createDataPartition(ca2008$AE, p=0.75)$Resample1
ca2008_train= ca2008[trainindex, ]
ca2008_test= ca2008[-trainindex, ]

trainindex = createDataPartition(ca2013$AE, p=0.75)$Resample1
ca2013_train= ca2013[trainindex, ]
ca2013_test= ca2013[-trainindex, ]

trainindex = createDataPartition(ca2018$AE, p=0.75)$Resample1
ca2018_train= ca2018[trainindex, ]
ca2018_test= ca2018[-trainindex, ]



# Resumir datos por estado (NOMGEO)
state_summary <- data %>%
  group_by(NOMGEO) %>%
  reframe(
    count = n(),
    mean_icp = mean(icp, na.rm = TRUE),
    mean_ici = mean(ici, na.rm = TRUE),
    mean_ics = mean(ics, na.rm = TRUE)
  )

print(state_summary)

# Resumir datos por sector (AE)
sector_summary <- data %>%
  group_by(AE) %>%
  reframe(
    count = n(),
    mean_icp = mean(icp, na.rm = TRUE),
    mean_ici = mean(ici, na.rm = TRUE),
    mean_ics = mean(ics, na.rm = TRUE)
  )

print(sector_summary)


# Visualizar datos por estado (NOMGEO)
plot_state_icp <- ggplot(state_summary, 
                         aes(x = NOMGEO, y = mean_icp)) +
  geom_bar(stat = "identity") +
  labs(title = "Media Índice de Capacidades de Producción", x = "Estado", y = "Media de marpot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot_state_icp)

# Visualizar datos por sector (AE)
plot_sector_icp <- ggplot(sector_summary, 
                             aes(x = AE, y = mean_icp)) +
  geom_bar(stat = "identity") +
  labs(title = "Media Índice de Capacidades de Producción por Sector", x = "Sector", y = "Media de marpot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot_sector_icp)

# Función de análisis por estado
state_analysis <- function(data, variable, category_var = NULL) {
  data %>%
    group_by(NOMGEO, AE) %>%
    reframe(
      count = n(),
      mean = mean({{ variable }}, na.rm = TRUE),
      sd = sd({{ variable }}, na.rm = TRUE),
      min = min({{ variable }}, na.rm = TRUE),
      max = max({{ variable }}, na.rm = TRUE)
    )
}

# Función de análisis por quintil y sector
quintil_sector_analysis <- function(ca2003, variable, segment) {
  ca2003 %>%
    group_by(AE, !!sym(segment)) %>%
    reframe(
      count = n(),
      mean = mean(!!sym(variable), na.rm = TRUE),
      sd = sd(!!sym(variable), na.rm = TRUE),
      min = min(!!sym(variable), na.rm = TRUE),
      max = max(!!sym(variable), na.rm = TRUE)
    )
}

# Análisis detallado para cada variable de interés
state_icp <- state_analysis(ca2003, "icp", "AE")
print(state_icp)
quintil_state_ici <- quintil_state_analysis(ca2003, "ici", "segpot")
print(quintil_state_ici)
quintil_state_ics <- quintil_state_analysis(ca2003, "ics", "segpot")
print(quintil_state_ics)
quintil_sector_ici <- quintil_sector_analysis(data, "ici", "segpot")
quintil_state_ics <- quintil_state_analysis(data, "ics", "segpot")

# Mostrar resultados en formato de tabla
# Tabla para quintil y estado
quintil_state_marpot %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Tabla para quintil y sector
quintil_sector_divpot %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

quintil_analysis <- function(data, variable, segment) {
  data %>%
    group_by(!!sym(segment)) %>%
    summarise(
      count = n(),
      mean = mean(!!sym(variable), na.rm = TRUE),
      sd = sd(!!sym(variable), na.rm = TRUE),
      min = min(!!sym(variable), na.rm = TRUE),
      max = max(!!sym(variable), na.rm = TRUE)
    )
}

# Análisis de quintiles para cada variable de interés
quintil_analysis_marpot <- quintil_analysis(data, "marpot", "segmarpot")

quintil_analysis_divpot <- quintil_analysis(data, "divpot", "segdivpot")

# Mostrar los resultados
print(quintil_analysis_marpot)

print(quintil_analysis_divpot)

# Función para visualizar datos por quintil
visualize_quintil <- function(data, variable, segment) {
  ggplot(data, aes_string(x = segment, y = variable)) +
    geom_boxplot() +
    labs(title = paste("Distribución de", variable, "por Quintil"),
         x = "Quintil",
         y = variable)
}

# Visualizar los datos para cada variable de interés
plot_marpot <- visualize_quintil(data, "marpot", "segmarpot")

plot_divpot <- visualize_quintil(data, "divpot", "segdivpot")

# Mostrar los gráficos
print(plot_marpot)

print(plot_divpot)


# Función para obtener características por quintil
characteristics_by_quintil <- function(data, variable, segment) {
  data %>%
    group_by(!!sym(segment)) %>%
    summarise(
      count = n(),
      mean = mean(!!sym(variable), na.rm = TRUE),
      median = median(!!sym(variable), na.rm = TRUE),
      sd = sd(!!sym(variable), na.rm = TRUE)
    ) %>%
    arrange(!!sym(segment))
}

# Obtener características por quintil para cada variable de interés
char_marpot <- characteristics_by_quintil(data, "marpot", "segmarpot")

char_divpot <- characteristics_by_quintil(data, "divpot", "segdivpot")

# Mostrar los resultados
print(char_marpot)

print(char_divpot)

# Agrupar datos por NOMGEO, AE y tcode
data_grouped <- data %>%
  group_by(NOMGEO, AE, tcode)

# Calcular estadísticas descriptivas por NOMGEO, AE y tcode
stats_grouped <- data_grouped %>%
  summarize(
    mean_marpot = mean(marpot),
    median_marpot = median(marpot),
    sd_marpot = sd(marpot),
    mean_divpot = mean(divpot),
    median_divpot = median(divpot),
    sd_divpot = sd(divpot)
  )


# Gráfico de barras para la media de marpot por NOMGEO y AE
ggplot(stats_grouped, aes(x = AE, y = mean_marpot, fill = tcode)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NOMGEO) +
  labs(title = "Media de marpot por sector y lustro", x = "Sector (AE)", y = "Media de marpot")

ggplot(stats_grouped, aes(x = AE, y = mean_divpot, fill = tcode)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NOMGEO) +
  labs(title = "Media de divpot por sector y lustro", x = "Sector (AE)", y = "Media de divpot")

# Boxplot para marpot por NOMGEO y AE
ggplot(data, aes(x = AE, y = marpot, fill = tcode)) +
  geom_boxplot() +
  facet_wrap(~ NOMGEO) +
  labs(title = "Boxplot de marpot por sector y lustro", x = "Sector (AE)", y = "Valor de marpot")

ggplot(data, aes(x = AE, y = divpot, fill = tcode)) +
  geom_boxplot() +
  facet_wrap(~ NOMGEO) +
  labs(title = "Boxplot de divpot por sector y lustro", x = "Sector (AE)", y = "Valor de divpot")

# Histograma para marpot por NOMGEO y AE
ggplot(data, aes(x = marpot, fill = tcode)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~  AE) +
  labs(title = "Histograma de marpot por sector", x = "Valor de marpot", y = "Frecuencia")

ggplot(data, aes(x = divpot, fill = tcode)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~  AE) +
  labs(title = "Histograma de marpot por sector", x = "Valor de divpot", y = "Frecuencia")

ggplot(data, aes(x = marpot, fill = tcode)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~  NOMGEO) +
  labs(title = "Histograma de marpot por entidad", x = "Valor de marpot", y = "Frecuencia")

ggplot(data, aes(x = marpot, fill = tcode)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~  NOMGEO) +
  labs(title = "Histograma de divpot por entidad", x = "Valor de divpot", y = "Frecuencia")

estado_segmento <- data %>%
  group_by(segmarpot, NOMGEO) %>%
  summarise(count = n()) %>%
  ungroup()

estado_segmento_d <- data %>%
  group_by(segdivpot, NOMGEO) %>%
  summarise(count = n()) %>%
  ungroup()

# Mostrar resultados en formato de tabla
estado_segmento %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

estado_segmento_d %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


sector_segmento <- data %>%
  group_by(segmarpot, AE) %>%
  summarise(count = n()) %>%
  ungroup()

sector_segmento_d <- data %>%
  group_by(segdivpot, AE) %>%
  summarise(count = n()) %>%
  ungroup()

# Mostrar resultados en formato de tabla
sector_segmento %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

sector_segmento_d %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



p1 <- ggplot(estado_segmento, aes(x = segmarpot, y = count, fill = NOMGEO)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución estatal por segmento especialización", x = "Segmento", y = "Número de estados") +
  theme_minimal()

p2 <- ggplot(estado_segmento_d, aes(x = segdivpot, y = count, fill = NOMGEO)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución estatal por segmento del peso en la diversidad", x = "Segmento", y = "Número de estados") +
  theme_minimal()

p3 <- ggplot(sector_segmento, aes(x = segmarpot, y = count, fill = AE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución sectorial por segmento especialización", x = "Segmento", y = "Número de sectores") +
  theme_minimal()

p4 <- ggplot(sector_segmento_d, aes(x = segdivpot, y = count, fill = AE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución sectorial por segmento del peso en la diversidad", x = "Segmento", y = "Número de sectores") +
  theme_minimal()

combined_plot <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2)

print(combined_plot)


# Árboles de decisión

set.seed(1234567890)

# por censos

control_poda <- rpart.control(
  cp = 0.012,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)


admarpot03 <-  rpart(segpot ~ icp+ici+ics,
                     data = ca2003_train, method = "class", control = control_poda)
predatmarpot03 <- predict(admarpot03, ca2003_test, type = "class")
confusionMatrix(predatmarpot03, ca2003_test$segpot)

fancyRpartPlot(admarpot03, main = "Clasificación especialización laboral 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI")


control_poda <- rpart.control(
  cp = 0.012,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

admarpot08 <- rpart(segpot ~ roa	+roic	+iaf	+rat	+dca	+mbi+	
                      ecppvs	+ecpacd	+intec	+efene	+automa+
                      tasaimasd	+renact	+intcap	+intsevav	+
                      capdig	+sostop	+intcon	+efcost	+invtal	+
                      ptfm+ctm+cem, 
                     data = ca2008_train, method = "class", control = control_poda)
predatmarpot08 <- predict(admarpot08, ca2008_test, type = "class")
confusionMatrix(predatmarpot08, ca2008_test$segpot)

fancyRpartPlot(admarpot08, main = "Clasificación especialización laboral 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2009: INEGI")


control_poda <- rpart.control(
  cp = 0.011,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)


admarpot13 <- rpart(segmarpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptfm+ctm+cem, 
                     data = ca2013_train, method = "class", control = control_poda)
predatmarpot13 <- predict(admarpot13, ca2013_test, type = "class")
confusionMatrix(predatmarpot13, ca2013_test$segmarpot)

fancyRpartPlot(admarpot13, main = "Clasificación especialización laboral 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2014: INEGI")


control_poda <- rpart.control(
  cp = 0.015,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)


admarpot18 <- rpart(segmarpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptfm+ctm+cem, 
                     data = ca2018_train, method = "class", control = control_poda)
predatmarpot18 <- predict(admarpot18, ca2018_test, type = "class")
confusionMatrix(predatmarpot18, ca2018_test$segmarpot)

fancyRpartPlot(admarpot18, main = "Clasificación especialización laboral 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2019: INEGI")




control_poda <- rpart.control(
  cp = 0.015,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

addivpot03 <- rpart(segdivpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptf, 
                     data = ca2003_train, method = "class", control = control_poda)
predatdivpot03 <- predict(addivpot03, ca2003_test, type = "class")
confusionMatrix(predatdivpot03, ca2003_test$segdivpot)

fancyRpartPlot(addivpot03, main = "Clasificación diversidad laboral 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI")


control_poda <- rpart.control(
  cp = 0.012,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

addivpot08 <- rpart(segdivpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptfm+ctm+cem, 
                     data = ca2008_train, method = "class", control = control_poda)
predatdivpot08 <- predict(addivpot08, ca2008_test, type = "class")
confusionMatrix(predatdivpot08, ca2008_test$segdivpot)

fancyRpartPlot(addivpot08, main = "Clasificación diversidad laboral 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2009: INEGI")

control_poda <- rpart.control(
  cp = 0.014,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

addivpot13 <- rpart(segdivpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptfm+ctm+cem, 
                     data = ca2013_train, method = "class", control = control_poda)
predatdivpot13 <- predict(addivpot13, ca2013_test, type = "class")
confusionMatrix(predatdivpot13, ca2013_test$segdivpot)

fancyRpartPlot(addivpot13, main = "Clasificación diversidad laboral 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2014: INEGI")



control_poda <- rpart.control(
  cp = 0.011,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

addivpot18 <- rpart(segdivpot ~ ecppvs+ecpacd+roa+roic+iaf+rat+dca+mbi+
                      protec+rhft+capinn+intcon+ptfm+ctm+cem, 
                     data = ca2018_train, method = "class", control = control_poda)
predatdivpot18 <- predict(addivpot18, ca2018_test, type = "class")
confusionMatrix(predatdivpot18, ca2018_test$segdivpot)

fancyRpartPlot(addivpot18, main = "Clasificación diversidad laboral 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico 2019: INEGI")



#windows(width = 10, height = 10)

par(mfrow = c(2, 2), mar = c(6, 6, 4, 2))

fancyRpartPlot(admarppvs03, main = "Clasificación del MARPPVS 2003",
               sub = "")

fancyRpartPlot(admarppvs08, main = "Clasificación del MARPPVS 2008",
               sub = "")

fancyRpartPlot(admarppvs13, main = "Clasificación del MARPPVS 2013",
               sub = "")

fancyRpartPlot(admarppvs18, main = "Clasificación del MARPPVS 2018",
               sub = "")

fancyRpartPlot(admarpacd03, main = "Clasificación del MARPACD 2003",
               sub = "")

fancyRpartPlot(admarpacd08, main = "Clasificación del MARPACD 2008",
               sub = "")

fancyRpartPlot(admarpacd13, main = "Clasificación del MARPACD 2013",
               sub = "")

fancyRpartPlot(admarpacd18, main = "Clasificación del MARPACD 2018",
               sub = "")

fancyRpartPlot(addivpacd03, main = "Clasificación del DIVPACD 2003",
               sub = "")

fancyRpartPlot(addivpacd08, main = "Clasificación del DIVPACD 2008",
               sub = "")

fancyRpartPlot(addivpacd13, main = "Clasificación del DIVPACD 2013",
               sub = "")

fancyRpartPlot(addivpacd18, main = "Clasificación del DIVPACD 2018",
               sub = "")
fancyRpartPlot(addivppvs03, main = "Clasificación del DIVPPVS 2003",
               sub = "")

fancyRpartPlot(addivppvs08, main = "Clasificación del DIVPPVS 2008",
               sub = "")

fancyRpartPlot(addivppvs13, main = "Clasificación del DIVPPVS 2013",
               sub = "")

fancyRpartPlot(addivppvs18, main = "Clasificación del DIVPPVS 2018",
               sub = "")

par(mfrow = c(1, 1))



impadmarpot03 <- varImp(admarpot03)
impaddivpot03 <- varImp(addivpot03)
impadmarpot08 <- varImp(admarpot08)
impaddivpot08 <- varImp(addivpot08)
impadmarpot13 <- varImp(admarpot13)
impaddivpot13 <- varImp(addivpot13)
impadmarpot18 <- varImp(admarpot18)
impaddivpot18 <- varImp(addivpot18)

lista_impadmarpot <- list(
  impadmarpot03 = impadmarpot03,
  impadmarpot08 = impadmarpot08,
  impadmarpot13 = impadmarpot13,
  impadmarpot18 = impadmarpot18)

for (nombre in names(lista_impadmarpot)) {
  lista_impadmarpot[[nombre]]$Variable <- rownames(lista_impadmarpot[[nombre]])
  lista_impadmarpot[[nombre]]$Prueba <- nombre
}

resultados_largos_marpot <- do.call(rbind, lista_impadmarpot)

ggplot(resultados_largos_marpot, aes(x = Variable, y = Overall, fill = Prueba)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Importancia relativa en la segmentación de la especialización laboral años 2003-2018",
       x = "Variable",
       y = "Importancia")



lista_impaddivpot <- list(
  impaddivpot03 = impaddivpot03,
  impaddivpot08 = impaddivpot08,
  impaddivpot13 = impaddivpot13,
  impaddivpot18 = impaddivpot18)


for (nombre in names(lista_impaddivpot)) {
  lista_impaddivpot[[nombre]]$Variable <- rownames(lista_impaddivpot[[nombre]])
  lista_impaddivpot[[nombre]]$Prueba <- nombre
}

resultados_largos_divpot <- do.call(rbind, lista_impaddivpot)

ggplot(resultados_largos_divpot, aes(x = Variable, y = Overall, fill = Prueba)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Importancia relativa en la segmentación de la diversificación laboral años 2003-2018",
       x = "Variable",
       y = "Importancia")



#Automated Machine learning

h2o.init()

ca03_train_h2o <- as.h2o(ca2003_train)
ca03_test_h2o <- as.h2o(ca2003_test)

# aml divpot

y <- "segpot" # Variable objetivo

excluded_vars <- c("segpot", "segpacd", "ID", "segppvs", 
                    "AE", "ptfm", "cem", "ctm",
                   "pacd", "ppvs", "pot")

x <- setdiff(names(ca03_train_h2o), excluded_vars)

print(x)

# Ejecutar H2O AutoML
aml_divpot <- h2o.automl(x = x, y = y, training_frame = ca03_train_h2o,
                         nfolds = 5,
                         exploitation_ratio = 0.5,
                         keep_cross_validation_predictions = TRUE,
                         include_algos = c("DRF", "GBM", "XGBoost", "GLM"), 
                         max_models = 10,
                         max_runtime_secs = 360,
                         seed = 123)

# Ver el resumen de los modelos
summary(aml_divpot)


# Obtener el mejor modelo del leaderboard
best_model_divpot <- h2o.get_best_model(aml_divpot)
print(best_model_divpot)

model_type_divpot <- class(best_model_divpot)
print(model_type_divpot)

# Realizar predicciones en el conjunto de prueba
pred_divpot <- h2o.predict(best_model_divpot, ca03_test_h2o)

# Convertir las predicciones y el conjunto de prueba a data frames de R
pred_divpot_df <- as.data.frame(pred_divpot)
cao_test_df <- as.data.frame(ca03_test_h2o)

# Crear la matriz de confusión
cmdivpot <- confusionMatrix(as.factor(pred_divpot_df[, 1]), cao_test_df[, y])
print(cmdivpot)

# Paramétros del modelo

model_info_divpot <- h2o.getModel(best_model_divpot@model_id)
print(model_info_divpot)

model_performance_divpot <- h2o.performance(best_model_divpot)
print(model_performance_divpot)

summary(best_model_divpot)

model_params_divpot <- best_model_divpot@parameters
print(model_params_divpot)

# Preparar los datos para LIME
train_df <- as.data.frame(ca03_train_h2o)
test_df <- as.data.frame(ca03_test_h2o)

print(names(train_df))
print(names(test_df))

# Función para graficar la importancia de las variables de un modelo base
h2o.varimp_plot(best_model_divpot)

# Extraer la importancia de las variables y convertir en un data frame
best_model_type_divpot <- best_model_divpot@model$model_category
print(best_model_type_divpot)

var_importance_divpot <- h2o.varimp(best_model_divpot)
var_importance_df_divpot <- as.data.frame(var_importance_divpot)

# Verificar el data frame
print(var_importance_df_divpot)

# Crea el gráfico de importancia de variables
plot_var_importance_divpot <- function(var_importance_df_divpot, model_id) {
  ggplot(var_importance_df_divpot, aes(x = reorder(variable, -relative_importance), y = relative_importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Importancia de Variables del Modelo", model_id),
         x = "Variables",
         y = "Importancia Relativa") +
    theme_minimal()
}

# Genera y guarda el gráfico de importancia de variables para el mejor modelo
plot_divpot <- plot_var_importance_divpot(var_importance_df_divpot, best_model_divpot@model_id)
print(plot_divpot)

# Guarda el gráfico como archivo PNG
ggsave(filename = paste("var_importance_", best_model_divpot@model_id, ".png", sep = ""), plot = plot)

# Crear el explainer de LIME utilizando un modelo base
explainer_divpot <- lime(train_df, best_model_divpot) 


# Generar explicaciones utilizando LIME
explanation_divpot <- explain(test_subset, explainer_divpot, n_labels = 1, n_features = 5)
print(explanation_divpot)

# Visualizar las explicaciones
plot_features(explanation_divpot)

# Realizar predicciones en el conjunto de prueba
pred_gbm_divpot <- h2o.predict(best_model_divpot, ca03_test_h2o)

# Convertir las predicciones y el conjunto de prueba a data frames de R
pred_gbm_df_divpot <- as.data.frame(pred_gbm_divpot)
cao_test_df <- as.data.frame(ca03_test_h2o)

# Crear la matriz de confusión
confusionMatrix(as.factor(pred_gbm_df_divpot[, 1]), cao_test_df$segpot)

# Obtener métricas de rendimiento del modelo GBM
perf_gbm_divpot <- h2o.performance(best_model_divpot, newdata = ca03_test_h2o)
print(perf_gbm_divpot)

# Obtener la matriz de confusión
conf_matrix_gbm_divpot <- h2o.confusionMatrix(best_model_divpot, newdata = ca03_test_h2o)
print(conf_matrix_gbm_divpot)


on.exit(h2o.shutdown(), add = TRUE)



# aml marpot

h2o.init()

cao_train_h2o <- as.h2o(cao_train)
cao_test_h2o <- as.h2o(cao_test)

y <- "segmarpot" # Variable objetivo

excluded_vars <- c("segmarpot", "segdivpot", "ID", "divpot", 
                   "marpot", "compot","tcode", "AE", "ptf")

x <- setdiff(names(cao_train_h2o), excluded_vars)

print(x)

# Ejecutar H2O AutoML
aml_marpot <- h2o.automl(x = x, y = y, training_frame = cao_train_h2o, 
                         max_runtime_secs = 3600, seed = 123)

# Ver el resumen de los modelos
summary(aml_marpot)

# Obtener el mejor modelo
best_model_marpot <- h2o.get_best_model(aml_marpot)
model_type_marpot <- class(best_model_marpot)
print(model_type_marpot)

best_model_type_marpot <- best_model_marpot@model$model_category
print(best_model_type_marpot)

# Realizar predicciones en el conjunto de prueba
pred_marpot <- h2o.predict(best_model_marpot, cao_test_h2o)
head(pred_marpot)

# Convertir las predicciones y el conjunto de prueba a data frames de R
pred_marpot_df <- as.data.frame(pred_marpot)
cao_test_df <- as.data.frame(cao_test_h2o)

# Crear la matriz de confusión
cmmarpot <- confusionMatrix(as.factor(pred_marpot_df[, 1]), cao_test_df[, y])
print(cmmarpot)

# Graficar la importancia de las variables del modelo

model_info <- h2o.getModel(best_model_marpot@model_id)
print(model_info)

model_performance <- h2o.performance(best_model_marpot)
print(model_performance)

summary(best_model_marpot)

model_params_marpot <- best_model_marpot@parameters
print(model_params_marpot)

# Función para graficar la importancia de las variables de un modelo base
h2o.varimp_plot(best_model_marpot)

# Extraer la importancia de las variables y convertir en un data frame
var_importance_marpot <- h2o.varimp(best_model_marpot)
var_importance_df_marpot <- as.data.frame(var_importance_marpot)

# Verificar el data frame
print(var_importance_df_marpot)


# Crea el gráfico de importancia de variables
plot_var_importance_marpot <- function(var_importance_df_marpot, model_id) {
  ggplot(var_importance_df_marpot, aes(x = reorder(variable, -relative_importance), y = relative_importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Importancia de Variables del Modelo", model_id),
         x = "Variables",
         y = "Importancia Relativa") +
    theme_minimal()
}

# Genera y guarda el gráfico de importancia de variables para el mejor modelo
plot_marpot <- plot_var_importance_marpot(var_importance_df_marpot, best_model_marpot@model_id)
print(plot_marpot)

# Guarda el gráfico como archivo PNG
ggsave(filename = paste("var_importance_", best_model_divpot@model_id, ".png", sep = ""), plot = plot)


# Obtener la tabla de clasificación del objeto AutoML
leaderboard_marpot <- aml_marpot@leaderboard

# Mostrar la tabla de clasificación
print(leaderboard_marpot)



# Preparar los datos para LIME
train_df_marpot <- as.data.frame(cao_train_h2o)
test_df_marpot <- as.data.frame(cao_test_h2o)

# Verificar que las columnas coinciden
print(all(colnames(train_df_marpot) == colnames(test_df_marpot)))

# Asegurarse de que las columnas de datos de prueba coinciden con las de entrenamiento
test_subset_marpot <- test_df_marpot[1:10, colnames(train_df_marpot)]

# Crear el explainer de LIME utilizando un modelo base
explainer_marpot <- lime(train_df_marpot, best_model_marpot) 

# Verificar consistencia de filas
if (nrow(test_subset_marpot) == nrow(predict(best_model_marpot, as.h2o(test_subset_marpot)))) {
  cat("El número de filas es consistente.\n")
} else {
  cat("El número de filas no es consistente. Revisa los datos.\n")
}

# Generar explicaciones utilizando LIME
explainer_marpot <- lime::lime(test_subset_marpot, best_model_marpot)
explanation_marpot <- lime::explain(test_subset_marpot, explainer_marpot, n_labels = 1, n_features = 5)

explanation_marpot <- explain(test_subset_marpot, explainer_marpot, n_labels = 1, n_features = 5)
print(explanation_marpot)

# Visualizar las explicaciones
plot_features(explanation_marpot)

# Realizar predicciones en el conjunto de prueba
pred_gbm_marpot <- h2o.predict(best_model_marpot, cao_test_h2o)

# Convertir las predicciones y el conjunto de prueba a data frames de R
pred_gbm_df_marpot <- as.data.frame(pred_gbm_marpot)
cao_test_df <- as.data.frame(cao_test_h2o)

# Crear la matriz de confusión
confusionMatrix(as.factor(pred_gbm_df_marpot[, 1]), cao_test_df$segmarpot)

# Obtener métricas de rendimiento del modelo GBM
perf_gbm_marpot <- h2o.performance(best_model_marpot, newdata = cao_test_h2o)
print(perf_gbm_marpot)

# Obtener la matriz de confusión
conf_matrix_gbm_marpot <- h2o.confusionMatrix(best_model_marpot, newdata = cao_test_h2o)
print(conf_matrix_gbm_marpot)


on.exit(h2o.shutdown(), add = TRUE)


# Obtener el directorio de trabajo actual
directorio <- getwd()

# Construir la ruta de las imágenes
ruta_imagen1 <- file.path(directorio, "impdivpot.png")
ruta_imagen2 <- file.path(directorio, "impmarpot.png")

# Leer las imágenes
imagen1 <- image_read(ruta_imagen1)
imagen2 <- image_read(ruta_imagen2)

# Combinar las imágenes lado a lado
combinada <- image_append(c(imagen1, imagen2), stack = FALSE)

# Guardar la imagen combinada
image_write(combinada, path = file.path(directorio, "imagen_combinada.jpg"))

ruta_imagen3 <- file.path(directorio, "expdivpot.png")
ruta_imagen4 <- file.path(directorio, "expmarpot.png")

# Leer las imágenes
imagen3 <- image_read(ruta_imagen3)
imagen4 <- image_read(ruta_imagen4)

# Combinar las imágenes lado a lado
combinada2 <- image_append(c(imagen3, imagen4), stack = FALSE)

# Guardar la imagen combinada
image_write(combinada2, path = file.path(directorio, "imagen_combinada2.jpg"))

