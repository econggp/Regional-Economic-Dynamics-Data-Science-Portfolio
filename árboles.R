
install_github("ramnathv/htmlwidgets", force = TRUE)
install_github("smartinsightsfromdata/rpivotTable", force = TRUE)
if (!requireNamespace("h2o", quietly = TRUE)) {
  install.packages("h2o")
}

library(devtools)
library(rpivotTable)
library(psych)
library(funModeling)
library(pastecs)
library(PerformanceAnalytics)
library(treemapify)
library(treemap)
library(reshape2)
library(ggplot2)
library(gplots)
library(ggcorrplot)
library(ggpubr)
library(rattle)
library(pheatmap)
library(vcd)
library(plyr)
library(gridExtra)
library(car)
library(corrr)
library(caret)
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
library(lime)

dataa <- baseind

tablaman<- rpivotTable(dataa, rows="NOMGEO", col="tcode", aggregatorName="Average", 
                       vals="marppvs")
tablaman

hist(dataa$marppvs, breaks = 30, main = "Distribución de marppvs", xlab = "marppvs")
hist(dataa$divppvs, breaks = 30, main = "Distribución de divppvs", xlab = "divppvs")
hist(dataa$marpacd, breaks = 30, main = "Distribución de marpacd", xlab = "marpacd")
hist(dataa$divpacd, breaks = 30, main = "Distribución de divpacd", xlab = "divpacd")

#data <- data %>% mutate(log_marppvs = ifelse(is.finite(log(marppvs)), log(marppvs), NA),log_marpacd = ifelse(is.finite(log(marpacd)), log(marpacd), NA))

#data <- data %>%mutate(log_marppvs = ifelse(is.na(log_marppvs), mean(log_marppvs, na.rm = TRUE), log_marppvs),log_marpacd = ifelse(is.na(log_marpacd), mean(log_marpacd, na.rm = TRUE), log_marpacd))


lower_bound_marppvsee <- quantile(dataa$marppvs, 0.01)
upper_bound_marppvsee <- quantile(dataa$marppvs, 0.99)

lower_bound_marpacdee <- quantile(dataa$marpacd, 0.01)
upper_bound_marpacdee <- quantile(dataa$marpacd, 0.99)

lower_bound_divppvsee <- quantile(dataa$divppvs, 0.01)
upper_bound_divppvsee <- quantile(dataa$divppvs, 0.99)

lower_bound_divpacdee <- quantile(dataa$divpacd, 0.01)
upper_bound_divpacdee <- quantile(dataa$divpacd, 0.99)

dataa <- dataa %>%
  filter(marppvs >= lower_bound_marppvsee & marppvs <= upper_bound_marppvsee) %>%
  filter(marpacd >= lower_bound_marpacdee & marpacd <= upper_bound_marpacdee) %>%
  filter(divppvs >= lower_bound_divppvsee & divppvs <= upper_bound_divppvsee) %>%
  filter(divpacd >= lower_bound_divpacdee & divpacd <= upper_bound_divpacdee)


# Cálculo de los percentiles
percentiles <- seq(0, 1, by = 0.2)

# Global limits
global_limits_marppvsee <- quantile(dataa$marppvs, probs = percentiles)
global_limits_marpacdee <- quantile(dataa$marpacd, probs = percentiles)
global_limits_divppvsee <- quantile(dataa$divppvs, probs = percentiles)
global_limits_divpacdee <- quantile(dataa$divpacd, probs = percentiles)

# Verificación de los límites globales
print(global_limits_marppvsee)
print(global_limits_marpacdee)
print(global_limits_divppvsee)
print(global_limits_divpacdee)

# Definición manual de las etiquetas para coincidir con los intervalos
labels <- c("P20", "P40", "P60", "P80", "P100")

# Asignación de segmentos con etiquetas correctas
dataa <- dataa %>%
  mutate(
    segmarppvs = cut(marppvs, breaks = global_limits_marppvsee, labels = labels, include.lowest = TRUE),
    segmarpacd = cut(marpacd, breaks = global_limits_marpacdee, labels = labels, include.lowest = TRUE),
    segdivppvs = cut(divppvs, breaks = global_limits_divppvsee, labels = labels, include.lowest = TRUE),
    segdivpacd = cut(divpacd, breaks = global_limits_divpacdee, labels = labels, include.lowest = TRUE)
  )

table(dataa$segmarppvs)
table(dataa$segmarpacd)
table(dataa$segdivppvs)
table(dataa$segdivpacd)



# Resumir datos por estado (NOMGEO)
state_summary <- dataa %>%
  group_by(NOMGEO) %>%
  summarise(
    count = n(),
    mean_marppvs = mean(marppvs, na.rm = TRUE),
    mean_marpacd = mean(marpacd, na.rm = TRUE),
    mean_divppvs = mean(divppvs, na.rm = TRUE),
    mean_divpacd = mean(divpacd, na.rm = TRUE)
  )

print(state_summary)



# Visualizar datos por estado (NOMGEO)
plot_state_marppvs <- ggplot(state_summary, aes(x = NOMGEO, y = mean_marppvs)) +
  geom_bar(stat = "identity") +
  labs(title = "Media de marppvse por Estado", x = "Estado", y = "Media de marppvse") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot_state_marppvs)

quintil_analysis <- function(dataa, variable, segment) {
  dataa %>%
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
quintil_analysis_marppvs <- quintil_analysis(dataa, "marppvs", "segmarppvs")
quintil_analysis_marpacd <- quintil_analysis(dataa, "marpacd", "segmarpacd")
quintil_analysis_divppvs <- quintil_analysis(dataa, "divppvs", "segdivppvs")
quintil_analysis_divpacd <- quintil_analysis(dataa, "divpacd", "segdivpacd")

# Mostrar los resultados
print(quintil_analysis_marppvs)
print(quintil_analysis_marpacd)
print(quintil_analysis_divppvs)
print(quintil_analysis_divpacd)

# Función para visualizar datos por quintil
visualize_quintil <- function(dataa, variable, segment) {
  ggplot(dataa, aes_string(x = segment, y = variable)) +
    geom_boxplot() +
    labs(title = paste("Distribución de", variable, "por Quintil"),
         x = "Quintil",
         y = variable)
}

# Visualizar los datos para cada variable de interés
plot_marppvs <- visualize_quintil(dataa, "marppvs", "segmarppvs")
plot_marpacd <- visualize_quintil(dataa, "marpacd", "segmarpacd")
plot_divppvs <- visualize_quintil(dataa, "divppvs", "segdivppvs")
plot_divpacd <- visualize_quintil(dataa, "divpacd", "segdivpacd")

# Mostrar los gráficos
print(plot_marppvs)
print(plot_marpacd)
print(plot_divppvs)
print(plot_divpacd)


# Función para obtener características por quintil
characteristics_by_quintil <- function(dataa, variable, segment) {
  dataa %>%
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
char_marppvs <- characteristics_by_quintil(dataa, "marppvs", "segmarppvs")
char_marpacd <- characteristics_by_quintil(dataa, "marpacd", "segmarpacd")
char_divppvs <- characteristics_by_quintil(dataa, "divppvs", "segdivppvs")
char_divpacd <- characteristics_by_quintil(dataa, "divpacd", "segdivpacd")

# Mostrar los resultados
print(char_marppvs)
print(char_marpacd)
print(char_divppvs)
print(char_divpacd)

caoo <- select(dataa, tcode, segmarppvs, segmarpacd, 
              segdivppvs, segdivpacd, marppvs:crrpacd, CVE_GEO) 


set.seed(987654321)

trainindex = createDataPartition(caoo$CVE_GEO, p=0.75)$Resample1
caoo_train= caoo[trainindex, ]
caoo_test= caoo[-trainindex, ]

# Árboles de decisión

set.seed(1234567890)


control_poda <- rpart.control(
  cp = 0.026,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)


arbmarppvs <-  rpart(segmarppvs ~ comppvs+compacd+eeppvs+eepacd+prppvs+prpacd+
                       srppvs+srpacd+roa+rat+roic+dca+iaf+mbi+ptf+qrppvs+qrpacd+
                       eeppvsr+eepacdr+marppvsr+marpacdr+comppvsr+compacdr+
                       divppvsr+divpacdr+prppvsr+prpacdr+srppvsr+srpacdr+roar+
                       ratr+roicr+dcar+iafr+mbir+ptfr+qrppvsr+qrpacdr+edjpacd+
                       eejpacd+etjpacd+eijpacd+emjpacd+ermjpacd+edjppvs+eejppvs+
                       etjppvs+eijppvs+emjppvs+ermjppvs+crrppvs+crrpacd,
                      data = caoo_train, method = "class", control = control_poda)
predarbmarppvs <- predict(arbmarppvs, caoo_test, type = "class")
confusionMatrix(predarbmarppvs, caoo_test$segmarppvs)

fancyRpartPlot(arbmarppvs, main = "Clasificación MARPPVS 2003-2018",
               sub = "")


control_poda <- rpart.control(
  cp = 0.015,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

arbmarpacd <-  rpart(segmarpacd ~ comppvs+compacd+eeppvs+eepacd+prppvs+prpacd+
                       srppvs+srpacd+roa+rat+roic+dca+iaf+mbi+ptf+qrppvs+qrpacd+
                       eeppvsr+eepacdr+marppvsr+marpacdr+comppvsr+compacdr+
                       divppvsr+divpacdr+prppvsr+prpacdr+srppvsr+srpacdr+roar+
                       ratr+roicr+dcar+iafr+mbir+ptfr+qrppvsr+qrpacdr+edjpacd+
                       eejpacd+etjpacd+eijpacd+emjpacd+ermjpacd+edjppvs+eejppvs+
                       etjppvs+eijppvs+emjppvs+ermjppvs+crrppvs+crrpacd,
                     data = caoo_train, method = "class", control = control_poda)
predarbmarpacd <- predict(arbmarpacd, caoo_test, type = "class")
confusionMatrix(predarbmarpacd, caoo_test$segmarpacd)

fancyRpartPlot(arbmarpacd, main = "Clasificación MARPACD 2003-2018",
               sub = "")

control_poda <- rpart.control(
  cp = 0.015,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)

arbdivpacd <-  rpart(segdivpacd ~ comppvs+compacd+eeppvs+eepacd+prppvs+prpacd+
                       srppvs+srpacd+roa+rat+roic+dca+iaf+mbi+ptf+qrppvs+qrpacd+
                       eeppvsr+eepacdr+marppvsr+marpacdr+comppvsr+compacdr+
                       divppvsr+divpacdr+prppvsr+prpacdr+srppvsr+srpacdr+roar+
                       ratr+roicr+dcar+iafr+mbir+ptfr+qrppvsr+qrpacdr+edjpacd+
                       eejpacd+etjpacd+eijpacd+emjpacd+ermjpacd+edjppvs+eejppvs+
                       etjppvs+eijppvs+emjppvs+ermjppvs+crrppvs+crrpacd,
                     data = caoo_train, method = "class", control = control_poda)
predarbdivpacd <- predict(arbdivpacd, caoo_test, type = "class")
confusionMatrix(predarbdivpacd, caoo_test$segdivpacd)

fancyRpartPlot(arbdivpacd, main = "Clasificación DIVPACD 2003-2018",
               sub = "")

control_poda <- rpart.control(
  cp = 0.015,
  minsplit = 3,
  minbucket = 3,
  maxdepth = 18,
  xval = 10,
  maxcompete = 4)


arbdivppvs <-  rpart(segdivppvs ~ comppvs+compacd+eeppvs+eepacd+prppvs+prpacd+
                       srppvs+srpacd+roa+rat+roic+dca+iaf+mbi+ptf+qrppvs+qrpacd+
                       eeppvsr+eepacdr+marppvsr+marpacdr+comppvsr+compacdr+
                       divppvsr+divpacdr+prppvsr+prpacdr+srppvsr+srpacdr+roar+
                       ratr+roicr+dcar+iafr+mbir+ptfr+qrppvsr+qrpacdr+edjpacd+
                       eejpacd+etjpacd+eijpacd+emjpacd+ermjpacd+edjppvs+eejppvs+
                       etjppvs+eijppvs+emjppvs+ermjppvs+crrppvs+crrpacd,
                     data = caoo_train, method = "class", control = control_poda)
predarbdivppvs <- predict(arbdivppvs, caoo_test, type = "class")
confusionMatrix(predarbdivppvs, caoo_test$segdivppvs)

fancyRpartPlot(arbdivppvs, main = "Clasificación DIVPPVS 2003-2018",
               sub = "")

