# install.packages("h2o", repos = "https://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")

library(car)
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
library(patchwork)
library(gplots)
library(ggcorrplot)
library(corrplot)
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
library(purrr)
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

hist(data$marpot, breaks = 30, main = "Distribución de marpot", xlab = "marppvs")
hist(data$divpot, breaks = 30, main = "Distribución de divpot", xlab = "divppvs")
hist(data$compot, breaks = 30, main = "Distribución de divpot", xlab = "divppvs")
hist(data$pot, breaks = 30, main = "Distribución de pot", xlab = "POT")

data$pot[is.na(data$pot)] <- median(data$pot, na.rm = TRUE)

data$pot_log <- log1p(data$pot)

summary(data$pot)

# Data frame para los modelos

cao <- select(data, tcode, AE, ID, pot:cem) 

ca2003 <- filter(cao, tcode == 2003)  
ca2008 <- filter(cao, tcode == 2008)  
ca2013 <- filter(cao, tcode == 2013)  
ca2018 <- filter(cao, tcode == 2018)

ca2003$tcode <- NULL
ca2008$tcode <- NULL
ca2013$tcode <- NULL
ca2018$tcode <- NULL


# Segmentar
ca2003$segpot <- cut(
  ca2003$pot,
  breaks = c(0, 4630, 11858, 29989, Inf),
  labels = c("Bajo", "Medio", "Alto-Medio", "Alto"),
  include.lowest = TRUE
)

ggplot(ca2003, aes(x = segpot)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución Segmentación",
       x = "Rango de Potencia",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(ca2003$segpot)

ca2008$segpot <- cut(
  ca2008$pot,
  breaks = c(0, 4630, 11858, 29989, Inf),
  labels = c("Bajo", "Medio", "Alto-Medio", "Alto"),
  include.lowest = TRUE
)

ggplot(ca2008, aes(x = segpot)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución Segmentación",
       x = "Rango de Potencia",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(ca2008$segpot)

ca2013$segpot <- cut(
  ca2013$pot,
  breaks = c(0, 4630, 11858, 29989, Inf),
  labels = c("Bajo", "Medio", "Alto-Medio", "Alto"),
  include.lowest = TRUE
)

ggplot(ca2013, aes(x = segpot)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución Segmentación",
       x = "Rango de Potencia",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(ca2013$segpot)

ca2018$segpot <- cut(
  ca2018$pot,
  breaks = c(0, 4630, 11858, 29989, Inf),
  labels = c("Bajo", "Medio", "Alto-Medio", "Alto"),
  include.lowest = TRUE
)

ggplot(ca2018, aes(x = segpot)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución Segmentación",
       x = "Rango de Potencia",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(ca2018$segpot)

ca2003ae <- ca2003 %>%
  group_by(AE) %>%
  group_split()

nombres_sectores <- ca2003 %>%
  distinct(AE) %>%
  pull()
names(ca2003ae) <- nombres_sectores

ca2008ae <- ca2008 %>%
  group_by(AE) %>%
  group_split()

nombres_sectores <- ca2008 %>%
  distinct(AE) %>%
  pull()
names(ca2008ae) <- nombres_sectores

ca2013ae <- ca2013 %>%
  group_by(AE) %>%
  group_split()

nombres_sectores <- ca2013 %>%
  distinct(AE) %>%
  pull()
names(ca2013ae) <- nombres_sectores

ca2018ae <- ca2018 %>%
  group_by(AE) %>%
  group_split()

nombres_sectores <- ca2018 %>%
  distinct(AE) %>%
  pull()
names(ca2018ae) <- nombres_sectores

# Particiones 

set.seed(123456)

particiones03 <- ca2003 %>%
  group_by(AE) %>%
  group_map(~ {
    indices <- sample(1:nrow(.x), size = 0.8 * nrow(.x))
    list(train = .x[indices, ], test = .x[-indices, ])
  })

# Asignar nombres
names(particiones03) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector03 <- map(particiones03, "train")
test_por_sector03 <- map(particiones03, "test")

particiones08 <- ca2008 %>%
  group_by(AE) %>%
  group_map(~ {
    indices <- sample(1:nrow(.x), size = 0.8 * nrow(.x))
    list(train = .x[indices, ], test = .x[-indices, ])
  })

# Asignar nombres
names(particiones08) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector08 <- map(particiones08, "train")
test_por_sector08 <- map(particiones08, "test")

particiones13 <- ca2013 %>%
  group_by(AE) %>%
  group_map(~ {
    indices <- sample(1:nrow(.x), size = 0.8 * nrow(.x))
    list(train = .x[indices, ], test = .x[-indices, ])
  })

# Asignar nombres
names(particiones13) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector13 <- map(particiones13, "train")
test_por_sector13 <- map(particiones13, "test")

particiones18 <- ca2018 %>%
  group_by(AE) %>%
  group_map(~ {
    indices <- sample(1:nrow(.x), size = 0.8 * nrow(.x))
    list(train = .x[indices, ], test = .x[-indices, ])
  })

# Asignar nombres
names(particiones18) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector18 <- map(particiones18, "train")
test_por_sector18 <- map(particiones18, "test")



#Automated Machine learning

library(h2o)
h2o.init()

cao_train03_h2o <- as.h2o(train_por_sector03)
cao_test03_h2o <- as.h2o(test_por_sector03)

cao_train08_h2o <- as.h2o(train_por_sector08)
cao_test08_h2o <- as.h2o(test_por_sector08)

cao_train13_h2o <- as.h2o(train_por_sector13)
cao_test13_h2o <- as.h2o(test_por_sector13)

cao_train18_h2o <- as.h2o(train_por_sector18)
cao_test18_h2o <- as.h2o(test_por_sector18)


# aml pot

exclude_cols03<- c("ID", "AE", "ptf", "ptfm", "ctm", "cem", "pot")
exclude_cols08<- c("ID", "AE", "ptf", "ptfm", "pot")
exclude_cols13<- c("ID", "AE", "ptf", "ptfm", "pot")
exclude_cols18<- c("ID", "AE", "ptf", "ptfm", "pot")


predictors03 <- setdiff(names(cao_train03_h2o), c(exclude_cols03))  # Predictores
predictors08 <- setdiff(names(cao_train08_h2o), c(exclude_cols08))  # Predictores
predictors13 <- setdiff(names(cao_train13_h2o), c(exclude_cols13))  # Predictores
predictors18 <- setdiff(names(cao_train18_h2o), c(exclude_cols18))  # Predictores

target <- "ptf"  # Variable objetivo

target_columns <- grep("\\.ptf$", names(cao_train03_h2o), value = TRUE)

print(target_columns)

hyper_params <- list(
  gbm = list(max_depth = 10, learn_rate = 0.01),
  xgboost = list(eta = 0.1, gamma = 0.5)
)

# Entrenar modelo con AutoML
# Lista para almacenar modelos
models_ptf03 <- list()

for (ptf_target in target_columns) {
  cat("\nEntrenando modelo para:", ptf_target, "\n")
  
  models_ptf03[[ptf_target]] <- h2o.automl(
    x = predictors03,                # Predictores
    y = ptf_target,                  # Una variable ptf a la vez
    training_frame = cao_train03_h2o,
    nfolds = 5,
    max_models = 10,                 # Reducir para mayor velocidad
    seed = 123
  )
  
  # Ver el líder actual
  print(models_ptf03[[ptf_target]]@leaderboard)
}








aml08 <- h2o.automl(
  x = predictors08,
  y = target,
  training_frame = cao_train08_h2o,
  nfolds = 5,
  exploitation_ratio = 0.5,
  keep_cross_validation_predictions = TRUE,
  exclude_algos = c("StackedEnsemble"),
  max_models = 20,
  max_runtime_secs = 600,
  seed = 123    
)

aml13 <- h2o.automl(
  x = predictors13,
  y = target,
  training_frame = cao_train13_h2o,
  nfolds = 5,
  exploitation_ratio = 0.5,
  keep_cross_validation_predictions = TRUE,
  exclude_algos = c("StackedEnsemble"),
  max_models = 20,
  max_runtime_secs = 600,
  seed = 123     
)

aml18 <- h2o.automl(
  x = predictors18,
  y = target,
  training_frame = cao_train18_h2o,
  nfolds = 5,
  exploitation_ratio = 0.5,
  keep_cross_validation_predictions = TRUE,
  exclude_algos = c("StackedEnsemble"),
  max_models = 20,
  max_runtime_secs = 600,
  seed = 123 
)

# Mostrar ranking de modelos
lb03 <- aml03@leaderboard
print(lb03)

lb08 <- aml08@leaderboard
print(lb08)

lb13 <- aml13@leaderboard
print(lb13)

lb18 <- aml18@leaderboard
print(lb18)

# Obtener el modelo líder
leader_model03 <- aml03@leader

leader_model08 <- aml08@leader

leader_model13 <- aml13@leader

leader_model18 <- aml18@leader

# Evaluar el modelo en test
pred03 <- h2o.predict(leader_model03, cao_train03_h2o)
pred08 <- h2o.predict(leader_model08, cao_train08_h2o)
pred13 <- h2o.predict(leader_model13, cao_train13_h2o)
pred18 <- h2o.predict(leader_model18, cao_train18_h2o)

# Convertir predicciones a dataframe para visualización
pred_df03 <- as.data.frame(pred03)
pred_df08 <- as.data.frame(pred08)
pred_df13 <- as.data.frame(pred13)
pred_df18 <- as.data.frame(pred18)

# Gráfico de barras de distribución de segmentos

ggplot(pred_df03, aes(x = predict)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Segmentos 03", 
       x = "Segmento", y = "Cantidad") +
  theme_minimal()

ggplot(pred_df08, aes(x = predict)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Segmentos 08", 
       x = "Segmento", y = "Cantidad") +
  theme_minimal()

ggplot(pred_df13, aes(x = predict)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Segmentos 13", 
       x = "Segmento", y = "Cantidad") +
  theme_minimal()

ggplot(pred_df18, aes(x = predict)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Segmentos 18", 
       x = "Segmento", y = "Cantidad") +
  theme_minimal()

train_with_pred03 <- h2o.cbind(cao_train03_h2o, pred03)
train_with_pred08 <- h2o.cbind(cao_train08_h2o, pred08)
train_with_pred13 <- h2o.cbind(cao_train13_h2o, pred13)
train_with_pred18 <- h2o.cbind(cao_train18_h2o, pred18)

train_with_pred_df03 <- as.data.frame(train_with_pred03)
train_with_pred_df08 <- as.data.frame(train_with_pred08)
train_with_pred_df13 <- as.data.frame(train_with_pred13)
train_with_pred_df18 <- as.data.frame(train_with_pred18)

ggplot(train_with_pred_df03, aes(x = pot, fill = predict)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~predict) +
  labs(title = "Distribución por Segmento 03") +
  theme_minimal()

ggplot(train_with_pred_df08, aes(x = segpot, fill = predict)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~predict) +
  labs(title = "Distribución por Segmento 08") +
  theme_minimal()

ggplot(train_with_pred_df13, aes(x = segpot, fill = predict)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~predict) +
  labs(title = "Distribución por Segmento 13") +
  theme_minimal()

ggplot(train_with_pred_df18, aes(x = segpot, fill = predict)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~predict) +
  labs(title = "Distribución por Segmento 18") +
  theme_minimal()

# Explicación global del modelo

conf_matrix03 <- h2o.confusionMatrix(leader_model03, cao_train03_h2o)
conf_matrix08 <- h2o.confusionMatrix(leader_model08, cao_train08_h2o)
conf_matrix13 <- h2o.confusionMatrix(leader_model13, cao_train13_h2o)
conf_matrix18 <- h2o.confusionMatrix(leader_model18, cao_train18_h2o)

print(conf_matrix03)
print(conf_matrix08)
print(conf_matrix13)
print(conf_matrix18)

confusion_data03 <- as.data.frame.matrix(conf_matrix03[-nrow(conf_matrix03), -ncol(conf_matrix03)])

confusionPlot03 <- as.data.frame.table(as.matrix(confusion_data03))
colnames(confusionPlot03) <- c("Actual", "Predicted", "Freq")
head(confusionPlot03)

ggplot(confusionPlot03, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matriz de Confusión - Segmentación Ocupacional 2003",
       x = "Predicho",
       y = "Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

confusion_data08 <- as.data.frame.matrix(conf_matrix08[-nrow(conf_matrix08), -ncol(conf_matrix08)])

confusionPlot08 <- as.data.frame.table(as.matrix(confusion_data08))
colnames(confusionPlot08) <- c("Actual", "Predicted", "Freq")
head(confusionPlot08)

ggplot(confusionPlot08, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matriz de Confusión - Segmentación Ocupacional 2008",
       x = "Predicho",
       y = "Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

confusion_data13 <- as.data.frame.matrix(conf_matrix13[-nrow(conf_matrix13), -ncol(conf_matrix13)])

confusionPlot13 <- as.data.frame.table(as.matrix(confusion_data13))
colnames(confusionPlot13) <- c("Actual", "Predicted", "Freq")
head(confusionPlot13)

ggplot(confusionPlot13, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matriz de Confusión - Segmentación Ocupacional 2013",
       x = "Predicho",
       y = "Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

confusion_data18 <- as.data.frame.matrix(conf_matrix18[-nrow(conf_matrix18), -ncol(conf_matrix18)])

confusionPlot18 <- as.data.frame.table(as.matrix(confusion_data18))
colnames(confusionPlot18) <- c("Actual", "Predicted", "Freq")
head(confusionPlot18)

ggplot(confusionPlot18, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matriz de Confusión - Segmentación Ocupacional 2018",
       x = "Predicho",
       y = "Real") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# Métricas clave
cat("RMSE:", h2o.rmse(performance03), "\n")
cat("MAE:", h2o.mae(performance03), "\n")
cat("R²:", h2o.r2(performance03), "\n")

cat("RMSE:", h2o.rmse(performance08), "\n")
cat("MAE:", h2o.mae(performance08), "\n")
cat("R²:", h2o.r2(performance08), "\n")

cat("RMSE:", h2o.rmse(performance13), "\n")
cat("MAE:", h2o.mae(performance13), "\n")
cat("R²:", h2o.r2(performance13), "\n")

cat("RMSE:", h2o.rmse(performance18), "\n")
cat("MAE:", h2o.mae(performance18), "\n")
cat("R²:", h2o.r2(performance18), "\n")

# Importancia de variables
var_imp03 <- h2o.varimp(leader_model03)
print(var_imp03)

var_imp08 <- h2o.varimp(leader_model08)
print(var_imp08)

var_imp13 <- h2o.varimp(leader_model13)
print(var_imp13)

var_imp18 <- h2o.varimp(leader_model18)
print(var_imp18)

# Gráfico de importancia (top 10)
h2o.varimp_plot(leader_model03, num_of_features = 16)

h2o.varimp_plot(leader_model08, num_of_features = 16)

h2o.varimp_plot(leader_model13, num_of_features = 16)

h2o.varimp_plot(leader_model18, num_of_features = 16)

# Guardar modelo para producción (opcional)

model_path03 <- h2o.saveModel(leader_model03, path = getwd(), force = TRUE)
cat("Modelo guardado en:", model_path03, "\n")

model_path08 <- h2o.saveModel(leader_model08, path = getwd(), force = TRUE)
cat("Modelo guardado en:", model_path08, "\n")

model_path13 <- h2o.saveModel(leader_model13, path = getwd(), force = TRUE)
cat("Modelo guardado en:", model_path13, "\n")

model_path18 <- h2o.saveModel(leader_model18, path = getwd(), force = TRUE)
cat("Modelo guardado en:", model_path18, "\n")

# Cerrar sesión de H2O (al finalizar)
h2o.shutdown()
