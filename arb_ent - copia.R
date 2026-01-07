install.packages("h2o", repos = "https://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")

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

data[["pacd"]] <- replace(data[["pacd"]], is.na(data[["pacd"]]), 0)
data[["ppvs"]] <- replace(data[["ppvs"]], is.na(data[["ppvs"]]), 0)
data[["pot"]] <- replace(data[["pot"]], is.na(data[["pot"]]), 0)

summary(data$pot)

# Data frame para los modelos

cao <- select(data, NOMGEO, tcode, AE, itec:compacd, pot) 

ca2003 <- filter(cao, tcode == 2003)  
ca2008 <- filter(cao, tcode == 2008)  
ca2013 <- filter(cao, tcode == 2013)  
ca2018 <- filter(cao, tcode == 2018)

ca2003$tcode <- NULL
ca2003$ptfm <- NULL
ca2003$ctm <- NULL
ca2003$cem <- NULL
ca2008$tcode <- NULL
ca2008$ptf <- NULL
ca2013$tcode <- NULL
ca2013$ptf <- NULL
ca2018$tcode <- NULL
ca2018$ptf <- NULL

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

ca2003$pot <- NULL
ca2008$pot <- NULL
ca2013$pot <- NULL
ca2018$pot <- NULL


# Datos agrupados por sector

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

particiones03 <- map(ca2003ae, ~ {
  set.seed(123456)  # Para reproducibilidad
  indices <- sample(1:nrow(.x), size = floor(0.75 * nrow(.x)))
  list(
    train = .x[indices, ],
    test = .x[-indices, ]
  )
})

# Nombres de los sectores se mantienen
names(particiones03) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector03 <- map(particiones03, "train")
test_por_sector03 <- map(particiones03, "test")

particiones08 <- map(ca2008ae, ~ {
  set.seed(123456)  # Para reproducibilidad
  indices <- sample(1:nrow(.x), size = floor(0.75 * nrow(.x)))
  list(
    train = .x[indices, ],
    test = .x[-indices, ]
  )
})

# Asignar nombres
names(particiones08) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector08 <- map(particiones08, "train")
test_por_sector08 <- map(particiones08, "test")

particiones13 <- map(ca2013ae, ~ {
  set.seed(123456)  # Para reproducibilidad
  indices <- sample(1:nrow(.x), size = floor(0.75 * nrow(.x)))
  list(
    train = .x[indices, ],
    test = .x[-indices, ]
  )
})

# Asignar nombres
names(particiones13) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector13 <- map(particiones13, "train")
test_por_sector13 <- map(particiones13, "test")

particiones18 <- map(ca2018ae, ~ {
  set.seed(123456)  # Para reproducibilidad
  indices <- sample(1:nrow(.x), size = floor(0.75 * nrow(.x)))
  list(
    train = .x[indices, ],
    test = .x[-indices, ]
  )
})

# Asignar nombres
names(particiones18) <- nombres_sectores

# Extraer conjuntos train y test
train_por_sector18 <- map(particiones18, "train")
test_por_sector18 <- map(particiones18, "test")



# Automated Machine learning

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


# aml ptf o ptfm

# Entrenar modelo con AutoML

# Sectores 2003

exclude_cols03<- c( "NOMGEO","AE", "segpot", "ptf")

predictors03 <- setdiff(names(cao_train03_h2o), c(exclude_cols03)) 

target <- "ptf"  # Variable objetivo

target_columns <- grep("\\.ptf$", names(cao_train03_h2o), value = TRUE)

print(target_columns)

models_ptf03 <- list()

for (ptf_target in target_columns) {
  cat("\nEntrenando modelo 2003 para:", ptf_target, "\n")
  
  # Entrenar AutoML excluyendo modelos de ensamble
  models_ptf03[[ptf_target]] <- h2o.automl(
    x = predictors03,
    y = ptf_target,
    training_frame = cao_train03_h2o,
    nfolds = 5,
    exploitation_ratio = 0.5,
    keep_cross_validation_predictions = TRUE,
    include_algos = c("DRF", "GBM", "XGBoost", "GLM"), # Solo modelos con importancia de variables
    max_models = 10,
    seed = 123  
  )
  
  # Ver el líder actual
  print(models_ptf03[[ptf_target]]@leaderboard)
  
  # Obtener el modelo líder
  leader_model03 <- models_ptf03[[ptf_target]]@leader
  
  # Verificar que el modelo es de un tipo que soporta importancia de variables
  if (leader_model03@algorithm %in% c("drf", "gbm", "xgboost")) {
    
    # Extraer importancia de variables
    var_imp <- h2o.varimp(leader_model03)
    
    cat("\nImportancia de variables 2003 para", ptf_target, ":\n")
    print(head(var_imp, 10))  # Mostrar solo las 10 más importantes
    
    # Visualización gráfica
    if (nrow(var_imp) > 0) {
      var_imp_df <- as.data.frame(var_imp)
      var_imp_df <- var_imp_df[order(-var_imp_df$scaled_importance), ]
      var_imp_df$variable <- factor(var_imp_df$variable, 
                                    levels = var_imp_df$variable[order(var_imp_df$scaled_importance)])
      
      # Gráfico de barras horizontales
      library(ggplot2)
      p <- ggplot(head(var_imp_df, 10), aes(x = reorder(variable, scaled_importance), 
                                            y = scaled_importance)) +
        geom_col(fill = "#1f77b4") +
        coord_flip() +
        labs(title = paste("Top 10 variables 2003 para:", ptf_target),
             subtitle = paste("Modelo:", leader_model03@algorithm),
             x = "",
             y = "Importancia escalada") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      print(p)
      
      # Guardar el gráfico
      ggsave(paste0("var_importance_", ptf_target, ".png"), 
             plot = p, width = 8, height = 6, dpi = 300)
    }
    
  } else {
    cat("\nEl modelo líder (", leader_model03@algorithm, 
        ") no es DRF, GBM o XGBoost, no se muestra importancia de variables.\n")
  }
}


# Sectores 2008

exclude_cols08<- c( "NOMGEO","AE", "segpot")

predictors08 <- setdiff(names(cao_train08_h2o), c(exclude_cols08))

target <- "ptfm"  # Variable objetivo

target_columns08 <- grep("\\.ptfm$", names(cao_train08_h2o), value = TRUE)

print(target_columns08)

models_ptf08 <- list()

for (ptfm_target in target_columns08) {
  cat("\nEntrenando modelo 2008 para:", ptfm_target, "\n")
  
  # Entrenar AutoML excluyendo modelos de ensamble
  models_ptf08[[ptfm_target]] <- h2o.automl(
    x = predictors08,
    y = ptfm_target,
    training_frame = cao_train08_h2o,
    nfolds = 5,
    exploitation_ratio = 0.5,
    keep_cross_validation_predictions = TRUE,
    include_algos = c("DRF", "GBM", "XGBoost", "GLM"), # Solo modelos con importancia de variables
    max_models = 10,
    seed = 123
  )
  
  # Ver el líder actual
  print(models_ptf08[[ptfm_target]]@leaderboard)
  
  # Obtener el modelo líder
  leader_model08 <- models_ptf08[[ptfm_target]]@leader
  
  # Verificar que el modelo es de un tipo que soporta importancia de variables
  if (leader_model08@algorithm %in% c("drf", "gbm", "xgboost")) {
    
    # Extraer importancia de variables
    var_imp <- h2o.varimp(leader_model08)
    
    cat("\nImportancia de variables 2008 para", ptfm_target, ":\n")
    print(head(var_imp, 10))  # Mostrar solo las 10 más importantes
    
    # Visualización gráfica
    if (nrow(var_imp) > 0) {
      var_imp_df <- as.data.frame(var_imp)
      var_imp_df <- var_imp_df[order(-var_imp_df$scaled_importance), ]
      var_imp_df$variable <- factor(var_imp_df$variable, 
                                    levels = var_imp_df$variable[order(var_imp_df$scaled_importance)])
      
      # Gráfico de barras horizontales
      library(ggplot2)
      p <- ggplot(head(var_imp_df, 10), aes(x = reorder(variable, scaled_importance), 
                                            y = scaled_importance)) +
        geom_col(fill = "#1f77b4") +
        coord_flip() +
        labs(title = paste("Top 10 variables 2008 para:", ptfm_target),
             subtitle = paste("Modelo:", leader_model08@algorithm),
             x = "",
             y = "Importancia escalada") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      print(p)
      
      # Guardar el gráfico (opcional)
      ggsave(paste0("var_importance_", ptfm_target, ".png"), 
             plot = p, width = 8, height = 6, dpi = 300)
    }
    
  } else {
    cat("\nEl modelo líder (", leader_model08@algorithm, 
        ") no es DRF, GBM o XGBoost, no se muestra importancia de variables.\n")
  }
}


# Sectores 2013

exclude_cols13<- c( "AE", "ptfm")

predictors13 <- setdiff(names(cao_train13_h2o), c(exclude_cols13))

target <- "ptfm"  # Variable objetivo

target_columns13 <- grep("\\.ptfm$", names(cao_train13_h2o), value = TRUE)

print(target_columns13)

models_ptf13 <- list()

for (ptfm_target in target_columns13) {
  cat("\nEntrenando modelo 2013 para:", ptfm_target, "\n")
  
  # Entrenar AutoML excluyendo modelos de ensamble
  models_ptf13[[ptfm_target]] <- h2o.automl(
    x = predictors13,
    y = ptfm_target,
    training_frame = cao_train13_h2o,
    nfolds = 5,
    exploitation_ratio = 0.5,
    keep_cross_validation_predictions = TRUE,
    include_algos = c("DRF", "GBM", "XGBoost", "GLM"), # Solo modelos con importancia de variables
    max_models = 10,
    seed = 123
  )
  
  # Ver el líder actual
  print(models_ptf13[[ptfm_target]]@leaderboard)
  
  # Obtener el modelo líder
  leader_model13 <- models_ptf13[[ptfm_target]]@leader
  
  # Verificar que el modelo es de un tipo que soporta importancia de variables
  if (leader_model13@algorithm %in% c("drf", "gbm", "xgboost")) {
    
    # Extraer importancia de variables
    var_imp <- h2o.varimp(leader_model13)
    
    cat("\nImportancia de variables 2013 para", ptfm_target, ":\n")
    print(head(var_imp, 10))  # Mostrar solo las 10 más importantes
    
    # Visualización gráfica
    if (nrow(var_imp) > 0) {
      var_imp_df <- as.data.frame(var_imp)
      var_imp_df <- var_imp_df[order(-var_imp_df$scaled_importance), ]
      var_imp_df$variable <- factor(var_imp_df$variable, 
                                    levels = var_imp_df$variable[order(var_imp_df$scaled_importance)])
      
      # Gráfico de barras horizontales
      library(ggplot2)
      p <- ggplot(head(var_imp_df, 10), aes(x = reorder(variable, scaled_importance), 
                                            y = scaled_importance)) +
        geom_col(fill = "#1f77b4") +
        coord_flip() +
        labs(title = paste("Top 10 variables 2013 para:", ptfm_target),
             subtitle = paste("Modelo:", leader_model13@algorithm),
             x = "",
             y = "Importancia escalada") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      print(p)
      
      # Guardar el gráfico (opcional)
      ggsave(paste0("var_importance_", ptfm_target, ".png"), 
             plot = p, width = 8, height = 6, dpi = 300)
    }
    
  } else {
    cat("\nEl modelo líder (", leader_model13@algorithm, 
        ") no es DRF, GBM o XGBoost, no se muestra importancia de variables.\n")
  }
}


# Sectores 2018

exclude_cols18<- c( "AE", "ptfm")

predictors18 <- setdiff(names(cao_train18_h2o), c(exclude_cols18))

target <- "ptfm"  # Variable objetivo

target_columns18 <- grep("\\.ptfm$", names(cao_train18_h2o), value = TRUE)

print(target_columns18)

models_ptf18 <- list()

for (ptfm_target in target_columns18) {
  cat("\nEntrenando modelo 2018 para:", ptfm_target, "\n")
  
  # Entrenar AutoML excluyendo modelos de ensamble
  models_ptf18[[ptfm_target]] <- h2o.automl(
    x = predictors18,
    y = ptfm_target,
    training_frame = cao_train18_h2o,
    nfolds = 5,
    exploitation_ratio = 0.5,
    keep_cross_validation_predictions = TRUE,
    include_algos = c("DRF", "GBM", "XGBoost"), # Solo modelos con importancia de variables
    max_models = 10,
    seed = 123
  )
  
  # Ver el líder actual
  print(models_ptf18[[ptfm_target]]@leaderboard)
  
  # Obtener el modelo líder
  leader_model18 <- models_ptf18[[ptfm_target]]@leader
  
  # Verificar que el modelo es de un tipo que soporta importancia de variables
  if (leader_model18@algorithm %in% c("drf", "gbm", "xgboost")) {
    
    # Extraer importancia de variables
    var_imp <- h2o.varimp(leader_model18)
    
    cat("\nImportancia de variables 2018 para", ptfm_target, ":\n")
    print(head(var_imp, 10))  # Mostrar solo las 10 más importantes
    
    # Visualización gráfica
    if (nrow(var_imp) > 0) {
      var_imp_df <- as.data.frame(var_imp)
      var_imp_df <- var_imp_df[order(-var_imp_df$scaled_importance), ]
      var_imp_df$variable <- factor(var_imp_df$variable, 
                                    levels = var_imp_df$variable[order(var_imp_df$scaled_importance)])
      
      # Gráfico de barras horizontales
      library(ggplot2)
      p <- ggplot(head(var_imp_df, 10), aes(x = reorder(variable, scaled_importance), 
                                            y = scaled_importance)) +
        geom_col(fill = "#1f77b4") +
        coord_flip() +
        labs(title = paste("Top 10 variables 2018 para:", ptfm_target),
             subtitle = paste("Modelo:", leader_model18@algorithm),
             x = "",
             y = "Importancia escalada") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5))
      
      print(p)
      
      # Guardar el gráfico (opcional)
      ggsave(paste0("var_importance_", ptfm_target, ".png"), 
             plot = p, width = 8, height = 6, dpi = 300)
    }
    
  } else {
    cat("\nEl modelo líder (", leader_model18@algorithm, 
        ") no es DRF, GBM o XGBoost, no se muestra importancia de variables.\n")
  }
}



# Obtener el modelo líder
leader_model03 

leader_model08 

leader_model13 

leader_model18 

# Evaluar el modelo en test
pred03 <- h2o.predict(leader_model03, cao_test03_h2o)
pred08 <- h2o.predict(leader_model08, cao_test08_h2o)
pred13 <- h2o.predict(leader_model13, cao_test13_h2o)
pred18 <- h2o.predict(leader_model18, cao_test18_h2o)

# Convertir predicciones a dataframe
pred_df03 <- as.data.frame(pred03)
pred_df08 <- as.data.frame(pred08)
pred_df13 <- as.data.frame(pred13)
pred_df18 <- as.data.frame(pred18)



# Guardar modelo para producción 

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
Y
