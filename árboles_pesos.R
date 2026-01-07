data <- dataman
data <- na.omit(data)


library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)

tablaman<- rpivotTable(data, rows="Entidad", col="AE", aggregatorName="Average", 
                   vals="ATAF")

tablaman

library(psych)
library(funModeling)
library(pastecs)
library(PerformanceAnalytics)
library(treemapify)
library(treemap)
library(ggplot2)
library(gplots)
library(ggcorrplot)
library(ggpubr)
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



c2003 <- filter(data, AC == 2003)  
c2008 <- filter(data, AC == 2008)  
c2013 <- filter(data, AC == 2013)  
c2018 <- filter(data, AC == 2018)  

c2003$AC <- NULL
c2008$AC <- NULL
c2013$AC <- NULL
c2018$AC <- NULL


psych::describe(c2018$VACB)

c2018$segmentovacb <- as.factor(cut(c2018$VACB, breaks=c(-13147, 70, 616, 5857, 300023), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))



ca <- select(c2018, segmentovacb, ID, Entidad, AE, pijataf:pbt_cag) %>% column_to_rownames(., var = 'ID')

agu <- filter(ca, Entidad %in% c("AGU"))  
bca <- filter(ca, Entidad %in% c("BCA"))
bcs <- filter(ca, Entidad %in% c("BCS"))
cam <- filter(ca, Entidad %in% c("CAM"))
coa <- filter(ca, Entidad %in% c("COA"))
col <- filter(ca, Entidad %in% c("COL"))
chi <- filter(ca, Entidad %in% c("CHI"))
chh <- filter(ca, Entidad %in% c("CHH"))
cmx <- filter(ca, Entidad %in% c("CMX"))
dur <- filter(ca, Entidad %in% c("DUR"))
gua <- filter(ca, Entidad %in% c("GUA"))
gue <- filter(ca, Entidad %in% c("GUE"))
hid <- filter(ca, Entidad %in% c("HID"))
jal <- filter(ca, Entidad %in% c("JAL"))
emx <- filter(ca, Entidad %in% c("EMX"))
mic <- filter(ca, Entidad %in% c("MIC"))
mor <- filter(ca, Entidad %in% c("MOR"))
nay <- filter(ca, Entidad %in% c("NAY"))
nle <- filter(ca, Entidad %in% c("NLE"))
oax <- filter(ca, Entidad %in% c("OAX"))
pue <- filter(ca, Entidad %in% c("PUE"))
que <- filter(ca, Entidad %in% c("QUE"))
qro <- filter(ca, Entidad %in% c("QRO"))
slp <- filter(ca, Entidad %in% c("SLP"))
sin <- filter(ca, Entidad %in% c("SIN"))
son <- filter(ca, Entidad %in% c("SON"))
tab <- filter(ca, Entidad %in% c("TAB"))
tam <- filter(ca, Entidad %in% c("TAM"))
tla <- filter(ca, Entidad %in% c("TLA"))
ver <- filter(ca, Entidad %in% c("VER"))
yuc <- filter(ca, Entidad %in% c("YUC"))
zac <- filter(ca, Entidad %in% c("ZAC"))

agu$Entidad <- NULL
bca$Entidad <- NULL
bcs$Entidad <- NULL
cam$Entidad <- NULL
chh$Entidad <- NULL
chi$Entidad <- NULL
cmx$Entidad <- NULL
coa$Entidad <- NULL
col$Entidad <- NULL
dur$Entidad <- NULL
emx$Entidad <- NULL
gua$Entidad <- NULL
gue$Entidad <- NULL
hid$Entidad <- NULL
jal$Entidad <- NULL
mic$Entidad <- NULL
mor$Entidad <- NULL
nay$Entidad <- NULL
nle$Entidad <- NULL
oax$Entidad <- NULL
pue$Entidad <- NULL
qro$Entidad <- NULL
que$Entidad <- NULL
sin$Entidad <- NULL
son$Entidad <- NULL
slp$Entidad <- NULL
tab$Entidad <- NULL
tam$Entidad <- NULL
tla$Entidad <- NULL
ver$Entidad <- NULL
yuc$Entidad <- NULL
zac$Entidad <- NULL



set.seed(987654321)

trainindex = createDataPartition(agu$pbt_fbcf, p=0.75)$Resample1
agu_train= agu[trainindex, ]
agu_test= agu[-trainindex, ]

trainindex = createDataPartition(bca$pbt_fbcf, p=0.75)$Resample1
bca_train= bca[trainindex, ]
bca_test= bca[-trainindex, ]

trainindex = createDataPartition(bcs$pbt_fbcf, p=0.75)$Resample1
bcs_train= bcs[trainindex, ]
bcs_test= bcs[-trainindex, ]

trainindex = createDataPartition(cam$pbt_fbcf, p=0.75)$Resample1
cam_train= cam[trainindex, ]
cam_test= cam[-trainindex, ]

trainindex = createDataPartition(coa$pbt_fbcf, p=0.75)$Resample1
coa_train= coa[trainindex, ]
coa_test= coa[-trainindex, ]

trainindex = createDataPartition(col$pbt_fbcf, p=0.75)$Resample1
col_train= col[trainindex, ]
col_test= col[-trainindex, ]

trainindex = createDataPartition(chi$pbt_fbcf, p=0.75)$Resample1
chi_train= chi[trainindex, ]
chi_test= chi[-trainindex, ]

trainindex = createDataPartition(chh$pbt_fbcf, p=0.75)$Resample1
chh_train= chh[trainindex, ]
chh_test= chh[-trainindex, ]

trainindex = createDataPartition(cmx$pbt_fbcf, p=0.75)$Resample1
cmx_train= cmx[trainindex, ]
cmx_test= cmx[-trainindex, ]

trainindex = createDataPartition(dur$pbt_fbcf, p=0.75)$Resample1
dur_train= dur[trainindex, ]
dur_test= dur[-trainindex, ]

trainindex = createDataPartition(gua$pbt_fbcf, p=0.75)$Resample1
gua_train= gua[trainindex, ]
gua_test= gua[-trainindex, ]

trainindex = createDataPartition(gue$pbt_fbcf, p=0.75)$Resample1
gue_train= gue[trainindex, ]
gue_test= gue[-trainindex, ]

trainindex = createDataPartition(hid$pbt_fbcf, p=0.75)$Resample1
hid_train= hid[trainindex, ]
hid_test= hid[-trainindex, ]

trainindex = createDataPartition(jal$pbt_fbcf, p=0.75)$Resample1
jal_train= jal[trainindex, ]
jal_test= jal[-trainindex, ]

trainindex = createDataPartition(emx$pbt_fbcf, p=0.75)$Resample1
emx_train= emx[trainindex, ]
emx_test= emx[-trainindex, ]

trainindex = createDataPartition(mic$pbt_fbcf, p=0.75)$Resample1
mic_train= mic[trainindex, ]
mic_test= mic[-trainindex, ]

trainindex = createDataPartition(mor$pbt_fbcf, p=0.75)$Resample1
mor_train= mor[trainindex, ]
mor_test= mor[-trainindex, ]

trainindex = createDataPartition(nay$pbt_fbcf, p=0.75)$Resample1
nay_train= nay[trainindex, ]
nay_test= nay[-trainindex, ]

trainindex = createDataPartition(nle$pbt_fbcf, p=0.75)$Resample1
nle_train= nle[trainindex, ]
nle_test= nle[-trainindex, ]

trainindex = createDataPartition(oax$pbt_fbcf, p=0.75)$Resample1
oax_train= oax[trainindex, ]
oax_test= oax[-trainindex, ]

trainindex = createDataPartition(pue$pbt_fbcf, p=0.75)$Resample1
pue_train= pue[trainindex, ]
pue_test= pue[-trainindex, ]

trainindex = createDataPartition(que$pbt_fbcf, p=0.75)$Resample1
que_train= que[trainindex, ]
que_test= que[-trainindex, ]

trainindex = createDataPartition(qro$pbt_fbcf, p=0.75)$Resample1
qro_train= qro[trainindex, ]
qro_test= qro[-trainindex, ]

trainindex = createDataPartition(slp$pbt_fbcf, p=0.75)$Resample1
slp_train= slp[trainindex, ]
slp_test= slp[-trainindex, ]

trainindex = createDataPartition(sin$pbt_fbcf, p=0.75)$Resample1
sin_train= sin[trainindex, ]
sin_test= sin[-trainindex, ]

trainindex = createDataPartition(son$pbt_fbcf, p=0.75)$Resample1
son_train= son[trainindex, ]
son_test= son[-trainindex, ]

trainindex = createDataPartition(tab$pbt_fbcf, p=0.75)$Resample1
tab_train= tab[trainindex, ]
tab_test= tab[-trainindex, ]

trainindex = createDataPartition(tam$pbt_fbcf, p=0.75)$Resample1
tam_train= tam[trainindex, ]
tam_test= tam[-trainindex, ]

trainindex = createDataPartition(tla$pbt_fbcf, p=0.75)$Resample1
tla_train= tla[trainindex, ]
tla_test= tla[-trainindex, ]

trainindex = createDataPartition(ver$pbt_fbcf, p=0.75)$Resample1
ver_train= ver[trainindex, ]
ver_test= ver[-trainindex, ]

trainindex = createDataPartition(yuc$pbt_fbcf, p=0.75)$Resample1
yuc_train= yuc[trainindex, ]
yuc_test= yuc[-trainindex, ]

trainindex = createDataPartition(zac$pbt_fbcf, p=0.75)$Resample1
zac_train= zac[trainindex, ]
zac_test= zac[-trainindex, ]


# Árboles de decisión

library(rpart)
library(rpart.plot)
library(rattle)
library(tree)


set.seed(1234567890)



# Entrenamiento del árbol de clasificación
arbol_clasificacion <- rpart(segmentovacb ~ ., data = agu_train, minsplit = 2, minbucket = 1, cp = 0)

# Validación cruzada para determinar el tamaño óptimo de poda
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(agu_train$segmentovacb)
cv_results <- caret::train(x = agu_train[, -ncol(agu_train)], y = agu_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))

size_optimo <- cv_results$bestTune$cp

# Entrenamiento del árbol podado
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adagu <- rpart(segmentovacb ~ ., data = agu_train, method = "class", control = control_poda)

# Gráfico del árbol podado
fancyRpartPlot(adagu, main = "Clasificación del VACB en Aguascalientes",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

# Predicciones en datos de prueba
predat <- predict(adagu, agu_test, type = "class")

# Matriz de confusión
confusionMatrix(predat, agu_test$segmentovacb)




arbol_clasificacion <- rpart(segmentovacb ~ ., data = bca_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bca_train$segmentovacb)
cv_results <- caret::train(x = bca_train[, -ncol(bca_train)], y = bca_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbca <- rpart(segmentovacb ~ ., data = bca_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adbca, main = "Clasificación del VACB en Baja California",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adbca, bca_test, type = "class")
confusionMatrix(predat, bca_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = bcs_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bcs_train$segmentovacb)
cv_results <- caret::train(x = bcs_train[, -ncol(bcs_train)], y = bcs_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbcs <- rpart(segmentovacb ~ ., data = bcs_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adbcs, main = "Clasificación del VACB en Baja California Sur",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adbcs, bcs_test, type = "class")
confusionMatrix(predat, bcs_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = cam_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cam_train$segmentovacb)
cv_results <- caret::train(x = cam_train[, -ncol(cam_train)], y = cam_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcam <- rpart(segmentovacb ~ ., data = cam_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcam, main = "Clasificación del VACB en Campeche",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcam, cam_test, type = "class")
confusionMatrix(predat, cam_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = chh_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chh_train$segmentovacb)
cv_results <- caret::train(x = chh_train[, -ncol(chh_train)], y = chh_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchh <- rpart(segmentovacb ~ ., data = chh_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adchh, main = "Clasificación del VACB en Chihuahua",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adchh, chh_test, type = "class")
confusionMatrix(predat, chh_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = chi_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chi_train$segmentovacb)
cv_results <- caret::train(x = chi_train[, -ncol(chi_train)], y = chi_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchi <- rpart(segmentovacb ~ .-pijataf, data = chi_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adchi, main = "Clasificación del VACB en Chiapas",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adchi, chi_test, type = "class")
confusionMatrix(predat, chi_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = cmx_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cmx_train$segmentovacb)
cv_results <- caret::train(x = cmx_train[, -ncol(cmx_train)], y = cmx_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcmx <- rpart(segmentovacb ~ ., data = cmx_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcmx, main = "Clasificación del VACB en la Ciudad de México",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcmx, cmx_test, type = "class")
confusionMatrix(predat, cmx_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = coa_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(coa_train$segmentovacb)
cv_results <- caret::train(x = coa_train[, -ncol(coa_train)], y = coa_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcoa <- rpart(segmentovacb ~ ., data = coa_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcoa, main = "Clasificación del VACB en Coahuila",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcoa, coa_test, type = "class")
confusionMatrix(predat, coa_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = col_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(col_train$segmentovacb)
cv_results <- caret::train(x = col_train[, -ncol(col_train)], y = col_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcol <- rpart(segmentovacb ~ .-pbt_htpot-vacb_htpot-fbcf_htpot, data = col_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcol, main = "Clasificación del VACB en Colima",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcol, col_test, type = "class")
confusionMatrix(predat, col_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = dur_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(dur_train$segmentovacb)
cv_results <- caret::train(x = dur_train[, -ncol(dur_train)], y = dur_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
addur <- rpart(segmentovacb ~ ., data = dur_train, method = "class", 
               control = control_poda)
fancyRpartPlot(addur, main = "Clasificación del VACB en Durango",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(addur, dur_test, type = "class")
confusionMatrix(predat, dur_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = emx_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(emx_train$segmentovacb)
cv_results <- caret::train(x = emx_train[, -ncol(emx_train)], y = emx_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
ademx <- rpart(segmentovacb ~ ., data = emx_train, method = "class", 
               control = control_poda)
fancyRpartPlot(ademx, main = "Clasificación del VACB en el Estado de México",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(ademx, emx_test, type = "class")
confusionMatrix(predat, emx_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = gua_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gua_train$segmentovacb)
cv_results <- caret::train(x = gua_train[, -ncol(gua_train)], y = gua_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgua <- rpart(segmentovacb ~ ., data = gua_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adgua, main = "Clasificación del VACB en Guanajuato",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adgua, gua_test, type = "class")
confusionMatrix(predat, gua_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = gue_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gue_train$segmentovacb)
cv_results <- caret::train(x = gue_train[, -ncol(gue_train)], y = gue_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgue <- rpart(segmentovacb ~ ., data = gue_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adgue, main = "Clasificación del VACB en Guerrero",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adgue, gue_test, type = "class")
confusionMatrix(predat, gue_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = hid_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(hid_train$segmentovacb)
cv_results <- caret::train(x = hid_train[, -ncol(hid_train)], y = hid_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adhid <- rpart(segmentovacb ~ ., data = hid_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adhid, main = "Clasificación del VACB en Hidalgo",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adhid, hid_test, type = "class")
confusionMatrix(predat, hid_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = jal_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(jal_train$segmentovacb)
cv_results <- caret::train(x = jal_train[, -ncol(jal_train)], y = jal_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adjal <- rpart(segmentovacb ~ ., data = jal_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adjal, main = "Clasificación del VACB en Jalisco",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adjal, jal_test, type = "class")
confusionMatrix(predat, jal_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = mic_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mic_train$segmentovacb)
cv_results <- caret::train(x = mic_train[, -ncol(mic_train)], y = mic_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admic <- rpart(segmentovacb ~ ., data = mic_train, method = "class", 
               control = control_poda)
fancyRpartPlot(admic, main = "Clasificación del VACB en Michoacán",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(admic, mic_test, type = "class")
confusionMatrix(predat, mic_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = mor_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mor_train$segmentovacb)
cv_results <- caret::train(x = mor_train[, -ncol(mor_train)], y = mor_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admor <- rpart(segmentovacb ~ ., data = mor_train, method = "class", 
               control = control_poda)
fancyRpartPlot(admor, main = "Clasificación del VACB en Morelos",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(admor, mor_test, type = "class")
confusionMatrix(predat, mor_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = nay_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nay_train$segmentovacb)
cv_results <- caret::train(x = nay_train[, -ncol(nay_train)], y = nay_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnay <- rpart(segmentovacb ~ .-fbcf_tsppvs-pbt_tsppvs-fbcf_dtaf, data = nay_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adnay, main = "Clasificación del VACB en Nayarit",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adnay, nay_test, type = "class")
confusionMatrix(predat, nay_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = nle_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nle_train$segmentovacb)
cv_results <- caret::train(x = nle_train[, -ncol(nle_train)], y = nle_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnle <- rpart(segmentovacb ~ ., data = nle_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adnle, main = "Clasificación del VACB en Nuevo León",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adnle, nle_test, type = "class")
confusionMatrix(predat, nle_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = oax_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(oax_train$segmentovacb)
cv_results <- caret::train(x = oax_train[, -ncol(oax_train)], y = oax_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adoax <- rpart(segmentovacb ~ ., data = oax_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adoax, main = "Clasificación del VACB en Oaxaca",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adoax, oax_test, type = "class")
confusionMatrix(predat, oax_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = pue_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(pue_train$segmentovacb)
cv_results <- caret::train(x = pue_train[, -ncol(pue_train)], y = pue_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adpue <- rpart(segmentovacb ~ ., data = pue_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adpue, main = "Clasificación del VACB en Puebla",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adpue, pue_test, type = "class")
confusionMatrix(predat, pue_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = que_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(que_train$segmentovacb)
cv_results <- caret::train(x = que_train[, -ncol(que_train)], y = que_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adque <- rpart(segmentovacb ~ ., data = que_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adque, main = "Clasificación del VACB en Querétaro",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adque, que_test, type = "class")
confusionMatrix(predat, que_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = qro_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(qro_train$segmentovacb)
cv_results <- caret::train(x = qro_train[, -ncol(qro_train)], y = qro_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adqro <- rpart(segmentovacb ~ ., data = qro_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adqro, main = "Clasificación del VACB en Quintana Roo",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adqro, qro_test, type = "class")
confusionMatrix(predat, qro_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = slp_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(slp_train$segmentovacb)
cv_results <- caret::train(x = slp_train[, -ncol(slp_train)], y = slp_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adslp <- rpart(segmentovacb ~ ., data = slp_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adslp, main = "Clasificación del VACB en San Luis Potosi",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adslp, slp_test, type = "class")
confusionMatrix(predat, slp_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = sin_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(sin_train$segmentovacb)
cv_results <- caret::train(x = sin_train[, -ncol(sin_train)], y = sin_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adsin <- rpart(segmentovacb ~ ., data = sin_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adsin, main = "Clasificación del VACB en Sinaloa",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adsin, sin_test, type = "class")
confusionMatrix(predat, sin_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = son_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(son_train$segmentovacb)
cv_results <- caret::train(x = son_train[, -ncol(son_train)], y = son_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adson <- rpart(segmentovacb ~ ., data = son_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adson, main = "Clasificación del VACB en Sonora",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adson, son_test, type = "class")
confusionMatrix(predat, son_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = tab_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tab_train$segmentovacb)
cv_results <- caret::train(x = tab_train[, -ncol(tab_train)], y = tab_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtab <- rpart(segmentovacb ~ ., data = tab_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtab, main = "Clasificación del VACB en Tabasco",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtab, tab_test, type = "class")
confusionMatrix(predat, tab_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = tam_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tam_train$segmentovacb)
cv_results <- caret::train(x = tam_train[, -ncol(tam_train)], y = tam_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtam <- rpart(segmentovacb ~ ., data = tam_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtam, main = "Clasificación del VACB en Tamaulipas",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtam, tam_test, type = "class")
confusionMatrix(predat, tam_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = tla_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tla_train$segmentovacb)
cv_results <- caret::train(x = tla_train[, -ncol(tla_train)], y = tla_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtla <- rpart(segmentovacb ~ ., data = tla_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtla, main = "Clasificación del VACB en Tlaxcala",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtla, tla_test, type = "class")
confusionMatrix(predat, tla_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = ver_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(ver_train$segmentovacb)
cv_results <- caret::train(x = ver_train[, -ncol(ver_train)], y = ver_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adver <- rpart(segmentovacb ~ ., data = ver_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adver, main = "Clasificación del VACB en Veracruz",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adver, ver_test, type = "class")
confusionMatrix(predat, ver_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = yuc_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(yuc_train$segmentovacb)
cv_results <- caret::train(x = yuc_train[, -ncol(yuc_train)], y = yuc_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adyuc <- rpart(segmentovacb ~ ., data = yuc_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adyuc, main = "Clasificación del VACB en Yucatán",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adyuc, yuc_test, type = "class")
confusionMatrix(predat, yuc_test$segmentovacb)



arbol_clasificacion <- rpart(segmentovacb ~ ., data = zac_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(zac_train$segmentovacb)
cv_results <- caret::train(x = zac_train[, -ncol(zac_train)], y = zac_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adzac <- rpart(segmentovacb ~ ., data = zac_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adzac, main = "Clasificación del VACB en Zacatecas",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adzac, zac_test, type = "class")
confusionMatrix(predat, zac_test$segmentovacb)



impadagu <- varImp(adagu)
impadbca <- varImp(adbca)
impadbcs <- varImp(adbcs)
impadcam <- varImp(adcam)
impadchh <- varImp(adchh)
impadchi <- varImp(adchi)
impadcmx <- varImp(adcmx)
impadcoa <- varImp(adcoa)
impadcol <- varImp(adcol)
impaddur <- varImp(addur)
impademx <- varImp(ademx)
impadgua <- varImp(adgua)
impadgue <- varImp(adgue)
impadhid <- varImp(adhid)
impadjal <- varImp(adjal)
impadmic <- varImp(admic)
impadmor <- varImp(admor)
impadnay <- varImp(adnay)
impadnle <- varImp(adnle)
impadoax <- varImp(adoax)
impadpue <- varImp(adpue)
impadqro <- varImp(adqro)
impadque <- varImp(adque)
impadsin <- varImp(adsin)
impadslp <- varImp(adslp)
impadson <- varImp(adson)
impadtab <- varImp(adtab)
impadtam <- varImp(adtam)
impadtla <- varImp(adtla)
impadver <- varImp(adver)
impadyuc <- varImp(adyuc)
impadzac <- varImp(adzac)


divadagu <- adagu$split
divadbca <- adbca$split
divadbcs <- adbcs$split
divadcam <- adcam$split
divadchh <- adchh$split
divadchi <- adchi$split
divadcmx <- adcmx$split
divadcoa <- adcoa$split
divadcol <- adcol$split
divaddur <- addur$split
divademx <- ademx$split
divadgua <- adgua$split
divadgue <- adgue$split
divadhid <- adhid$split
divadjal <- adjal$split
divadmic <- admic$split
divadmor <- admor$split
divadnay <- adnay$split
divadnle <- adnle$split
divadoax <- adoax$split
divadpue <- adpue$split
divadqro <- adqro$split
divadque <- adque$split
divadsin <- adsin$split
divadslp <- adslp$split
divadson <- adson$split
divadtab <- adtab$split
divadtam <- adtam$split
divadtla <- adtla$split
divadver <- adver$split
divadyuc <- adyuc$split
divadzac <- adzac$split

agu0<-impadagu 
bca0<-impadbca 
bcs0<-impadbcs 
cam0<-impadcam 
chh0<-impadchh
chi0<-impadchi 
cmx0<-impadcmx 
coa0<-impadcoa 
col0<-impadcol
dur0<-impaddur 
emx0<-impademx 
gua0<-impadgua 
gue0<-impadgue 
hid0<-impadhid
jal0<-impadjal 
mic0<-impadmic 
mor0<-impadmor 
nay0<-impadnay 
nle0<-impadnle
oax0<-impadoax 
pue0<-impadpue 
qro0<-impadqro 
que0<-impadque 
sin0<-impadsin
son0<-impadson 
slp0<-impadslp 
tab0<-impadtab 
tam0<-impadtam 
tla0<-impadtla
ver0<-impadver 
yuc0<-impadyuc 
zac0<-impadzac



d1 <- data.frame(agu0, bca0, bcs0, cam0, chh0,
                 cmx0, coa0, 
                 dur0, emx0, gua0,  gue0, hid0,
                 jal0, mic0, mor0,  nle0,
                 oax0, pue0, qro0, que0, sin0,
                 son0, slp0, tab0, tam0, tla0,
                 ver0, yuc0, zac0)
names(d1) <- c("AGU", "BCA", "BCS", "CAM", "CHH", "CMX", "COA", "DUR",
               "EMX", "GUA", "GUE", "HID", "JAL", "MIC", "MOR", "NLE", "OAX",
               "PUE", "QRO", "QUE", "SIN", "SON", "SLP", "TAB", "TAM",
               "TLA", "VER", "YUC", "ZAC")


dt1 <- t(d1)



d2 <- data.frame(chi0)
names(d2) <- c("CHI")
nuevas_filas <- data.frame(CHI = 0)
d2 <- rbind(d2, nuevas_filas)
dt2 <- t(d2)

d3 <- data.frame(col0, nay0)
names(d3) <- c("COL", "NAY")
nuevas_filas <- data.frame(COL = c(0, 0,0),
                           NAY = c(0, 0, 0))
d3 <- rbind(d3, nuevas_filas)
dt3 <- t(d3)

dc <- rbind(dt3, dt2, dt1)
dcc <- as.data.frame(dc)


#Conglomerados

library(ade4)

acp <- dudi.pca(dcc,
                scannf=F, 
                scale=T, 
                nf=7)

library(factoextra)

fviz_pca_biplot(acp, repel = FALSE,
                col.var = "steelblue",
                col.ind = "black" )

plot_num(dcc)

con <- select(dcc, AE:pbt_rabmi)
con <- na.omit(con)

dis <- dist(con, method = "maximum")

res.dist <- get_dist(con, stand = FALSE, 
                     method = "euclidean")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))


library(corrplot)

corrplot(as.matrix(dis), is.corr = FALSE, 
         method = "color",
         order = "hclust", type = "upper")

d <- as.matrix(dis)

heatmap(d, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas de las Entidades")

res.hc <- hclust(dist(con), "ward.D" )

fviz_dend(res.hc, cex = 0.5, k = 5, rect = TRUE)


grp <- cutree(res.hc, k = 5)

fviz_cluster(list(data = con, cluster = grp),
             ellipse.type = "convex", 
             repel = T, show.clust.cent = T, ggtheme = theme_minimal())

library(ape)

plot(as.phylo(res.hc), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc), type = "unrooted", cex = 0.9,
     no.margin = TRUE)

library(igraph)

fviz_dend(res.hc, k=5, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc, cex= 0.5, k=5, k_colors="jco",
          type="circular")

plot(as.phylo(res.hc), type = "radial")

