data <- datasec

library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)

tablasec<- rpivotTable(data, rows="AC", col="AE", aggregatorName="Average", 
                   vals="ATAF")

tablasec

library(ade4)
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
library(ggrepel)
library(scales)
library(labelled)
library(janitor)
library(sjPlot)
library(sjmisc)
library(visdat)
library(naniar)
library(GPArotation)
library(lavaan)
library(corrplot)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(ranger)
library(xgboost)
library(e1071)
library(polycor)
library(DandEFA) 
library(factoextra)
library(igraph)
library(tidygraph)
library(ggraph)
library(mclust)
library(cluster)
library(ape)
library(vegan)



c2003 <- filter(data, AC == 2003)  
c2008 <- filter(data, AC == 2008)  
c2013 <- filter(data, AC == 2013)  
c2018 <- filter(data, AC == 2018)  

c2003$AC <- NULL
c2008$AC <- NULL
c2013$AC <- NULL
c2018$AC <- NULL


psych::describe(c2018$VACB)

c2018$segmentovacb <- as.factor(cut(c2018$VACB, breaks=c(-13147, 68.55, 688, 8383.24, 506455), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))


ca <- select(c2018, ID, EF, pijataf:segmentovacb) %>% column_to_rownames(., var = 'ID')

ca <- ca[complete.cases(ca$pbt_ataf), ]


# Análisis factorial exploratorio

set.seed(345678)

ca18 <- select(ca, pijataf:pbt_it)

mat_cor_ca18 <- hetcor(ca18)$correlations #matriz de correlación policorica

ggcorrplot(mat_cor_ca18,type="lower",hc.order = T)

cortest.bartlett(mat_cor_ca18)->p_esf

p_esf$p

KMO(mat_cor_ca18)

scree(mat_cor_ca18)

fa.parallel(mat_cor_ca18, n.obs=9000, fa="fa", fm="paf")

modelo_ca18<-fa(ca18,rotate = "promax",
                nfactors = 15,fm="paf")

fa.diagram(modelo_ca18, digits=2, 
           main="Cargas factoriales de las ratios", 
           adj = 2)

print(modelo_ca18$loadings,cut=0)

modelo_ca18$uniquenesses

modelo_ca18$communalities

dandelion(modelo_ca18$loadings,bound = 0.5,mcex = c(-1,1),
          palet = c("red","blue"))

cor.plot(modelo_ca18,
         main="Mapa de Calor de los pesos factoriales e índices", 
         diag=TRUE,number=F, 
         show.legend = TRUE)

ca180 <- na.omit(ca18)


acp18 <- dudi.pca(ca180,
                scannf=F, 
                scale=T, 
                nf=12)

fviz_pca_var(acp18)

(contrib <- acp18$co*acp18$co)

contrib18 <- as.matrix(contrib)

corrplot(contrib18,is.corr=F)



# Árboles de decisión

set.seed(1234567890)

agu <- filter(ca, EF %in% c("AGU"))  
bca <- filter(ca, EF %in% c("BCA"))
bcs <- filter(ca, EF %in% c("BCS"))
cam <- filter(ca, EF %in% c("CAM"))
coa <- filter(ca, EF %in% c("COA"))
col <- filter(ca, EF %in% c("COL"))
chi <- filter(ca, EF %in% c("CHI"))
chh <- filter(ca, EF %in% c("CHH"))
cmx <- filter(ca, EF %in% c("CMX"))
dur <- filter(ca, EF %in% c("DUR"))
gua <- filter(ca, EF %in% c("GUA"))
gue <- filter(ca, EF %in% c("GUE"))
hid <- filter(ca, EF %in% c("HID"))
jal <- filter(ca, EF %in% c("JAL"))
emx <- filter(ca, EF %in% c("EMX"))
mic <- filter(ca, EF %in% c("MIC"))
mor <- filter(ca, EF %in% c("MOR"))
nay <- filter(ca, EF %in% c("NAY"))
nle <- filter(ca, EF %in% c("NLE"))
oax <- filter(ca, EF %in% c("OAX"))
pue <- filter(ca, EF %in% c("PUE"))
que <- filter(ca, EF %in% c("QUE"))
qro <- filter(ca, EF %in% c("QRO"))
slp <- filter(ca, EF %in% c("SLP"))
sin <- filter(ca, EF %in% c("SIN"))
son <- filter(ca, EF %in% c("SON"))
tab <- filter(ca, EF %in% c("TAB"))
tam <- filter(ca, EF %in% c("TAM"))
tla <- filter(ca, EF %in% c("TLA"))
ver <- filter(ca, EF %in% c("VER"))
yuc <- filter(ca, EF %in% c("YUC"))
zac <- filter(ca, EF %in% c("ZAC"))

agu$EF <- NULL
bca$EF <- NULL
bcs$EF <- NULL
cam$EF <- NULL
chh$EF <- NULL
chi$EF <- NULL
cmx$EF <- NULL
coa$EF <- NULL
col$EF <- NULL
dur$EF <- NULL
emx$EF <- NULL
gua$EF <- NULL
gue$EF <- NULL
hid$EF <- NULL
jal$EF <- NULL
mic$EF <- NULL
mor$EF <- NULL
nay$EF <- NULL
nle$EF <- NULL
oax$EF <- NULL
pue$EF <- NULL
qro$EF <- NULL
que$EF <- NULL
sin$EF <- NULL
son$EF <- NULL
slp$EF <- NULL
tab$EF <- NULL
tam$EF <- NULL
tla$EF <- NULL
ver$EF <- NULL
yuc$EF <- NULL
zac$EF <- NULL



# División de las bases de datos, entrenamiento y prueba

trainindex = createDataPartition(agu$pbt_ataf, p=0.75, list = FALSE)
agu_train= agu[trainindex, ]
agu_test= agu[-trainindex, ]

trainindex = createDataPartition(bca$pbt_ataf, p=0.75, list = FALSE)
bca_train= bca[trainindex, ]
bca_test= bca[-trainindex, ]

trainindex = createDataPartition(bcs$pbt_ataf, p=0.75, list = FALSE)
bcs_train= bcs[trainindex, ]
bcs_test= bcs[-trainindex, ]

trainindex = createDataPartition(cam$pbt_ataf, p=0.75, list = FALSE)
cam_train= cam[trainindex, ]
cam_test= cam[-trainindex, ]

trainindex = createDataPartition(coa$pbt_ataf, p=0.75, list = FALSE)
coa_train= coa[trainindex, ]
coa_test= coa[-trainindex, ]

trainindex = createDataPartition(col$pbt_ataf, p=0.75, list = FALSE)
col_train= col[trainindex, ]
col_test= col[-trainindex, ]

trainindex = createDataPartition(chi$pbt_ataf, p=0.75, list = FALSE)
chi_train= chi[trainindex, ]
chi_test= chi[-trainindex, ]

trainindex = createDataPartition(chh$pbt_ataf, p=0.75, list = FALSE)
chh_train= chh[trainindex, ]
chh_test= chh[-trainindex, ]

trainindex = createDataPartition(cmx$pbt_ataf, p=0.75, list = FALSE)
cmx_train= cmx[trainindex, ]
cmx_test= cmx[-trainindex, ]

trainindex = createDataPartition(dur$pbt_ataf, p=0.75, list = FALSE)
dur_train= dur[trainindex, ]
dur_test= dur[-trainindex, ]

trainindex = createDataPartition(gua$pbt_ataf, p=0.75, list = FALSE)
gua_train= gua[trainindex, ]
gua_test= gua[-trainindex, ]

trainindex = createDataPartition(gue$pbt_ataf, p=0.75, list = FALSE)
gue_train= gue[trainindex, ]
gue_test= gue[-trainindex, ]

trainindex = createDataPartition(hid$pbt_ataf, p=0.75, list = FALSE)
hid_train= hid[trainindex, ]
hid_test= hid[-trainindex, ]

trainindex = createDataPartition(jal$pbt_ataf, p=0.75, list = FALSE)
jal_train= jal[trainindex, ]
jal_test= jal[-trainindex, ]

trainindex = createDataPartition(emx$pbt_ataf, p=0.75, list = FALSE)
emx_train= emx[trainindex, ]
emx_test= emx[-trainindex, ]

trainindex = createDataPartition(mic$pbt_ataf, p=0.75, list = FALSE)
mic_train= mic[trainindex, ]
mic_test= mic[-trainindex, ]

trainindex = createDataPartition(mor$pbt_ataf, p=0.75, list = FALSE)
mor_train= mor[trainindex, ]
mor_test= mor[-trainindex, ]

trainindex = createDataPartition(nay$pbt_ataf, p=0.75, list = FALSE)
nay_train= nay[trainindex, ]
nay_test= nay[-trainindex, ]

trainindex = createDataPartition(nle$pbt_ataf, p=0.75, list = FALSE)
nle_train= nle[trainindex, ]
nle_test= nle[-trainindex, ]

trainindex = createDataPartition(oax$pbt_ataf, p=0.75, list = FALSE)
oax_train= oax[trainindex, ]
oax_test= oax[-trainindex, ]

trainindex = createDataPartition(pue$pbt_ataf, p=0.75, list = FALSE)
pue_train= pue[trainindex, ]
pue_test= pue[-trainindex, ]

trainindex = createDataPartition(que$pbt_ataf, p=0.75, list = FALSE)
que_train= que[trainindex, ]
que_test= que[-trainindex, ]

trainindex = createDataPartition(qro$pbt_ataf, p=0.75, list = FALSE)
qro_train= qro[trainindex, ]
qro_test= qro[-trainindex, ]

trainindex = createDataPartition(slp$pbt_ataf, p=0.75, list = FALSE)
slp_train= slp[trainindex, ]
slp_test= slp[-trainindex, ]

trainindex = createDataPartition(sin$pbt_ataf, p=0.75, list = FALSE)
sin_train= sin[trainindex, ]
sin_test= sin[-trainindex, ]

trainindex = createDataPartition(son$pbt_ataf, p=0.75, list = FALSE)
son_train= son[trainindex, ]
son_test= son[-trainindex, ]

trainindex = createDataPartition(tab$pbt_ataf, p=0.75, list = FALSE)
tab_train= tab[trainindex, ]
tab_test= tab[-trainindex, ]

trainindex = createDataPartition(tam$pbt_ataf, p=0.75, list = FALSE)
tam_train= tam[trainindex, ]
tam_test= tam[-trainindex, ]

trainindex = createDataPartition(tla$pbt_ataf, p=0.75, list = FALSE)
tla_train= tla[trainindex, ]
tla_test= tla[-trainindex, ]

trainindex = createDataPartition(ver$pbt_ataf, p=0.75, list = FALSE)
ver_train= ver[trainindex, ]
ver_test= ver[-trainindex, ]

trainindex = createDataPartition(yuc$pbt_ataf, p=0.75, list = FALSE)
yuc_train= yuc[trainindex, ]
yuc_test= yuc[-trainindex, ]

trainindex = createDataPartition(zac$pbt_ataf, p=0.75, list = FALSE)
zac_train= zac[trainindex, ]
zac_test= zac[-trainindex, ]

# Entrenamiento del árbol de clasificación
arbol_clasificacion_agu <- rpart(segmentovacb ~ ., data = agu_train, minsplit = 2, minbucket = 1, cp = 0)

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
fancyRpartPlot(adagu, main = "Clasificación del VACB en Aguascalientes año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

# Predicciones en datos de prueba
predat <- predict(adagu, agu_test, type = "class")

# Matriz de confusión
confusionMatrix(predat, agu_test$segmentovacb)

iagu <- adagu$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Aguascalientes")

iagu


arbol_clasificacion_bca <- rpart(segmentovacb ~ ., data = bca_train, minsplit = 2, 
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
fancyRpartPlot(adbca, main = "Clasificación del VACB en Baja California año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adbca, bca_test, type = "class")
confusionMatrix(predat, bca_test$segmentovacb)

ibca <- adbca$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Baja California")

ibca



arbol_clasificacion_bcs <- rpart(segmentovacb ~ ., data = bcs_train, minsplit = 2, 
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
fancyRpartPlot(adbcs, main = "Clasificación del VACB en Baja California Sur año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adbcs, bcs_test, type = "class")
confusionMatrix(predat, bcs_test$segmentovacb)

ibcs <- adbcs$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Baja California Sur")

ibcs



arbol_clasificacion_cam <- rpart(segmentovacb ~ ., data = cam_train, minsplit = 2, 
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
fancyRpartPlot(adcam, main = "Clasificación del VACB en Campeche año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcam, cam_test, type = "class")
confusionMatrix(predat, cam_test$segmentovacb)

icam <- adcam$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Campeche")

icam



arbol_clasificacion_chh <- rpart(segmentovacb ~ ., data = chh_train, minsplit = 2, 
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
fancyRpartPlot(adchh, main = "Clasificación del VACB en Chihuahua año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adchh, chh_test, type = "class")
confusionMatrix(predat, chh_test$segmentovacb)

ichh <- adchh$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Chihuahua")

ichh



arbol_clasificacion_chi <- rpart(segmentovacb ~ ., data = chi_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chi_train$segmentovacb)
cv_results <- caret::train(x = chi_train[, -ncol(chi_train)], y = chi_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchi <- rpart(segmentovacb ~ ., data = chi_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adchi, main = "Clasificación del VACB en Chiapas año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adchi, chi_test, type = "class")
confusionMatrix(predat, chi_test$segmentovacb)

ichi <- adchi$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Chiapas")

ichi



arbol_clasificacion_cmx <- rpart(segmentovacb ~ ., data = cmx_train, minsplit = 2, 
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
fancyRpartPlot(adcmx, main = "Clasificación del VACB en la Ciudad de México año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcmx, cmx_test, type = "class")
confusionMatrix(predat, cmx_test$segmentovacb)

icmx <- adcmx$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Ciudad de México")

icmx



arbol_clasificacion_coa <- rpart(segmentovacb ~ ., data = coa_train, minsplit = 2, 
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
fancyRpartPlot(adcoa, main = "Clasificación del VACB en Coahuila año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcoa, coa_test, type = "class")
confusionMatrix(predat, coa_test$segmentovacb)

icoa <- adcoa$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Coahuila")

icoa



arbol_clasificacion_col <- rpart(segmentovacb ~ ., data = col_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(col_train$segmentovacb)
cv_results <- caret::train(x = col_train[, -ncol(col_train)], y = col_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcol <- rpart(segmentovacb ~ ., data = col_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcol, main = "Clasificación del VACB en Colima año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adcol, col_test, type = "class")
confusionMatrix(predat, col_test$segmentovacb)

icol <- adcol$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Colima")

icol




arbol_clasificacion_dur <- rpart(segmentovacb ~ ., data = dur_train, minsplit = 2, 
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
fancyRpartPlot(addur, main = "Clasificación del VACB en Durango año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(addur, dur_test, type = "class")
confusionMatrix(predat, dur_test$segmentovacb)

idur <- addur$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Durango")

idur



arbol_clasificacion_emx <- rpart(segmentovacb ~ ., data = emx_train, minsplit = 2, 
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
fancyRpartPlot(ademx, main = "Clasificación del VACB en el Estado de México año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(ademx, emx_test, type = "class")
confusionMatrix(predat, emx_test$segmentovacb)

iemx <- ademx$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Estado de México")

iemx



arbol_clasificacion_gua <- rpart(segmentovacb ~ ., data = gua_train, minsplit = 2, 
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
fancyRpartPlot(adgua, main = "Clasificación del VACB en Guanajuato año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adgua, gua_test, type = "class")
confusionMatrix(predat, gua_test$segmentovacb)

igua <- adgua$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Guanajuato")

igua



arbol_clasificacion_gue <- rpart(segmentovacb ~ ., data = gue_train, minsplit = 2, 
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
fancyRpartPlot(adgue, main = "Clasificación del VACB en Guerrero año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adgue, gue_test, type = "class")
confusionMatrix(predat, gue_test$segmentovacb)

igue <- adgue$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Guerrero")

igue



arbol_clasificacion_hid <- rpart(segmentovacb ~ ., data = hid_train, minsplit = 2, 
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
fancyRpartPlot(adhid, main = "Clasificación del VACB en Hidalgo año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adhid, hid_test, type = "class")
confusionMatrix(predat, hid_test$segmentovacb)

ihid <- adhid$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Hidalgo")

ihid



arbol_clasificacion_jal <- rpart(segmentovacb ~ ., data = jal_train, minsplit = 2, 
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
fancyRpartPlot(adjal, main = "Clasificación del VACB en Jalisco año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adjal, jal_test, type = "class")
confusionMatrix(predat, jal_test$segmentovacb)

ijal <- adjal$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Jalisco")

ijal



arbol_clasificacion_mic <- rpart(segmentovacb ~ ., data = mic_train, minsplit = 2, 
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
fancyRpartPlot(admic, main = "Clasificación del VACB en Michoacán año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(admic, mic_test, type = "class")
confusionMatrix(predat, mic_test$segmentovacb)

imic <- admic$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Michoacán")

imic



arbol_clasificacion_mor <- rpart(segmentovacb ~ ., data = mor_train, minsplit = 2, 
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
fancyRpartPlot(admor, main = "Clasificación del VACB en Morelos año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(admor, mor_test, type = "class")
confusionMatrix(predat, mor_test$segmentovacb)

imor <- admor$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Morelos")

imor



arbol_clasificacion_nay <- rpart(segmentovacb ~ ., data = nay_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nay_train$segmentovacb)
cv_results <- caret::train(x = nay_train[, -ncol(nay_train)], y = nay_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnay <- rpart(segmentovacb ~ ., data = nay_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adnay, main = "Clasificación del VACB en Nayarit año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adnay, nay_test, type = "class")
confusionMatrix(predat, nay_test$segmentovacb)

inay <- adnay$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Nayarit")

inay



arbol_clasificacion_nle <- rpart(segmentovacb ~ ., data = nle_train, minsplit = 2, 
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
fancyRpartPlot(adnle, main = "Clasificación del VACB en Nuevo León año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adnle, nle_test, type = "class")
confusionMatrix(predat, nle_test$segmentovacb)

inle <- adnle$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Nuevo Léon")

inle



arbol_clasificacion_oax <- rpart(segmentovacb ~ ., data = oax_train, minsplit = 2, 
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
fancyRpartPlot(adoax, main = "Clasificación del VACB en Oaxaca año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adoax, oax_test, type = "class")
confusionMatrix(predat, oax_test$segmentovacb)

ioax <- adoax$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Oaxaca")

ioax



arbol_clasificacion_pue <- rpart(segmentovacb ~ ., data = pue_train, minsplit = 2, 
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
fancyRpartPlot(adpue, main = "Clasificación del VACB en Puebla año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adpue, pue_test, type = "class")
confusionMatrix(predat, pue_test$segmentovacb)

ipue <- adpue$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Puebla")

ipue



arbol_clasificacion_que <- rpart(segmentovacb ~ ., data = que_train, minsplit = 2, 
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
fancyRpartPlot(adque, main = "Clasificación del VACB en Querétaro año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adque, que_test, type = "class")
confusionMatrix(predat, que_test$segmentovacb)

ique <- adque$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Querétaro")
       

ique



arbol_clasificacion_qro <- rpart(segmentovacb ~ ., data = qro_train, minsplit = 2, 
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
fancyRpartPlot(adqro, main = "Clasificación del VACB en Quintana Roo año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adqro, qro_test, type = "class")
confusionMatrix(predat, qro_test$segmentovacb)

iqro <- adqro$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Quintana Roo")

iqro



arbol_clasificacion_slp <- rpart(segmentovacb ~ ., data = slp_train, minsplit = 2, 
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
fancyRpartPlot(adslp, main = "Clasificación del VACB en San Luis Potosi año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adslp, slp_test, type = "class")
confusionMatrix(predat, slp_test$segmentovacb)

islp <- adcol$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: San Luis Potosi")

islp



arbol_clasificacion_sin <- rpart(segmentovacb ~ ., data = sin_train, minsplit = 2, 
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
fancyRpartPlot(adsin, main = "Clasificación del VACB en Sinaloa año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adsin, sin_test, type = "class")
confusionMatrix(predat, sin_test$segmentovacb)

isin <- adsin$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Sinaloa")

isin



arbol_clasificacion_son <- rpart(segmentovacb ~ ., data = son_train, minsplit = 2, 
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
fancyRpartPlot(adson, main = "Clasificación del VACB en Sonora año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adson, son_test, type = "class")
confusionMatrix(predat, son_test$segmentovacb)

ison <- adson$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Sonora")

ison



arbol_clasificacion_tab <- rpart(segmentovacb ~ ., data = tab_train, minsplit = 2, 
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
fancyRpartPlot(adtab, main = "Clasificación del VACB en Tabasco año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtab, tab_test, type = "class")
confusionMatrix(predat, tab_test$segmentovacb)

itab <- adtab$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Tabasco")

itab



arbol_clasificacion_tam <- rpart(segmentovacb ~ ., data = tam_train, minsplit = 2, 
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
fancyRpartPlot(adtam, main = "Clasificación del VACB en Tamaulipas año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtam, tam_test, type = "class")
confusionMatrix(predat, tam_test$segmentovacb)

itam <- adtam$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Tamaulipas")

itam



arbol_clasificacion_tla <- rpart(segmentovacb ~ ., data = tla_train, minsplit = 2, 
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
fancyRpartPlot(adtla, main = "Clasificación del VACB en Tlaxcala año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adtla, tla_test, type = "class")
confusionMatrix(predat, tla_test$segmentovacb)

itla <- adtla$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Tlaxcala")

itla



arbol_clasificacion_ver <- rpart(segmentovacb ~ ., data = ver_train, minsplit = 2, 
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
fancyRpartPlot(adver, main = "Clasificación del VACB en Veracruz año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adver, ver_test, type = "class")
confusionMatrix(predat, ver_test$segmentovacb)

iver <- adver$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Veracruz")

iver



arbol_clasificacion_yuc <- rpart(segmentovacb ~ ., data = yuc_train, minsplit = 2, 
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
fancyRpartPlot(adyuc, main = "Clasificación del VACB en Yucatán año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adyuc, yuc_test, type = "class")
confusionMatrix(predat, yuc_test$segmentovacb)

iyuc <- adyuc$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Yucatán")

iyuc



arbol_clasificacion_zac <- rpart(segmentovacb ~ ., data = zac_train, minsplit = 2, 
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
fancyRpartPlot(adzac, main = "Clasificación del VACB en Zacatecas año 2018",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")
predat <- predict(adzac, zac_test, type = "class")
confusionMatrix(predat, zac_test$segmentovacb)

izac <- adzac$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "segmentovacb") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(segmentovacb, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia en la clasificación del VACB: Zacatecas")

izac



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


dt1 <- data.frame(agu0, bca0, bcs0, cam0, chh0, chi0,
                 cmx0, coa0, col0,
                 dur0, emx0, gua0,  gue0, hid0,
                 jal0, mic0, mor0,  nay0, nle0, oax0,
                 pue0, qro0, que0, sin0,
                 son0, slp0, tab0, tam0, tla0,
                 ver0, yuc0, zac0)
names(dt1) <- c("AGU", "BCA", "BCS", "CAM", "CHH", "CHI","CMX", "COA", "COL","DUR",
               "EMX", "GUA", "GUE", "HID", "JAL", "MIC", "MOR", "NAY","NLE", "OAX",
               "PUE", "QRO", "QUE", "SIN", "SON", "SLP", "TAB", "TAM", "TLA","VER", "YUC", "ZAC")

dt01 <- as.matrix(dt1)

ppt <- prop.table(dt01, margin = 1)

barplot(ppt)

write.csv(dt01,"pesos18.csv")

dt10 <- t(dt1)

dcc <- as.data.frame(dt10)

plot_num(dcc)

con <- select(dcc, AE:vacb_ataf)




#Conglomerados

set.seed(12345678)

clustering <- kmeans(con, centers = 10)

correlations <- cor(con, clustering$cluster)

print(correlations)

fviz_nbclust(x = con, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(con, method = "euclidean"), nstart = 50)

km_clusters <- kmeans(x = con, centers = 7, nstart = 50)

fviz_cluster(object = km_clusters, data = con, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Agrupación por capacidades productivas físicas: K-means") +
  theme_bw() +
  theme(legend.position = "none")


res.dist <- get_dist(con, stand = FALSE, 
                     method = "euclidean")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))

corrplot(as.matrix(res.dist), is.corr = FALSE, 
         method = "color",
         order = "hclust", type = "upper")

d <- as.matrix(res.dist)

heatmap(d, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2008")


res.hc <- hclust(res.dist, "ward.D")

agrup18 <- fviz_dend(res.hc, cex = 0.5, k = 9, rect = TRUE,
          k_colors = "ucscgb", 
          color_labels_by_k = TRUE,
          main = "Figura 10. Agrupación estatal por capacidades productivas año 2018",
          ylab = "Pesos en la matriz de distancia",
          xlab = "Fuente: Elaboración propia con datos del Censo Económico 2019: INEGI", 
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

agrup18

grp <- cutree(res.hc, k = 9)

fviz_cluster(list(data = con, cluster = grp),
             ellipse.type = "convex",  rect = TRUE,
             k_colors = "uchicago", 
             color_labels_by_k = TRUE,
             repel = T, show.clust.cent = T, 
             ggtheme = theme_minimal(), main = "Agrupación por capacidades productivas año 2018",
             xlab = "fbcf_ataf", ylab = "vacb_htpot")

plot(as.phylo(res.hc), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc), type = "unrooted", cex = 0.7,
     no.margin = T)


fviz_dend(res.hc, k=9, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc, cex= 0.8, k= 9,
          k_colors =    "lancet", 
          color_labels_by_k = TRUE,
          type="circular", title = "Agrupación estatal por capacidades productivas año 2018",
          xlab = "Entidades Federativas", 
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

plot(as.phylo(res.hc), type = "radial")


