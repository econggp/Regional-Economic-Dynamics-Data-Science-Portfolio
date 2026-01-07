psych::describe(c2013$VACB)

c2013$segmentovacb <- as.factor(cut(c2013$VACB, breaks=c(-7056, 36.4, 462.66, 8053, 569621), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))


ca13 <- select(c2013, ID, EF, pijataf:segmentovacb) %>% column_to_rownames(., var = 'ID')

ca13 <- ca13[complete.cases(ca13$pbt_ataf), ]

agu13 <- filter(ca13, EF %in% c("AGU"))  
bca13 <- filter(ca13, EF %in% c("BCA"))
bcs13 <- filter(ca13, EF %in% c("BCS"))
cam13 <- filter(ca13, EF %in% c("CAM"))
coa13 <- filter(ca13, EF %in% c("COA"))
col13 <- filter(ca13, EF %in% c("COL"))
chi13 <- filter(ca13, EF %in% c("CHI"))
chh13 <- filter(ca13, EF %in% c("CHH"))
cmx13 <- filter(ca13, EF %in% c("CMX"))
dur13 <- filter(ca13, EF %in% c("DUR"))
gua13 <- filter(ca13, EF %in% c("GUA"))
gue13 <- filter(ca13, EF %in% c("GUE"))
hid13 <- filter(ca13, EF %in% c("HID"))
jal13 <- filter(ca13, EF %in% c("JAL"))
emx13 <- filter(ca13, EF %in% c("EMX"))
mic13 <- filter(ca13, EF %in% c("MIC"))
mor13 <- filter(ca13, EF %in% c("MOR"))
nay13 <- filter(ca13, EF %in% c("NAY"))
nle13 <- filter(ca13, EF %in% c("NLE"))
oax13 <- filter(ca13, EF %in% c("OAX"))
pue13 <- filter(ca13, EF %in% c("PUE"))
que13 <- filter(ca13, EF %in% c("QUE"))
qro13 <- filter(ca13, EF %in% c("QRO"))
slp13 <- filter(ca13, EF %in% c("SLP"))
sin13 <- filter(ca13, EF %in% c("SIN"))
son13 <- filter(ca13, EF %in% c("SON"))
tab13 <- filter(ca13, EF %in% c("TAB"))
tam13 <- filter(ca13, EF %in% c("TAM"))
tla13 <- filter(ca13, EF %in% c("TLA"))
ver13 <- filter(ca13, EF %in% c("VER"))
yuc13 <- filter(ca13, EF %in% c("YUC"))
zac13 <- filter(ca13, EF %in% c("ZAC"))

agu13$EF <- NULL
bca13$EF <- NULL
bcs13$EF <- NULL
cam13$EF <- NULL
chh13$EF <- NULL
chi13$EF <- NULL
cmx13$EF <- NULL
coa13$EF <- NULL
col13$EF <- NULL
dur13$EF <- NULL
emx13$EF <- NULL
gua13$EF <- NULL
gue13$EF <- NULL
hid13$EF <- NULL
jal13$EF <- NULL
mic13$EF <- NULL
mor13$EF <- NULL
nay13$EF <- NULL
nle13$EF <- NULL
oax13$EF <- NULL
pue13$EF <- NULL
qro13$EF <- NULL
que13$EF <- NULL
sin13$EF <- NULL
son13$EF <- NULL
slp13$EF <- NULL
tab13$EF <- NULL
tam13$EF <- NULL
tla13$EF <- NULL
ver13$EF <- NULL
yuc13$EF <- NULL
zac13$EF <- NULL

set.seed(987654321)

trainindex = createDataPartition(agu13$pbt_ataf, p=0.75, list = FALSE)
agu13_train= agu13[trainindex, ]
agu13_test= agu13[-trainindex, ]

trainindex = createDataPartition(bca13$pbt_ataf, p=0.75, list = FALSE)
bca13_train= bca13[trainindex, ]
bca13_test= bca13[-trainindex, ]

trainindex = createDataPartition(bcs13$pbt_ataf, p=0.75, list = FALSE)
bcs13_train= bcs13[trainindex, ]
bcs13_test= bcs13[-trainindex, ]

trainindex = createDataPartition(cam13$pbt_ataf, p=0.75, list = FALSE)
cam13_train= cam13[trainindex, ]
cam13_test= cam13[-trainindex, ]

trainindex = createDataPartition(coa13$pbt_ataf, p=0.75, list = FALSE)
coa13_train= coa13[trainindex, ]
coa13_test= coa13[-trainindex, ]

trainindex = createDataPartition(col13$pbt_ataf, p=0.75, list = FALSE)
col13_train= col13[trainindex, ]
col13_test= col13[-trainindex, ]

trainindex = createDataPartition(chi13$pbt_ataf, p=0.75, list = FALSE)
chi13_train= chi13[trainindex, ]
chi13_test= chi13[-trainindex, ]

trainindex = createDataPartition(chh13$pbt_ataf, p=0.75, list = FALSE)
chh13_train= chh13[trainindex, ]
chh13_test= chh13[-trainindex, ]

trainindex = createDataPartition(cmx13$pbt_ataf, p=0.75, list = FALSE)
cmx13_train= cmx13[trainindex, ]
cmx13_test= cmx13[-trainindex, ]

trainindex = createDataPartition(dur13$pbt_ataf, p=0.75, list = FALSE)
dur13_train= dur13[trainindex, ]
dur13_test= dur13[-trainindex, ]

trainindex = createDataPartition(gua13$pbt_ataf, p=0.75, list = FALSE)
gua13_train= gua13[trainindex, ]
gua13_test= gua13[-trainindex, ]

trainindex = createDataPartition(gue13$pbt_ataf, p=0.75, list = FALSE)
gue13_train= gue13[trainindex, ]
gue13_test= gue13[-trainindex, ]

trainindex = createDataPartition(hid13$pbt_ataf, p=0.75, list = FALSE)
hid13_train= hid13[trainindex, ]
hid13_test= hid13[-trainindex, ]

trainindex = createDataPartition(jal13$pbt_ataf, p=0.75, list = FALSE)
jal13_train= jal13[trainindex, ]
jal13_test= jal13[-trainindex, ]

trainindex = createDataPartition(emx13$pbt_ataf, p=0.75, list = FALSE)
emx13_train= emx13[trainindex, ]
emx13_test= emx13[-trainindex, ]

trainindex = createDataPartition(mic13$pbt_ataf, p=0.75, list = FALSE)
mic13_train= mic13[trainindex, ]
mic13_test= mic13[-trainindex, ]

trainindex = createDataPartition(mor13$pbt_ataf, p=0.75, list = FALSE)
mor13_train= mor13[trainindex, ]
mor13_test= mor13[-trainindex, ]

trainindex = createDataPartition(nay13$pbt_ataf, p=0.75, list = FALSE)
nay13_train= nay13[trainindex, ]
nay13_test= nay13[-trainindex, ]

trainindex = createDataPartition(nle13$pbt_ataf, p=0.75, list = FALSE)
nle13_train= nle13[trainindex, ]
nle13_test= nle13[-trainindex, ]

trainindex = createDataPartition(oax13$pbt_ataf, p=0.75, list = FALSE)
oax13_train= oax13[trainindex, ]
oax13_test= oax13[-trainindex, ]

trainindex = createDataPartition(pue13$pbt_ataf, p=0.75, list = FALSE)
pue13_train= pue13[trainindex, ]
pue13_test= pue13[-trainindex, ]

trainindex = createDataPartition(que13$pbt_ataf, p=0.75, list = FALSE)
que13_train= que13[trainindex, ]
que13_test= que13[-trainindex, ]

trainindex = createDataPartition(qro13$pbt_ataf, p=0.75, list = FALSE)
qro13_train= qro13[trainindex, ]
qro13_test= qro13[-trainindex, ]

trainindex = createDataPartition(slp13$pbt_ataf, p=0.75, list = FALSE)
slp13_train= slp13[trainindex, ]
slp13_test= slp13[-trainindex, ]

trainindex = createDataPartition(sin13$pbt_ataf, p=0.75, list = FALSE)
sin13_train= sin13[trainindex, ]
sin13_test= sin13[-trainindex, ]

trainindex = createDataPartition(son13$pbt_ataf, p=0.75, list = FALSE)
son13_train= son13[trainindex, ]
son13_test= son13[-trainindex, ]

trainindex = createDataPartition(tab13$pbt_ataf, p=0.75, list = FALSE)
tab13_train= tab13[trainindex, ]
tab13_test= tab13[-trainindex, ]

trainindex = createDataPartition(tam13$pbt_ataf, p=0.75, list = FALSE)
tam13_train= tam13[trainindex, ]
tam13_test= tam13[-trainindex, ]

trainindex = createDataPartition(tla13$pbt_ataf, p=0.75, list = FALSE)
tla13_train= tla13[trainindex, ]
tla13_test= tla13[-trainindex, ]

trainindex = createDataPartition(ver13$pbt_ataf, p=0.75, list = FALSE)
ver13_train= ver13[trainindex, ]
ver13_test= ver13[-trainindex, ]

trainindex = createDataPartition(yuc13$pbt_ataf, p=0.75, list = FALSE)
yuc13_train= yuc13[trainindex, ]
yuc13_test= yuc13[-trainindex, ]

trainindex = createDataPartition(zac13$pbt_ataf, p=0.75, list = FALSE)
zac13_train= zac13[trainindex, ]
zac13_test= zac13[-trainindex, ]



# Árboles de decisión

set.seed(1234567890)

arbol_clasificacionagu13 <- rpart(segmentovacb ~ ., data = agu13_train, 
                             minsplit = 2, minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(agu13_train$segmentovacb)
cv_results <- caret::train(x = agu13_train[, -ncol(agu13_train)], y = agu13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adagu13 <- rpart(segmentovacb ~ ., data = agu13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adagu13, main = "Clasificación del VACB en Aguascalientes año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adagu13, agu13_test, type = "class")
confusionMatrix(predat, agu13_test$segmentovacb)



arbol_clasificacionbca13 <- rpart(segmentovacb ~ ., data = bca13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bca13_train$segmentovacb)
cv_results <- caret::train(x = bca13_train[, -ncol(bca13_train)], y = bca13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbca13 <- rpart(segmentovacb ~ ., data = bca13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adbca13, main = "Clasificación del VACB en Baja California año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adbca13, bca13_test, type = "class")
confusionMatrix(predat, bca13_test$segmentovacb)



arbol_clasificacionbcs13 <- rpart(segmentovacb ~ ., data = bcs13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bcs13_train$segmentovacb)
cv_results <- caret::train(x = bcs13_train[, -ncol(bcs13_train)], y = bcs13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbcs13 <- rpart(segmentovacb ~ ., data = bcs13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adbcs13, main = "Clasificación del VACB en Baja California Sur año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adbcs13, bcs13_test, type = "class")
confusionMatrix(predat, bcs13_test$segmentovacb)



arbol_clasificacioncam13 <- rpart(segmentovacb ~ ., data = cam13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cam13_train$segmentovacb)
cv_results <- caret::train(x = cam13_train[, -ncol(cam13_train)], y = cam13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcam13 <- rpart(segmentovacb ~ ., data = cam13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcam13, main = "Clasificación del VACB en Campeche año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adcam13, cam13_test, type = "class")
confusionMatrix(predat, cam13_test$segmentovacb)



arbol_clasificacionchh13 <- rpart(segmentovacb ~ ., data = chh13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chh13_train$segmentovacb)
cv_results <- caret::train(x = chh13_train[, -ncol(chh13_train)], y = chh13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchh13 <- rpart(segmentovacb ~ ., data = chh13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adchh13, main = "Clasificación del VACB en Chihuahua año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adchh13, chh13_test, type = "class")
confusionMatrix(predat, chh13_test$segmentovacb)



arbol_clasificacionchi13 <- rpart(segmentovacb ~ ., data = chi13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chi13_train$segmentovacb)
cv_results <- caret::train(x = chi13_train[, -ncol(chi13_train)], y = chi13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchi13 <- rpart(segmentovacb ~ ., data = chi13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adchi13, main = "Clasificación del VACB en Chiapas año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adchi13, chi13_test, type = "class")
confusionMatrix(predat, chi13_test$segmentovacb)



arbol_clasificacioncmx13 <- rpart(segmentovacb ~ ., data = cmx13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cmx13_train$segmentovacb)
cv_results <- caret::train(x = cmx13_train[, -ncol(cmx13_train)], y = cmx13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcmx13 <- rpart(segmentovacb ~ ., data = cmx13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcmx13, main = "Clasificación del VACB en la Ciudad de México año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adcmx13, cmx13_test, type = "class")
confusionMatrix(predat, cmx13_test$segmentovacb)



arbol_clasificacioncoa13 <- rpart(segmentovacb ~ ., data = coa13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(coa13_train$segmentovacb)
cv_results <- caret::train(x = coa13_train[, -ncol(coa13_train)], y = coa13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcoa13 <- rpart(segmentovacb ~ ., data = coa13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcoa13, main = "Clasificación del VACB en Coahuila año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adcoa13, coa13_test, type = "class")
confusionMatrix(predat, coa13_test$segmentovacb)



arbol_clasificacioncol13 <- rpart(segmentovacb ~ ., data = col13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(col13_train$segmentovacb)
cv_results <- caret::train(x = col13_train[, -ncol(col13_train)], y = col13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcol13 <- rpart(segmentovacb ~ ., data = col13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcol13, main = "Clasificación del VACB en Colima año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adcol13, col13_test, type = "class")
confusionMatrix(predat, col13_test$segmentovacb)



arbol_clasificaciondur13 <- rpart(segmentovacb ~ ., data = dur13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(dur13_train$segmentovacb)
cv_results <- caret::train(x = dur13_train[, -ncol(dur13_train)], y = dur13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
addur13 <- rpart(segmentovacb ~ ., data = dur13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(addur13, main = "Clasificación del VACB en Durango año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(addur13, dur13_test, type = "class")
confusionMatrix(predat, dur13_test$segmentovacb)



arbol_clasificacionemx13 <- rpart(segmentovacb ~ ., data = emx13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(emx13_train$segmentovacb)
cv_results <- caret::train(x = emx13_train[, -ncol(emx13_train)], y = emx13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
ademx13 <- rpart(segmentovacb ~ ., data = emx13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(ademx13, main = "Clasificación del VACB en el Estado de México año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(ademx13, emx13_test, type = "class")
confusionMatrix(predat, emx13_test$segmentovacb)



arbol_clasificaciongua13 <- rpart(segmentovacb ~ ., data = gua13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gua13_train$segmentovacb)
cv_results <- caret::train(x = gua13_train[, -ncol(gua13_train)], y = gua13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgua13 <- rpart(segmentovacb ~ ., data = gua13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adgua13, main = "Clasificación del VACB en Guanajuato año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adgua13, gua13_test, type = "class")
confusionMatrix(predat, gua13_test$segmentovacb)



arbol_clasificaciongue13 <- rpart(segmentovacb ~ ., data = gue13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gue13_train$segmentovacb)
cv_results <- caret::train(x = gue13_train[, -ncol(gue13_train)], y = gue13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgue13 <- rpart(segmentovacb ~ ., data = gue13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adgue13, main = "Clasificación del VACB en Guerrero año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adgue13, gue13_test, type = "class")
confusionMatrix(predat, gue13_test$segmentovacb)



arbol_clasificacionhid13 <- rpart(segmentovacb ~ ., data = hid13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(hid13_train$segmentovacb)
cv_results <- caret::train(x = hid13_train[, -ncol(hid13_train)], y = hid13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adhid13 <- rpart(segmentovacb ~ ., data = hid13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adhid13, main = "Clasificación del VACB en Hidalgo año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adhid13, hid13_test, type = "class")
confusionMatrix(predat, hid13_test$segmentovacb)



arbol_clasificacionjal13 <- rpart(segmentovacb ~ ., data = jal13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(jal13_train$segmentovacb)
cv_results <- caret::train(x = jal13_train[, -ncol(jal13_train)], y = jal13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adjal13 <- rpart(segmentovacb ~ ., data = jal13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adjal13, main = "Clasificación del VACB en Jalisco año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adjal13, jal13_test, type = "class")
confusionMatrix(predat, jal13_test$segmentovacb)



arbol_clasificacionmic13 <- rpart(segmentovacb ~ ., data = mic13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mic13_train$segmentovacb)
cv_results <- caret::train(x = mic13_train[, -ncol(mic13_train)], y = mic13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admic13 <- rpart(segmentovacb ~ ., data = mic13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(admic13, main = "Clasificación del VACB en Michoacán año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(admic13, mic13_test, type = "class")
confusionMatrix(predat, mic13_test$segmentovacb)



arbol_clasificacionmor13 <- rpart(segmentovacb ~ ., data = mor13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mor13_train$segmentovacb)
cv_results <- caret::train(x = mor13_train[, -ncol(mor13_train)], y = mor13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admor13 <- rpart(segmentovacb ~ ., data = mor13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(admor13, main = "Clasificación del VACB en Morelos año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(admor13, mor13_test, type = "class")
confusionMatrix(predat, mor13_test$segmentovacb)



arbol_clasificacionnay13 <- rpart(segmentovacb ~ ., data = nay13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nay13_train$segmentovacb)
cv_results <- caret::train(x = nay13_train[, -ncol(nay13_train)], y = nay13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnay13 <- rpart(segmentovacb ~ ., data = nay13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adnay13, main = "Clasificación del VACB en Nayarit año 13",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adnay13, nay13_test, type = "class")
confusionMatrix(predat, nay13_test$segmentovacb)



arbol_clasificacionnle13 <- rpart(segmentovacb ~ ., data = nle13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nle13_train$segmentovacb)
cv_results <- caret::train(x = nle13_train[, -ncol(nle13_train)], y = nle13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnle13 <- rpart(segmentovacb ~ ., data = nle13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adnle, main = "Clasificación del VACB en Nuevo León año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adnle13, nle13_test, type = "class")
confusionMatrix(predat, nle13_test$segmentovacb)



arbol_clasificacionoax13 <- rpart(segmentovacb ~ ., data = oax13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(oax13_train$segmentovacb)
cv_results <- caret::train(x = oax13_train[, -ncol(oax13_train)], y = oax13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adoax13 <- rpart(segmentovacb ~ ., data = oax13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adoax13, main = "Clasificación del VACB en Oaxaca año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adoax13, oax13_test, type = "class")
confusionMatrix(predat, oax13_test$segmentovacb)



arbol_clasificacionpue13 <- rpart(segmentovacb ~ ., data = pue13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(pue13_train$segmentovacb)
cv_results <- caret::train(x = pue13_train[, -ncol(pue13_train)], y = pue13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adpue13 <- rpart(segmentovacb ~ ., data = pue13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adpue13, main = "Clasificación del VACB en Puebla año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adpue13, pue13_test, type = "class")
confusionMatrix(predat, pue13_test$segmentovacb)



arbol_clasificacionque13 <- rpart(segmentovacb ~ ., data = que13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(que13_train$segmentovacb)
cv_results <- caret::train(x = que13_train[, -ncol(que13_train)], y = que13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adque13 <- rpart(segmentovacb ~ ., data = que13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adque13, main = "Clasificación del VACB en Querétaro año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adque13, que13_test, type = "class")
confusionMatrix(predat, que13_test$segmentovacb)



arbol_clasificacionqro13 <- rpart(segmentovacb ~ ., data = qro13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(qro13_train$segmentovacb)
cv_results <- caret::train(x = qro13_train[, -ncol(qro13_train)], y = qro13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adqro13 <- rpart(segmentovacb ~ ., data = qro13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adqro13, main = "Clasificación del VACB en Quintana Roo año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adqro13, qro13_test, type = "class")
confusionMatrix(predat, qro13_test$segmentovacb)



arbol_clasificacionslp13 <- rpart(segmentovacb ~ ., data = slp13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(slp13_train$segmentovacb)
cv_results <- caret::train(x = slp13_train[, -ncol(slp13_train)], y = slp13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adslp13 <- rpart(segmentovacb ~ ., data = slp13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adslp13, main = "Clasificación del VACB en San Luis Potosi año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adslp13, slp13_test, type = "class")
confusionMatrix(predat, slp13_test$segmentovacb)



arbol_clasificacionsin13 <- rpart(segmentovacb ~ ., data = sin13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(sin13_train$segmentovacb)
cv_results <- caret::train(x = sin13_train[, -ncol(sin13_train)], y = sin13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adsin13 <- rpart(segmentovacb ~ ., data = sin13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adsin13, main = "Clasificación del VACB en Sinaloa año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adsin13, sin13_test, type = "class")
confusionMatrix(predat, sin13_test$segmentovacb)



arbol_clasificacionson13 <- rpart(segmentovacb ~ ., data = son13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(son13_train$segmentovacb)
cv_results <- caret::train(x = son13_train[, -ncol(son13_train)], y = son13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adson13 <- rpart(segmentovacb ~ ., data = son13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adson13, main = "Clasificación del VACB en Sonora año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adson13, son13_test, type = "class")
confusionMatrix(predat, son13_test$segmentovacb)



arbol_clasificaciontab13 <- rpart(segmentovacb ~ ., data = tab13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tab13_train$segmentovacb)
cv_results <- caret::train(x = tab13_train[, -ncol(tab13_train)], y = tab13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtab13 <- rpart(segmentovacb ~ ., data = tab13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtab13, main = "Clasificación del VACB en Tabasco año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adtab13, tab13_test, type = "class")
confusionMatrix(predat, tab13_test$segmentovacb)



arbol_clasificaciontam13 <- rpart(segmentovacb ~ ., data = tam13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tam13_train$segmentovacb)
cv_results <- caret::train(x = tam13_train[, -ncol(tam13_train)], y = tam13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtam13 <- rpart(segmentovacb ~ ., data = tam13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtam13, main = "Clasificación del VACB en Tamaulipas año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adtam13, tam13_test, type = "class")
confusionMatrix(predat, tam13_test$segmentovacb)



arbol_clasificaciontla13 <- rpart(segmentovacb ~ ., data = tla13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tla13_train$segmentovacb)
cv_results <- caret::train(x = tla13_train[, -ncol(tla13_train)], y = tla13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtla13 <- rpart(segmentovacb ~ ., data = tla13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtla13, main = "Clasificación del VACB en Tlaxcala año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adtla13, tla13_test, type = "class")
confusionMatrix(predat, tla13_test$segmentovacb)



arbol_clasificacionver13 <- rpart(segmentovacb ~ ., data = ver13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(ver13_train$segmentovacb)
cv_results <- caret::train(x = ver13_train[, -ncol(ver13_train)], y = ver13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adver13 <- rpart(segmentovacb ~ ., data = ver13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adver13, main = "Clasificación del VACB en Veracruz año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adver13, ver13_test, type = "class")
confusionMatrix(predat, ver13_test$segmentovacb)



arbol_clasificacioyuc13 <- rpart(segmentovacb ~ ., data = yuc13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(yuc13_train$segmentovacb)
cv_results <- caret::train(x = yuc13_train[, -ncol(yuc13_train)], y = yuc13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adyuc13 <- rpart(segmentovacb ~ ., data = yuc13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adyuc13, main = "Clasificación del VACB en Yucatán año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adyuc13, yuc13_test, type = "class")
confusionMatrix(predat, yuc13_test$segmentovacb)



arbol_clasificacionzac13 <- rpart(segmentovacb ~ ., data = zac13_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(zac13_train$segmentovacb)
cv_results <- caret::train(x = zac13_train[, -ncol(zac13_train)], y = zac13_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adzac13 <- rpart(segmentovacb ~ ., data = zac13_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adzac13, main = "Clasificación del VACB en Zacatecas año 2013",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2014: INEGI")
predat <- predict(adzac13, zac13_test, type = "class")
confusionMatrix(predat, zac13_test$segmentovacb)



impadagu13 <- varImp(adagu13)
impadbca13 <- varImp(adbca13)
impadbcs13 <- varImp(adbcs13)
impadcam13 <- varImp(adcam13)
impadchh13 <- varImp(adchh13)
impadchi13 <- varImp(adchi13)
impadcmx13 <- varImp(adcmx13)
impadcoa13 <- varImp(adcoa13)
impadcol13 <- varImp(adcol13)
impaddur13 <- varImp(addur13)
impademx13 <- varImp(ademx13)
impadgua13 <- varImp(adgua13)
impadgue13 <- varImp(adgue13)
impadhid13 <- varImp(adhid13)
impadjal13 <- varImp(adjal13)
impadmic13 <- varImp(admic13)
impadmor13 <- varImp(admor13)
impadnay13 <- varImp(adnay13)
impadnle13 <- varImp(adnle13)
impadoax13 <- varImp(adoax13)
impadpue13 <- varImp(adpue13)
impadqro13 <- varImp(adqro13)
impadque13 <- varImp(adque13)
impadsin13 <- varImp(adsin13)
impadslp13 <- varImp(adslp13)
impadson13 <- varImp(adson13)
impadtab13 <- varImp(adtab13)
impadtam13 <- varImp(adtam13)
impadtla13 <- varImp(adtla13)
impadver13 <- varImp(adver13)
impadyuc13 <- varImp(adyuc13)
impadzac13 <- varImp(adzac13)

agu013<-impadagu13 
bca013<-impadbca13 
bcs013<-impadbcs13 
cam013<-impadcam13 
chh013<-impadchh13
chi013<-impadchi13 
cmx013<-impadcmx13 
coa013<-impadcoa13 
col013<-impadcol13
dur013<-impaddur13 
emx013<-impademx13 
gua013<-impadgua13 
gue013<-impadgue13 
hid013<-impadhid13
jal013<-impadjal13 
mic013<-impadmic13 
mor013<-impadmor13 
nay013<-impadnay13 
nle013<-impadnle13
oax013<-impadoax13 
pue013<-impadpue13 
qro013<-impadqro13 
que013<-impadque13 
sin013<-impadsin13
son013<-impadson13 
slp013<-impadslp13 
tab013<-impadtab13 
tam013<-impadtam13 
tla013<-impadtla13
ver013<-impadver13 
yuc013<-impadyuc13 
zac013<-impadzac13



d113 <- data.frame(agu013, bca013, bcs013, cam013, chh013, chi013,
                   cmx013, coa013, col013,
                   dur013, emx013, gua013, gue013, hid013,
                   jal013, mic013, mor013, nay013, nle013, oax013,
                   pue013, qro013, que013, sin013,
                   son013, slp013, tab013, tam013, tla013,
                   ver013, yuc013, zac013)
names(d113) <- c("AGU", "BCA", "BCS", "CAM", "CHH", "CHI","CMX", "COA", "COL", "DUR",
                 "EMX", "GUA", "GUE", "HID", "JAL", "MIC",  "MOR", "NAY","NLE", "OAX",
                 "PUE", "QRO", "QUE", "SIN", "SON", "SLP", "TAB", "TAM", "TLA", "VER", 
                 "YUC", "ZAC")


dt013 <- as.matrix(d113)

ppt13 <- prop.table(dt013, margin = 1)

barplot(ppt13)

write.csv(dt013,"pesos13.csv")

dt0113 <- t(dt013)

dcc13 <- as.data.frame(dt0113)

plot_num(dcc13)

con13 <- select(dcc13, AE:fbcf_pacd)


#Conglomerados

set.seed(12345678)

clustering13 <- kmeans(con13, centers = 10)

correlations13 <- cor(con13, clustering13$cluster)

print(correlations13)

fviz_nbclust(x = con13, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(con13, method = "euclidean"), nstart = 50)

km_clusters13 <- kmeans(x = con13, centers = 7, nstart = 50)

fviz_cluster(object = km_clusters13, data = con, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Agrupación por capacidades productivas físicas: K-means") +
  theme_bw() +
  theme(legend.position = "none")


res.dist13 <- get_dist(con13, stand = FALSE, 
                     method = "euclidean")

fviz_dist(res.dist13, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))

corrplot(as.matrix(res.dist13), is.corr = FALSE, 
         method = "color",
         order = "hclust", type = "upper")

d13 <- as.matrix(res.dist13)

heatmap(d13, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2013")


res.hc13 <- hclust(res.dist13, "ward.D2")

agrup13 <- fviz_dend(res.hc13, cex = 0.5, k = 8, rect = TRUE,
          k_colors = "npg", 
          color_labels_by_k = TRUE,
          main = "Figura 8. Agrupación estatal por capacidades productivas año 2013",
          xlab = "Fuente: Elaboración propia con datos del Censo Económico 2014: INEGI",
          ylab = "Pesos en la matriz de distancia",
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

agrup13

grp13 <- cutree(res.hc13, k = 8)

fviz_cluster(list(data = con13, cluster = grp13),
             ellipse.type = "convex",  rect = TRUE,
             k_colors =  "aaas", 
             color_labels_by_k = TRUE,
             repel = T, show.clust.cent = T, 
             ggtheme = theme_minimal(), main = "Agrupación por capacidades productivas año 2013",
             xlab = "fbcf_ataf", ylab = "vacb_htpot")

plot(as.phylo(res.hc13), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc13), type = "unrooted", cex = 0.7,
     no.margin = T)


fviz_dend(res.hc13, k=8, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc13, cex= 0.8, k=8,
          k_colors = "lancet", 
          color_labels_by_k = TRUE,
          type="circular", title = "Agrupación estatal por desempeño y capacidades productivas año 2013",
          xlab = "Entidades Federativas", 
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

plot(as.phylo(res.hc13), type = "radial")

