psych::describe(c2003$VACB)

c2003$segmentovacb <- as.factor(cut(c2003$VACB, breaks=c(-4012, 21.61, 276.03, 4133, 246837), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))


ca03 <- select(c2003, ID, EF, pijataf:segmentovacb) %>% column_to_rownames(., var = 'ID')

ca03 <- ca03[complete.cases(ca03$pbt_ataf), ]

agu03 <- filter(ca03, EF %in% c("AGU"))  
bca03 <- filter(ca03, EF %in% c("BCA"))
bcs03 <- filter(ca03, EF %in% c("BCS"))
cam03 <- filter(ca03, EF %in% c("CAM"))
coa03 <- filter(ca03, EF %in% c("COA"))
col03 <- filter(ca03, EF %in% c("COL"))
chi03 <- filter(ca03, EF %in% c("CHI"))
chh03 <- filter(ca03, EF %in% c("CHH"))
cmx03 <- filter(ca03, EF %in% c("CMX"))
dur03 <- filter(ca03, EF %in% c("DUR"))
gua03 <- filter(ca03, EF %in% c("GUA"))
gue03 <- filter(ca03, EF %in% c("GUE"))
hid03 <- filter(ca03, EF %in% c("HID"))
jal03 <- filter(ca03, EF %in% c("JAL"))
emx03 <- filter(ca03, EF %in% c("EMX"))
mic03 <- filter(ca03, EF %in% c("MIC"))
mor03 <- filter(ca03, EF %in% c("MOR"))
nay03 <- filter(ca03, EF %in% c("NAY"))
nle03 <- filter(ca03, EF %in% c("NLE"))
oax03 <- filter(ca03, EF %in% c("OAX"))
pue03 <- filter(ca03, EF %in% c("PUE"))
que03 <- filter(ca03, EF %in% c("QUE"))
qro03 <- filter(ca03, EF %in% c("QRO"))
slp03 <- filter(ca03, EF %in% c("SLP"))
sin03 <- filter(ca03, EF %in% c("SIN"))
son03 <- filter(ca03, EF %in% c("SON"))
tab03 <- filter(ca03, EF %in% c("TAB"))
tam03 <- filter(ca03, EF %in% c("TAM"))
tla03 <- filter(ca03, EF %in% c("TLA"))
ver03 <- filter(ca03, EF %in% c("VER"))
yuc03 <- filter(ca03, EF %in% c("YUC"))
zac03 <- filter(ca03, EF %in% c("ZAC"))

agu03$EF <- NULL
bca03$EF <- NULL
bcs03$EF <- NULL
cam03$EF <- NULL
chh03$EF <- NULL
chi03$EF <- NULL
cmx03$EF <- NULL
coa03$EF <- NULL
col03$EF <- NULL
dur03$EF <- NULL
emx03$EF <- NULL
gua03$EF <- NULL
gue03$EF <- NULL
hid03$EF <- NULL
jal03$EF <- NULL
mic03$EF <- NULL
mor03$EF <- NULL
nay03$EF <- NULL
nle03$EF <- NULL
oax03$EF <- NULL
pue03$EF <- NULL
qro03$EF <- NULL
que03$EF <- NULL
sin03$EF <- NULL
son03$EF <- NULL
slp03$EF <- NULL
tab03$EF <- NULL
tam03$EF <- NULL
tla03$EF <- NULL
ver03$EF <- NULL
yuc03$EF <- NULL
zac03$EF <- NULL

set.seed(987654321)

trainindex = createDataPartition(agu03$pbt_ataf, p=0.75, list = FALSE)
agu03_train= agu03[trainindex, ]
agu03_test= agu03[-trainindex, ]

trainindex = createDataPartition(bca03$pbt_ataf, p=0.75, list = FALSE)
bca03_train= bca03[trainindex, ]
bca03_test= bca03[-trainindex, ]

trainindex = createDataPartition(bcs03$pbt_ataf, p=0.75, list = FALSE)
bcs03_train= bcs03[trainindex, ]
bcs03_test= bcs03[-trainindex, ]

trainindex = createDataPartition(cam03$pbt_ataf, p=0.75, list = FALSE)
cam03_train= cam03[trainindex, ]
cam03_test= cam03[-trainindex, ]

trainindex = createDataPartition(coa03$pbt_ataf, p=0.75, list = FALSE)
coa03_train= coa03[trainindex, ]
coa03_test= coa03[-trainindex, ]

trainindex = createDataPartition(col03$pbt_ataf, p=0.75, list = FALSE)
col03_train= col03[trainindex, ]
col03_test= col03[-trainindex, ]

trainindex = createDataPartition(chi03$pbt_ataf, p=0.75, list = FALSE)
chi03_train= chi03[trainindex, ]
chi03_test= chi03[-trainindex, ]

trainindex = createDataPartition(chh03$pbt_ataf, p=0.75, list = FALSE)
chh03_train= chh03[trainindex, ]
chh03_test= chh03[-trainindex, ]

trainindex = createDataPartition(cmx03$pbt_ataf, p=0.75, list = FALSE)
cmx03_train= cmx03[trainindex, ]
cmx03_test= cmx03[-trainindex, ]

trainindex = createDataPartition(dur03$pbt_ataf, p=0.75, list = FALSE)
dur03_train= dur03[trainindex, ]
dur03_test= dur03[-trainindex, ]

trainindex = createDataPartition(gua03$pbt_ataf, p=0.75, list = FALSE)
gua03_train= gua03[trainindex, ]
gua03_test= gua03[-trainindex, ]

trainindex = createDataPartition(gue03$pbt_ataf, p=0.75, list = FALSE)
gue03_train= gue03[trainindex, ]
gue03_test= gue03[-trainindex, ]

trainindex = createDataPartition(hid03$pbt_ataf, p=0.75, list = FALSE)
hid03_train= hid03[trainindex, ]
hid03_test= hid03[-trainindex, ]

trainindex = createDataPartition(jal03$pbt_ataf, p=0.75, list = FALSE)
jal03_train= jal03[trainindex, ]
jal03_test= jal03[-trainindex, ]

trainindex = createDataPartition(emx03$pbt_ataf, p=0.75, list = FALSE)
emx03_train= emx03[trainindex, ]
emx03_test= emx03[-trainindex, ]

trainindex = createDataPartition(mic03$pbt_ataf, p=0.75, list = FALSE)
mic03_train= mic03[trainindex, ]
mic03_test= mic03[-trainindex, ]

trainindex = createDataPartition(mor03$pbt_ataf, p=0.75, list = FALSE)
mor03_train= mor03[trainindex, ]
mor03_test= mor03[-trainindex, ]

trainindex = createDataPartition(nay03$pbt_ataf, p=0.75, list = FALSE)
nay03_train= nay03[trainindex, ]
nay03_test= nay03[-trainindex, ]

trainindex = createDataPartition(nle03$pbt_ataf, p=0.75, list = FALSE)
nle03_train= nle03[trainindex, ]
nle03_test= nle03[-trainindex, ]

trainindex = createDataPartition(oax03$pbt_ataf, p=0.75, list = FALSE)
oax03_train= oax03[trainindex, ]
oax03_test= oax03[-trainindex, ]

trainindex = createDataPartition(pue03$pbt_ataf, p=0.75, list = FALSE)
pue03_train= pue03[trainindex, ]
pue03_test= pue03[-trainindex, ]

trainindex = createDataPartition(que03$pbt_ataf, p=0.75, list = FALSE)
que03_train= que03[trainindex, ]
que03_test= que03[-trainindex, ]

trainindex = createDataPartition(qro03$pbt_ataf, p=0.75, list = FALSE)
qro03_train= qro03[trainindex, ]
qro03_test= qro03[-trainindex, ]

trainindex = createDataPartition(slp03$pbt_ataf, p=0.75, list = FALSE)
slp03_train= slp03[trainindex, ]
slp03_test= slp03[-trainindex, ]

trainindex = createDataPartition(sin03$pbt_ataf, p=0.75, list = FALSE)
sin03_train= sin03[trainindex, ]
sin03_test= sin03[-trainindex, ]

trainindex = createDataPartition(son03$pbt_ataf, p=0.75, list = FALSE)
son03_train= son03[trainindex, ]
son03_test= son03[-trainindex, ]

trainindex = createDataPartition(tab03$pbt_ataf, p=0.75, list = FALSE)
tab03_train= tab03[trainindex, ]
tab03_test= tab03[-trainindex, ]

trainindex = createDataPartition(tam03$pbt_ataf, p=0.75, list = FALSE)
tam03_train= tam03[trainindex, ]
tam03_test= tam03[-trainindex, ]

trainindex = createDataPartition(tla03$pbt_ataf, p=0.75, list = FALSE)
tla03_train= tla03[trainindex, ]
tla03_test= tla03[-trainindex, ]

trainindex = createDataPartition(ver03$pbt_ataf, p=0.75, list = FALSE)
ver03_train= ver03[trainindex, ]
ver03_test= ver03[-trainindex, ]

trainindex = createDataPartition(yuc03$pbt_ataf, p=0.75, list = FALSE)
yuc03_train= yuc03[trainindex, ]
yuc03_test= yuc03[-trainindex, ]

trainindex = createDataPartition(zac03$pbt_ataf, p=0.75, list = FALSE)
zac03_train= zac03[trainindex, ]
zac03_test= zac03[-trainindex, ]



# Árboles de decisión

set.seed(1234567890)

arbol_clasificacionagu03 <- rpart(segmentovacb ~ ., data = agu03_train, 
                             minsplit = 2, minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(agu03_train$segmentovacb)
cv_results <- caret::train(x = agu03_train[, -ncol(agu03_train)], y = agu03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adagu03 <- rpart(segmentovacb ~ ., data = agu03_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adagu03, main = "Clasificación del VACB en Aguascalientes año 2003", sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adagu03, agu03_test, type = "class")
confusionMatrix(predat, agu03_test$segmentovacb)



arbol_clasificacionbca03 <- rpart(segmentovacb ~ ., data = bca03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bca03_train$segmentovacb)
cv_results <- caret::train(x = bca03_train[, -ncol(bca03_train)], y = bca03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbca03 <- rpart(segmentovacb ~ ., data = bca03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adbca03, main = "Clasificación del VACB en Baja California año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adbca03, bca03_test, type = "class")
confusionMatrix(predat, bca03_test$segmentovacb)



arbol_clasificacionbcs03 <- rpart(segmentovacb ~ ., data = bcs03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bcs03_train$segmentovacb)
cv_results <- caret::train(x = bcs03_train[, -ncol(bcs03_train)], y = bcs03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbcs03 <- rpart(segmentovacb ~ ., data = bcs03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adbcs03, main = "Clasificación del VACB en Baja California Sur año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adbcs03, bcs03_test, type = "class")
confusionMatrix(predat, bcs03_test$segmentovacb)



arbol_clasificacioncam03 <- rpart(segmentovacb ~ ., data = cam03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cam03_train$segmentovacb)
cv_results <- caret::train(x = cam03_train[, -ncol(cam03_train)], y = cam03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcam03 <- rpart(segmentovacb ~ ., data = cam03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcam03, main = "Clasificación del VACB en Campeche año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adcam03, cam03_test, type = "class")
confusionMatrix(predat, cam03_test$segmentovacb)



arbol_clasificacionchh03 <- rpart(segmentovacb ~ ., data = chh03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chh03_train$segmentovacb)
cv_results <- caret::train(x = chh03_train[, -ncol(chh03_train)], y = chh03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchh03 <- rpart(segmentovacb ~ ., data = chh03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adchh03, main = "Clasificación del VACB en Chihuahua año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adchh03, chh03_test, type = "class")
confusionMatrix(predat, chh03_test$segmentovacb)



arbol_clasificacionchi03 <- rpart(segmentovacb ~ ., data = chi03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chi03_train$segmentovacb)
cv_results <- caret::train(x = chi03_train[, -ncol(chi03_train)], y = chi03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchi03 <- rpart(segmentovacb ~ ., data = chi03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adchi03, main = "Clasificación del VACB en Chiapas año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adchi03, chi03_test, type = "class")
confusionMatrix(predat, chi03_test$segmentovacb)



arbol_clasificacioncmx03 <- rpart(segmentovacb ~ ., data = cmx03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cmx03_train$segmentovacb)
cv_results <- caret::train(x = cmx03_train[, -ncol(cmx03_train)], y = cmx03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcmx03 <- rpart(segmentovacb ~ ., data = cmx03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcmx03, main = "Clasificación del VACB en la Ciudad de México año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adcmx03, cmx03_test, type = "class")
confusionMatrix(predat, cmx03_test$segmentovacb)



arbol_clasificacioncoa03 <- rpart(segmentovacb ~ ., data = coa03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(coa03_train$segmentovacb)
cv_results <- caret::train(x = coa03_train[, -ncol(coa03_train)], y = coa03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcoa03 <- rpart(segmentovacb ~ ., data = coa03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcoa03, main = "Clasificación del VACB en Coahuila año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adcoa03, coa03_test, type = "class")
confusionMatrix(predat, coa03_test$segmentovacb)



arbol_clasificacioncol03 <- rpart(segmentovacb ~ ., data = col03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(col03_train$segmentovacb)
cv_results <- caret::train(x = col03_train[, -ncol(col03_train)], y = col03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcol03 <- rpart(segmentovacb ~ ., data = col03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adcol03, main = "Clasificación del VACB en Colima año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adcol03, col03_test, type = "class")
confusionMatrix(predat, col03_test$segmentovacb)



arbol_clasificaciondur03 <- rpart(segmentovacb ~ ., data = dur03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(dur03_train$segmentovacb)
cv_results <- caret::train(x = dur03_train[, -ncol(dur03_train)], y = dur03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
addur03 <- rpart(segmentovacb ~ ., data = dur03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(addur03, main = "Clasificación del VACB en Durango año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(addur03, dur03_test, type = "class")
confusionMatrix(predat, dur03_test$segmentovacb)



arbol_clasificacionemx03 <- rpart(segmentovacb ~ ., data = emx03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(emx03_train$segmentovacb)
cv_results <- caret::train(x = emx03_train[, -ncol(emx03_train)], y = emx03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
ademx03 <- rpart(segmentovacb ~ ., data = emx03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(ademx03, main = "Clasificación del VACB en el Estado de México año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(ademx03, emx03_test, type = "class")
confusionMatrix(predat, emx03_test$segmentovacb)



arbol_clasificaciongua03 <- rpart(segmentovacb ~ ., data = gua03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gua03_train$segmentovacb)
cv_results <- caret::train(x = gua03_train[, -ncol(gua03_train)], y = gua03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgua03 <- rpart(segmentovacb ~ ., data = gua03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adgua03, main = "Clasificación del VACB en Guanajuato año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adgua03, gua03_test, type = "class")
confusionMatrix(predat, gua03_test$segmentovacb)



arbol_clasificaciongue03 <- rpart(segmentovacb ~ ., data = gue03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gue03_train$segmentovacb)
cv_results <- caret::train(x = gue03_train[, -ncol(gue03_train)], y = gue03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgue03 <- rpart(segmentovacb ~ ., data = gue03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adgue03, main = "Clasificación del VACB en Guerrero año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adgue03, gue03_test, type = "class")
confusionMatrix(predat, gue03_test$segmentovacb)



arbol_clasificacionhid03 <- rpart(segmentovacb ~ ., data = hid03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(hid03_train$segmentovacb)
cv_results <- caret::train(x = hid03_train[, -ncol(hid03_train)], y = hid03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adhid03 <- rpart(segmentovacb ~ ., data = hid03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adhid03, main = "Clasificación del VACB en Hidalgo año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adhid03, hid03_test, type = "class")
confusionMatrix(predat, hid03_test$segmentovacb)



arbol_clasificacionjal03 <- rpart(segmentovacb ~ ., data = jal03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(jal03_train$segmentovacb)
cv_results <- caret::train(x = jal03_train[, -ncol(jal03_train)], y = jal03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adjal03 <- rpart(segmentovacb ~ ., data = jal03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adjal03, main = "Clasificación del VACB en Jalisco año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adjal03, jal03_test, type = "class")
confusionMatrix(predat, jal03_test$segmentovacb)



arbol_clasificacionmic03 <- rpart(segmentovacb ~ ., data = mic03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mic03_train$segmentovacb)
cv_results <- caret::train(x = mic03_train[, -ncol(mic03_train)], y = mic03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admic03 <- rpart(segmentovacb ~ ., data = mic03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(admic03, main = "Clasificación del VACB en Michoacán año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(admic03, mic03_test, type = "class")
confusionMatrix(predat, mic03_test$segmentovacb)



arbol_clasificacionmor03 <- rpart(segmentovacb ~ ., data = mor03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mor03_train$segmentovacb)
cv_results <- caret::train(x = mor03_train[, -ncol(mor03_train)], y = mor03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admor03 <- rpart(segmentovacb ~ ., data = mor03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(admor03, main = "Clasificación del VACB en Morelos año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(admor03, mor03_test, type = "class")
confusionMatrix(predat, mor03_test$segmentovacb)



arbol_clasificacionnay03 <- rpart(segmentovacb ~ ., data = nay03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nay03_train$segmentovacb)
cv_results <- caret::train(x = nay03_train[, -ncol(nay03_train)], y = nay03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnay03 <- rpart(segmentovacb ~ ., data = nay03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adnay03, main = "Clasificación del VACB en Nayarit año 03",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adnay03, nay03_test, type = "class")
confusionMatrix(predat, nay03_test$segmentovacb)



arbol_clasificacionnle03 <- rpart(segmentovacb ~ ., data = nle03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nle03_train$segmentovacb)
cv_results <- caret::train(x = nle03_train[, -ncol(nle03_train)], y = nle03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnle03 <- rpart(segmentovacb ~ ., data = nle03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adnle, main = "Clasificación del VACB en Nuevo León año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adnle03, nle03_test, type = "class")
confusionMatrix(predat, nle03_test$segmentovacb)



arbol_clasificacionoax03 <- rpart(segmentovacb ~ ., data = oax03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(oax03_train$segmentovacb)
cv_results <- caret::train(x = oax03_train[, -ncol(oax03_train)], y = oax03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adoax03 <- rpart(segmentovacb ~ ., data = oax03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adoax03, main = "Clasificación del VACB en Oaxaca año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adoax03, oax03_test, type = "class")
confusionMatrix(predat, oax03_test$segmentovacb)



arbol_clasificacionpue03 <- rpart(segmentovacb ~ ., data = pue03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(pue03_train$segmentovacb)
cv_results <- caret::train(x = pue03_train[, -ncol(pue03_train)], y = pue03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adpue03 <- rpart(segmentovacb ~ ., data = pue03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adpue03, main = "Clasificación del VACB en Puebla año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adpue03, pue03_test, type = "class")
confusionMatrix(predat, pue03_test$segmentovacb)



arbol_clasificacionque03 <- rpart(segmentovacb ~ ., data = que03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(que03_train$segmentovacb)
cv_results <- caret::train(x = que03_train[, -ncol(que03_train)], y = que03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adque03 <- rpart(segmentovacb ~ ., data = que03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adque03, main = "Clasificación del VACB en Querétaro año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adque03, que03_test, type = "class")
confusionMatrix(predat, que03_test$segmentovacb)



arbol_clasificacionqro03 <- rpart(segmentovacb ~ ., data = qro03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(qro03_train$segmentovacb)
cv_results <- caret::train(x = qro03_train[, -ncol(qro03_train)], y = qro03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adqro03 <- rpart(segmentovacb ~ ., data = qro03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adqro03, main = "Clasificación del VACB en Quintana Roo año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adqro03, qro03_test, type = "class")
confusionMatrix(predat, qro03_test$segmentovacb)



arbol_clasificacionslp03 <- rpart(segmentovacb ~ ., data = slp03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(slp03_train$segmentovacb)
cv_results <- caret::train(x = slp03_train[, -ncol(slp03_train)], y = slp03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adslp03 <- rpart(segmentovacb ~ ., data = slp03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adslp03, main = "Clasificación del VACB en San Luis Potosi año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adslp03, slp03_test, type = "class")
confusionMatrix(predat, slp03_test$segmentovacb)



arbol_clasificacionsin03 <- rpart(segmentovacb ~ ., data = sin03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(sin03_train$segmentovacb)
cv_results <- caret::train(x = sin03_train[, -ncol(sin03_train)], y = sin03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adsin03 <- rpart(segmentovacb ~ ., data = sin03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adsin03, main = "Clasificación del VACB en Sinaloa año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adsin03, sin03_test, type = "class")
confusionMatrix(predat, sin03_test$segmentovacb)



arbol_clasificacionson03 <- rpart(segmentovacb ~ ., data = son03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(son03_train$segmentovacb)
cv_results <- caret::train(x = son03_train[, -ncol(son03_train)], y = son03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adson03 <- rpart(segmentovacb ~ ., data = son03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adson03, main = "Clasificación del VACB en Sonora año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adson03, son03_test, type = "class")
confusionMatrix(predat, son03_test$segmentovacb)



arbol_clasificaciontab03 <- rpart(segmentovacb ~ ., data = tab03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tab03_train$segmentovacb)
cv_results <- caret::train(x = tab03_train[, -ncol(tab03_train)], y = tab03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtab03 <- rpart(segmentovacb ~ ., data = tab03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtab03, main = "Clasificación del VACB en Tabasco año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adtab03, tab03_test, type = "class")
confusionMatrix(predat, tab03_test$segmentovacb)



arbol_clasificaciontam03 <- rpart(segmentovacb ~ ., data = tam03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tam03_train$segmentovacb)
cv_results <- caret::train(x = tam03_train[, -ncol(tam03_train)], y = tam03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtam03 <- rpart(segmentovacb ~ ., data = tam03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtam03, main = "Clasificación del VACB en Tamaulipas año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adtam03, tam03_test, type = "class")
confusionMatrix(predat, tam03_test$segmentovacb)



arbol_clasificaciontla03 <- rpart(segmentovacb ~ ., data = tla03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tla03_train$segmentovacb)
cv_results <- caret::train(x = tla03_train[, -ncol(tla03_train)], y = tla03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtla03 <- rpart(segmentovacb ~ ., data = tla03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adtla03, main = "Clasificación del VACB en Tlaxcala año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adtla03, tla03_test, type = "class")
confusionMatrix(predat, tla03_test$segmentovacb)



arbol_clasificacionver03 <- rpart(segmentovacb ~ ., data = ver03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(ver03_train$segmentovacb)
cv_results <- caret::train(x = ver03_train[, -ncol(ver03_train)], y = ver03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adver03 <- rpart(segmentovacb ~ ., data = ver03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adver03, main = "Clasificación del VACB en Veracruz año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adver03, ver03_test, type = "class")
confusionMatrix(predat, ver03_test$segmentovacb)



arbol_clasificacionyuc03 <- rpart(segmentovacb ~ ., data = yuc03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(yuc03_train$segmentovacb)
cv_results <- caret::train(x = yuc03_train[, -ncol(yuc03_train)], y = yuc03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adyuc03 <- rpart(segmentovacb ~ ., data = yuc03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adyuc03, main = "Clasificación del VACB en Yucatán año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adyuc03, yuc03_test, type = "class")
confusionMatrix(predat, yuc03_test$segmentovacb)



arbol_clasificacionzac03 <- rpart(segmentovacb ~ ., data = zac03_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(zac03_train$segmentovacb)
cv_results <- caret::train(x = zac03_train[, -ncol(zac03_train)], y = zac03_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adzac03 <- rpart(segmentovacb ~ ., data = zac03_train, method = "class", 
               control = control_poda)
fancyRpartPlot(adzac03, main = "Clasificación del VACB en Zacatecas año 2003",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2004: INEGI")
predat <- predict(adzac03, zac03_test, type = "class")
confusionMatrix(predat, zac03_test$segmentovacb)



impadagu03 <- varImp(adagu03)
impadbca03 <- varImp(adbca03)
impadbcs03 <- varImp(adbcs03)
impadcam03 <- varImp(adcam03)
impadchh03 <- varImp(adchh03)
impadchi03 <- varImp(adchi03)
impadcmx03 <- varImp(adcmx03)
impadcoa03 <- varImp(adcoa03)
impadcol03 <- varImp(adcol03)
impaddur03 <- varImp(addur03)
impademx03 <- varImp(ademx03)
impadgua03 <- varImp(adgua03)
impadgue03 <- varImp(adgue03)
impadhid03 <- varImp(adhid03)
impadjal03 <- varImp(adjal03)
impadmic03 <- varImp(admic03)
impadmor03 <- varImp(admor03)
impadnay03 <- varImp(adnay03)
impadnle03 <- varImp(adnle03)
impadoax03 <- varImp(adoax03)
impadpue03 <- varImp(adpue03)
impadqro03 <- varImp(adqro03)
impadque03 <- varImp(adque03)
impadsin03 <- varImp(adsin03)
impadslp03 <- varImp(adslp03)
impadson03 <- varImp(adson03)
impadtab03 <- varImp(adtab03)
impadtam03 <- varImp(adtam03)
impadtla03 <- varImp(adtla03)
impadver03 <- varImp(adver03)
impadyuc03 <- varImp(adyuc03)
impadzac03 <- varImp(adzac03)

agu003<-impadagu03 
bca003<-impadbca03 
bcs003<-impadbcs03 
cam003<-impadcam03 
chh003<-impadchh03
chi003<-impadchi03 
cmx003<-impadcmx03 
coa003<-impadcoa03 
col003<-impadcol03
dur003<-impaddur03 
emx003<-impademx03 
gua003<-impadgua03 
gue003<-impadgue03 
hid003<-impadhid03
jal003<-impadjal03 
mic003<-impadmic03 
mor003<-impadmor03 
nay003<-impadnay03 
nle003<-impadnle03
oax003<-impadoax03 
pue003<-impadpue03 
qro003<-impadqro03 
que003<-impadque03 
sin003<-impadsin03
son003<-impadson03 
slp003<-impadslp03 
tab003<-impadtab03 
tam003<-impadtam03 
tla003<-impadtla03
ver003<-impadver03 
yuc003<-impadyuc03 
zac003<-impadzac03



d103 <- data.frame(agu003, bca003, bcs003, cam003, chh003, chi003,
                 cmx003, coa003, col003,
                 dur003, emx003, gua003, gue003, hid003,
                 jal003, mic003, mor003, nay003, nle003, oax003,
                 pue003, qro003, que003, sin003,
                 son003, slp003, tab003, tam003, tla003,
                 ver003, yuc003, zac003)
names(d103) <- c("AGU", "BCA", "BCS", "CAM", "CHH", "CHI","CMX", "COA", "COL","DUR",
               "EMX", "GUA", "GUE", "HID", "JAL", "MIC", "MOR", "NAY","NLE", "OAX",
               "PUE", "QRO", "QUE", "SIN", "SON", "SLP", "TAB", "TAM", "TLA","VER", "YUC", "ZAC")


dt003 <- as.matrix(d103)

ppt03 <- prop.table(dt003, margin = 1)

barplot(ppt03)

write.csv(dt003,"pesos03.csv")

dt0003 <- t(dt003)

dcc03 <- as.data.frame(dt0003)

plot_num(dcc03)

con03 <- select(dcc03, AE:vacb_atecp)


#Conglomerados

set.seed(12345678)

clustering03 <- kmeans(con03, centers = 10)

correlations03 <- cor(con03, clustering03$cluster)

print(correlations03)

fviz_nbclust(x = con03, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(con13, method = "euclidean"), nstart = 50)

km_clusters03 <- kmeans(x = con03, centers = 7, nstart = 50)

fviz_cluster(object = km_clusters03, data = con, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Agrupación por capacidades productivas físicas: K-means") +
  theme_bw() +
  theme(legend.position = "none")


res.dist03 <- get_dist(con03, stand = FALSE, 
                       method = "euclidean")

fviz_dist(res.dist03, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))

corrplot(as.matrix(res.dist03), is.corr = FALSE, 
         method = "color",
         order = "hclust", type = "upper")

d03 <- as.matrix(res.dist03)

heatmap(d03, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2003")


res.hc03 <- hclust(res.dist03, "ward.D" )

agrup03 <- fviz_dend(res.hc03, cex = 0.5, k = 9, rect = TRUE,
          k_colors = "npg", 
          color_labels_by_k = TRUE,
          main = "Figura 4. Agrupación estatal por capacidades productivas año 2003",
          xlab = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI",
          ylab = "Pesos en la matriz de distancia",
          sub = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI")

agrup03

grp03 <- cutree(res.hc03, k = 9)

fviz_cluster(list(data = con03, cluster = grp03),
             ellipse.type = "convex",  rect = TRUE,
             k_colors = "aaas", 
             color_labels_by_k = TRUE,
             repel = T, show.clust.cent = T, 
             ggtheme = theme_minimal(), main = "Agrupación por capacidades productivas año 2003",
             xlab = "fbcf_ataf", ylab = "vacb_htpot")

plot(as.phylo(res.hc03), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc03), type = "unrooted", cex = 0.7,
     no.margin = T)


fviz_dend(res.hc03, k=9, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc03, cex= 0.8, k=9,
          k_colors =  "lancet", 
          color_labels_by_k = TRUE,
          type="circular", title = "Figura 4. Agrupación estatal por desempeño y capacidades productivas año 2003",
          xlab = "Entidades Federativas", 
          sub = "Fuente: Elaboración propia con datos del Censo Económico 2004: INEGI")

plot(as.phylo(res.hc03), type = "radial")
