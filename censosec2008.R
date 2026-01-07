psych::describe(c2008$VACB)

c2008$segmentovacb <- as.factor(cut(c2008$VACB, breaks=c(-7713, 25.92, 400.39, 7906, 571213), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))


ca08 <- select(c2008, ID, EF, pijataf:segmentovacb) %>% column_to_rownames(., var = 'ID')

ca08 <- ca[complete.cases(ca08$pbt_ataf), ]

agu08 <- filter(ca08, EF %in% c("AGU"))  
bca08 <- filter(ca08, EF %in% c("BCA"))
bcs08 <- filter(ca08, EF %in% c("BCS"))
cam08 <- filter(ca08, EF %in% c("CAM"))
coa08 <- filter(ca08, EF %in% c("COA"))
col08 <- filter(ca08, EF %in% c("COL"))
chi08 <- filter(ca08, EF %in% c("CHI"))
chh08 <- filter(ca08, EF %in% c("CHH"))
cmx08 <- filter(ca08, EF %in% c("CMX"))
dur08 <- filter(ca08, EF %in% c("DUR"))
gua08 <- filter(ca08, EF %in% c("GUA"))
gue08 <- filter(ca08, EF %in% c("GUE"))
hid08 <- filter(ca08, EF %in% c("HID"))
jal08 <- filter(ca08, EF %in% c("JAL"))
emx08 <- filter(ca08, EF %in% c("EMX"))
mic08 <- filter(ca08, EF %in% c("MIC"))
mor08 <- filter(ca08, EF %in% c("MOR"))
nay08 <- filter(ca08, EF %in% c("NAY"))
nle08 <- filter(ca08, EF %in% c("NLE"))
oax08 <- filter(ca08, EF %in% c("OAX"))
pue08 <- filter(ca08, EF %in% c("PUE"))
que08 <- filter(ca08, EF %in% c("QUE"))
qro08 <- filter(ca08, EF %in% c("QRO"))
slp08 <- filter(ca08, EF %in% c("SLP"))
sin08 <- filter(ca08, EF %in% c("SIN"))
son08 <- filter(ca08, EF %in% c("SON"))
tab08 <- filter(ca08, EF %in% c("TAB"))
tam08 <- filter(ca08, EF %in% c("TAM"))
tla08 <- filter(ca08, EF %in% c("TLA"))
ver08 <- filter(ca08, EF %in% c("VER"))
yuc08 <- filter(ca08, EF %in% c("YUC"))
zac08 <- filter(ca08, EF %in% c("ZAC"))

agu08$EF <- NULL
bca08$EF <- NULL
bcs08$EF <- NULL
cam08$EF <- NULL
chh08$EF <- NULL
chi08$EF <- NULL
cmx08$EF <- NULL
coa08$EF <- NULL
col08$EF <- NULL
dur08$EF <- NULL
emx08$EF <- NULL
gua08$EF <- NULL
gue08$EF <- NULL
hid08$EF <- NULL
jal08$EF <- NULL
mic08$EF <- NULL
mor08$EF <- NULL
nay08$EF <- NULL
nle08$EF <- NULL
oax08$EF <- NULL
pue08$EF <- NULL
qro08$EF <- NULL
que08$EF <- NULL
sin08$EF <- NULL
son08$EF <- NULL
slp08$EF <- NULL
tab08$EF <- NULL
tam08$EF <- NULL
tla08$EF <- NULL
ver08$EF <- NULL
yuc08$EF <- NULL
zac08$EF <- NULL

set.seed(987654321)

trainindex = createDataPartition(agu08$pbt_ataf, p=0.75, list = FALSE)
agu08_train= agu08[trainindex, ]
agu08_test= agu08[-trainindex, ]

trainindex = createDataPartition(bca08$pbt_ataf, p=0.75, list = FALSE)
bca08_train= bca08[trainindex, ]
bca08_test= bca08[-trainindex, ]

trainindex = createDataPartition(bcs08$pbt_ataf, p=0.75, list = FALSE)
bcs08_train= bcs08[trainindex, ]
bcs08_test= bcs08[-trainindex, ]

trainindex = createDataPartition(cam08$pbt_ataf, p=0.75, list = FALSE)
cam08_train= cam08[trainindex, ]
cam08_test= cam08[-trainindex, ]

trainindex = createDataPartition(coa08$pbt_ataf, p=0.75, list = FALSE)
coa08_train= coa08[trainindex, ]
coa08_test= coa08[-trainindex, ]

trainindex = createDataPartition(col08$pbt_ataf, p=0.75, list = FALSE)
col08_train= col08[trainindex, ]
col08_test= col08[-trainindex, ]

trainindex = createDataPartition(chi08$pbt_ataf, p=0.75, list = FALSE)
chi08_train= chi08[trainindex, ]
chi08_test= chi08[-trainindex, ]

trainindex = createDataPartition(chh08$pbt_ataf, p=0.75, list = FALSE)
chh08_train= chh08[trainindex, ]
chh08_test= chh08[-trainindex, ]

trainindex = createDataPartition(cmx08$pbt_ataf, p=0.75, list = FALSE)
cmx08_train= cmx08[trainindex, ]
cmx08_test= cmx08[-trainindex, ]

trainindex = createDataPartition(dur08$pbt_ataf, p=0.75, list = FALSE)
dur08_train= dur08[trainindex, ]
dur08_test= dur08[-trainindex, ]

trainindex = createDataPartition(gua08$pbt_ataf, p=0.75, list = FALSE)
gua08_train= gua08[trainindex, ]
gua08_test= gua08[-trainindex, ]

trainindex = createDataPartition(gue08$pbt_ataf, p=0.75, list = FALSE)
gue08_train= gue08[trainindex, ]
gue08_test= gue08[-trainindex, ]

trainindex = createDataPartition(hid08$pbt_ataf, p=0.75, list = FALSE)
hid08_train= hid08[trainindex, ]
hid08_test= hid08[-trainindex, ]

trainindex = createDataPartition(jal08$pbt_ataf, p=0.75, list = FALSE)
jal08_train= jal08[trainindex, ]
jal08_test= jal08[-trainindex, ]

trainindex = createDataPartition(emx08$pbt_ataf, p=0.75, list = FALSE)
emx08_train= emx08[trainindex, ]
emx08_test= emx08[-trainindex, ]

trainindex = createDataPartition(mic08$pbt_ataf, p=0.75, list = FALSE)
mic08_train= mic08[trainindex, ]
mic08_test= mic08[-trainindex, ]

trainindex = createDataPartition(mor08$pbt_ataf, p=0.75, list = FALSE)
mor08_train= mor08[trainindex, ]
mor08_test= mor08[-trainindex, ]

trainindex = createDataPartition(nay08$pbt_ataf, p=0.75, list = FALSE)
nay08_train= nay08[trainindex, ]
nay08_test= nay08[-trainindex, ]

trainindex = createDataPartition(nle08$pbt_ataf, p=0.75, list = FALSE)
nle08_train= nle08[trainindex, ]
nle08_test= nle08[-trainindex, ]

trainindex = createDataPartition(oax08$pbt_ataf, p=0.75, list = FALSE)
oax08_train= oax08[trainindex, ]
oax08_test= oax08[-trainindex, ]

trainindex = createDataPartition(pue08$pbt_ataf, p=0.75, list = FALSE)
pue08_train= pue08[trainindex, ]
pue08_test= pue08[-trainindex, ]

trainindex = createDataPartition(que08$pbt_ataf, p=0.75, list = FALSE)
que08_train= que08[trainindex, ]
que08_test= que08[-trainindex, ]

trainindex = createDataPartition(qro08$pbt_ataf, p=0.75, list = FALSE)
qro08_train= qro08[trainindex, ]
qro08_test= qro08[-trainindex, ]

trainindex = createDataPartition(slp08$pbt_ataf, p=0.75, list = FALSE)
slp08_train= slp08[trainindex, ]
slp08_test= slp08[-trainindex, ]

trainindex = createDataPartition(sin08$pbt_ataf, p=0.75, list = FALSE)
sin08_train= sin08[trainindex, ]
sin08_test= sin08[-trainindex, ]

trainindex = createDataPartition(son08$pbt_ataf, p=0.75, list = FALSE)
son08_train= son08[trainindex, ]
son08_test= son08[-trainindex, ]

trainindex = createDataPartition(tab08$pbt_ataf, p=0.75, list = FALSE)
tab08_train= tab08[trainindex, ]
tab08_test= tab08[-trainindex, ]

trainindex = createDataPartition(tam08$pbt_ataf, p=0.75, list = FALSE)
tam08_train= tam08[trainindex, ]
tam08_test= tam08[-trainindex, ]

trainindex = createDataPartition(tla08$pbt_ataf, p=0.75, list = FALSE)
tla08_train= tla08[trainindex, ]
tla08_test= tla08[-trainindex, ]

trainindex = createDataPartition(ver08$pbt_ataf, p=0.75, list = FALSE)
ver08_train= ver08[trainindex, ]
ver08_test= ver08[-trainindex, ]

trainindex = createDataPartition(yuc08$pbt_ataf, p=0.75, list = FALSE)
yuc08_train= yuc08[trainindex, ]
yuc08_test= yuc08[-trainindex, ]

trainindex = createDataPartition(zac08$pbt_ataf, p=0.75, list = FALSE)
zac08_train= zac08[trainindex, ]
zac08_test= zac08[-trainindex, ]



# Árboles de decisión

set.seed(1234567890)

arbol_clasificacionagu08 <- rpart(segmentovacb ~ ., data = agu08_train, 
                             minsplit = 2, minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(agu08_train$segmentovacb)
cv_results <- caret::train(x = agu08_train[, -ncol(agu08_train)], y = agu08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adagu08 <- rpart(segmentovacb ~ ., data = agu08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adagu08, main = "Clasificación del VACB en Aguascalientes año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adagu08, agu08_test, type = "class")
confusionMatrix(predat, agu08_test$segmentovacb)



arbol_clasificacionbca08 <- rpart(segmentovacb ~ ., data = bca08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bca08_train$segmentovacb)
cv_results <- caret::train(x = bca08_train[, -ncol(bca08_train)], y = bca08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbca08 <- rpart(segmentovacb ~ ., data = bca08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adbca08, main = "Clasificación del VACB en Baja California año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adbca08, bca08_test, type = "class")
confusionMatrix(predat, bca08_test$segmentovacb)



arbol_clasificacionbcs08 <- rpart(segmentovacb ~ ., data = bcs08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(bcs08_train$segmentovacb)
cv_results <- caret::train(x = bcs08_train[, -ncol(bcs08_train)], y = bcs08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adbcs08 <- rpart(segmentovacb ~ ., data = bcs08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adbcs08, main = "Clasificación del VACB en Baja California Sur año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adbcs08, bcs08_test, type = "class")
confusionMatrix(predat, bcs08_test$segmentovacb)



arbol_clasificacioncam08 <- rpart(segmentovacb ~ ., data = cam08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cam08_train$segmentovacb)
cv_results <- caret::train(x = cam08_train[, -ncol(cam08_train)], y = cam08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcam08 <- rpart(segmentovacb ~ ., data = cam08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcam08, main = "Clasificación del VACB en Campeche año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adcam08, cam08_test, type = "class")
confusionMatrix(predat, cam08_test$segmentovacb)



arbol_clasificacionchh08 <- rpart(segmentovacb ~ ., data = chh08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chh08_train$segmentovacb)
cv_results <- caret::train(x = chh08_train[, -ncol(chh08_train)], y = chh08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchh08 <- rpart(segmentovacb ~ ., data = chh08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adchh08, main = "Clasificación del VACB en Chihuahua año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adchh08, chh08_test, type = "class")
confusionMatrix(predat, chh08_test$segmentovacb)



arbol_clasificacionchi08 <- rpart(segmentovacb ~ ., data = chi08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(chi08_train$segmentovacb)
cv_results <- caret::train(x = chi08_train[, -ncol(chi08_train)], y = chi08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adchi08 <- rpart(segmentovacb ~ ., data = chi08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adchi08, main = "Clasificación del VACB en Chiapas año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adchi08, chi08_test, type = "class")
confusionMatrix(predat, chi08_test$segmentovacb)



arbol_clasificacioncmx08 <- rpart(segmentovacb ~ ., data = cmx08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(cmx08_train$segmentovacb)
cv_results <- caret::train(x = cmx08_train[, -ncol(cmx08_train)], y = cmx08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcmx08 <- rpart(segmentovacb ~ ., data = cmx08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcmx08, main = "Clasificación del VACB en la Ciudad de México año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adcmx08, cmx08_test, type = "class")
confusionMatrix(predat, cmx08_test$segmentovacb)



arbol_clasificacioncoa08 <- rpart(segmentovacb ~ ., data = coa08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(coa08_train$segmentovacb)
cv_results <- caret::train(x = coa08_train[, -ncol(coa08_train)], y = coa08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcoa08 <- rpart(segmentovacb ~ ., data = coa08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcoa08, main = "Clasificación del VACB en Coahuila año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adcoa08, coa08_test, type = "class")
confusionMatrix(predat, coa08_test$segmentovacb)



arbol_clasificacioncol08 <- rpart(segmentovacb ~ ., data = col08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(col08_train$segmentovacb)
cv_results <- caret::train(x = col08_train[, -ncol(col08_train)], y = col08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adcol08 <- rpart(segmentovacb ~ ., data = col08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adcol08, main = "Clasificación del VACB en Colima año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adcol08, col08_test, type = "class")
confusionMatrix(predat, col08_test$segmentovacb)



arbol_clasificaciondur08 <- rpart(segmentovacb ~ ., data = dur08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(dur08_train$segmentovacb)
cv_results <- caret::train(x = dur08_train[, -ncol(dur08_train)], y = dur08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
addur08 <- rpart(segmentovacb ~ ., data = dur08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(addur08, main = "Clasificación del VACB en Durango año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(addur08, dur08_test, type = "class")
confusionMatrix(predat, dur08_test$segmentovacb)



arbol_clasificacionemx08 <- rpart(segmentovacb ~ ., data = emx08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(emx08_train$segmentovacb)
cv_results <- caret::train(x = emx08_train[, -ncol(emx08_train)], y = emx08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
ademx08 <- rpart(segmentovacb ~ ., data = emx08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(ademx08, main = "Clasificación del VACB en el Estado de México año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(ademx08, emx08_test, type = "class")
confusionMatrix(predat, emx08_test$segmentovacb)



arbol_clasificaciongua08 <- rpart(segmentovacb ~ ., data = gua08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gua08_train$segmentovacb)
cv_results <- caret::train(x = gua08_train[, -ncol(gua08_train)], y = gua08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgua08 <- rpart(segmentovacb ~ ., data = gua08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adgua08, main = "Clasificación del VACB en Guanajuato año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adgua08, gua08_test, type = "class")
confusionMatrix(predat, gua08_test$segmentovacb)



arbol_clasificaciongue08 <- rpart(segmentovacb ~ ., data = gue08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(gue08_train$segmentovacb)
cv_results <- caret::train(x = gue08_train[, -ncol(gue08_train)], y = gue08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adgue08 <- rpart(segmentovacb ~ ., data = gue08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adgue08, main = "Clasificación del VACB en Guerrero año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adgue08, gue08_test, type = "class")
confusionMatrix(predat, gue08_test$segmentovacb)



arbol_clasificacionhid08 <- rpart(segmentovacb ~ ., data = hid08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(hid08_train$segmentovacb)
cv_results <- caret::train(x = hid08_train[, -ncol(hid08_train)], y = hid08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adhid08 <- rpart(segmentovacb ~ ., data = hid08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adhid08, main = "Clasificación del VACB en Hidalgo año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adhid08, hid08_test, type = "class")
confusionMatrix(predat, hid08_test$segmentovacb)



arbol_clasificacionjal08 <- rpart(segmentovacb ~ ., data = jal08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(jal08_train$segmentovacb)
cv_results <- caret::train(x = jal08_train[, -ncol(jal08_train)], y = jal08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adjal08 <- rpart(segmentovacb ~ ., data = jal08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adjal08, main = "Clasificación del VACB en Jalisco año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adjal08, jal08_test, type = "class")
confusionMatrix(predat, jal08_test$segmentovacb)



arbol_clasificacionmic08 <- rpart(segmentovacb ~ ., data = mic08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mic08_train$segmentovacb)
cv_results <- caret::train(x = mic08_train[, -ncol(mic08_train)], y = mic08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admic08 <- rpart(segmentovacb ~ ., data = mic08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(admic08, main = "Clasificación del VACB en Michoacán año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(admic08, mic08_test, type = "class")
confusionMatrix(predat, mic08_test$segmentovacb)



arbol_clasificacionmor08 <- rpart(segmentovacb ~ ., data = mor08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(mor08_train$segmentovacb)
cv_results <- caret::train(x = mor08_train[, -ncol(mor08_train)], y = mor08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
admor08 <- rpart(segmentovacb ~ ., data = mor08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(admor08, main = "Clasificación del VACB en Morelos año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(admor08, mor08_test, type = "class")
confusionMatrix(predat, mor08_test$segmentovacb)



arbol_clasificacionnay08 <- rpart(segmentovacb ~ ., data = nay08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nay08_train$segmentovacb)
cv_results <- caret::train(x = nay08_train[, -ncol(nay08_train)], y = nay08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnay08 <- rpart(segmentovacb ~ ., data = nay08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adnay08, main = "Clasificación del VACB en Nayarit año 08",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adnay08, nay08_test, type = "class")
confusionMatrix(predat, nay08_test$segmentovacb)



arbol_clasificacionnle08 <- rpart(segmentovacb ~ ., data = nle08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(nle08_train$segmentovacb)
cv_results <- caret::train(x = nle08_train[, -ncol(nle08_train)], y = nle08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adnle08 <- rpart(segmentovacb ~ ., data = nle08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adnle, main = "Clasificación del VACB en Nuevo León año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adnle08, nle08_test, type = "class")
confusionMatrix(predat, nle08_test$segmentovacb)



arbol_clasificacionoax08 <- rpart(segmentovacb ~ ., data = oax08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(oax08_train$segmentovacb)
cv_results <- caret::train(x = oax08_train[, -ncol(oax08_train)], y = oax08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adoax08 <- rpart(segmentovacb ~ ., data = oax08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adoax08, main = "Clasificación del VACB en Oaxaca año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adoax08, oax08_test, type = "class")
confusionMatrix(predat, oax08_test$segmentovacb)



arbol_clasificacionpue08 <- rpart(segmentovacb ~ ., data = pue08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(pue08_train$segmentovacb)
cv_results <- caret::train(x = pue08_train[, -ncol(pue08_train)], y = pue08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adpue08 <- rpart(segmentovacb ~ ., data = pue08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adpue08, main = "Clasificación del VACB en Puebla año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adpue08, pue08_test, type = "class")
confusionMatrix(predat, pue08_test$segmentovacb)



arbol_clasificacionque08 <- rpart(segmentovacb ~ ., data = que08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(que08_train$segmentovacb)
cv_results <- caret::train(x = que08_train[, -ncol(que08_train)], y = que08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adque08 <- rpart(segmentovacb ~ ., data = que08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adque08, main = "Clasificación del VACB en Querétaro año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adque08, que08_test, type = "class")
confusionMatrix(predat, que08_test$segmentovacb)



arbol_clasificacionqro08 <- rpart(segmentovacb ~ ., data = qro08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(qro08_train$segmentovacb)
cv_results <- caret::train(x = qro08_train[, -ncol(qro08_train)], y = qro08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adqro08 <- rpart(segmentovacb ~ ., data = qro08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adqro08, main = "Clasificación del VACB en Quintana Roo año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adqro08, qro08_test, type = "class")
confusionMatrix(predat, qro08_test$segmentovacb)



arbol_clasificacionslp08 <- rpart(segmentovacb ~ ., data = slp08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(slp08_train$segmentovacb)
cv_results <- caret::train(x = slp08_train[, -ncol(slp08_train)], y = slp08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adslp08 <- rpart(segmentovacb ~ ., data = slp08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adslp08, main = "Clasificación del VACB en San Luis Potosi año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adslp08, slp08_test, type = "class")
confusionMatrix(predat, slp08_test$segmentovacb)



arbol_clasificacionsin08 <- rpart(segmentovacb ~ ., data = sin08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(sin08_train$segmentovacb)
cv_results <- caret::train(x = sin08_train[, -ncol(sin08_train)], y = sin08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adsin08 <- rpart(segmentovacb ~ ., data = sin08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adsin08, main = "Clasificación del VACB en Sinaloa año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adsin08, sin08_test, type = "class")
confusionMatrix(predat, sin08_test$segmentovacb)



arbol_clasificacionson08 <- rpart(segmentovacb ~ ., data = son08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(son08_train$segmentovacb)
cv_results <- caret::train(x = son08_train[, -ncol(son08_train)], y = son08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adson08 <- rpart(segmentovacb ~ ., data = son08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adson08, main = "Clasificación del VACB en Sonora año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adson08, son08_test, type = "class")
confusionMatrix(predat, son08_test$segmentovacb)



arbol_clasificaciontab08 <- rpart(segmentovacb ~ ., data = tab08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tab08_train$segmentovacb)
cv_results <- caret::train(x = tab08_train[, -ncol(tab08_train)], y = tab08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtab08 <- rpart(segmentovacb ~ ., data = tab08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtab08, main = "Clasificación del VACB en Tabasco año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adtab08, tab08_test, type = "class")
confusionMatrix(predat, tab08_test$segmentovacb)



arbol_clasificaciontam08 <- rpart(segmentovacb ~ ., data = tam08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tam08_train$segmentovacb)
cv_results <- caret::train(x = tam08_train[, -ncol(tam08_train)], y = tam08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtam08 <- rpart(segmentovacb ~ ., data = tam08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtam08, main = "Clasificación del VACB en Tamaulipas año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adtam08, tam08_test, type = "class")
confusionMatrix(predat, tam08_test$segmentovacb)



arbol_clasificaciontla08 <- rpart(segmentovacb ~ ., data = tla08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(tla08_train$segmentovacb)
cv_results <- caret::train(x = tla08_train[, -ncol(tla08_train)], y = tla08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adtla08 <- rpart(segmentovacb ~ ., data = tla08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adtla08, main = "Clasificación del VACB en Tlaxcala año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adtla08, tla08_test, type = "class")
confusionMatrix(predat, tla08_test$segmentovacb)



arbol_clasificacionver08 <- rpart(segmentovacb ~ ., data = ver08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(ver08_train$segmentovacb)
cv_results <- caret::train(x = ver08_train[, -ncol(ver08_train)], y = ver08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adver08 <- rpart(segmentovacb ~ ., data = ver08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adver08, main = "Clasificación del VACB en Veracruz año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adver08, ver08_test, type = "class")
confusionMatrix(predat, ver08_test$segmentovacb)



arbol_clasificacionyuc08 <- rpart(segmentovacb ~ ., data = yuc08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(yuc08_train$segmentovacb)
cv_results <- caret::train(x = yuc08_train[, -ncol(yuc08_train)], y = yuc08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adyuc08 <- rpart(segmentovacb ~ ., data = yuc08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adyuc08, main = "Clasificación del VACB en Yucatán año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adyuc08, yuc08_test, type = "class")
confusionMatrix(predat, yuc08_test$segmentovacb)



arbol_clasificacionzac08 <- rpart(segmentovacb ~ ., data = zac08_train, minsplit = 2, 
                             minbucket = 1, cp = 0)
cv_arbol <- trainControl(method = "cv", number = 10)
classLevels <- levels(zac08_train$segmentovacb)
cv_results <- caret::train(x = zac08_train[, -ncol(zac08_train)], y = zac08_train$segmentovacb,
                           method = "rpart", trControl = cv_arbol, tuneGrid = data.frame(cp = 0),
                           preProcess = c("center", "scale"))
size_optimo <- cv_results$bestTune$cp
control_poda <- rpart.control(cp = size_optimo, minsplit = 3)
adzac08 <- rpart(segmentovacb ~ ., data = zac08_train, method = "class", 
                 control = control_poda)
fancyRpartPlot(adzac08, main = "Clasificación del VACB en Zacatecas año 2008",
               sub = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI")
predat <- predict(adzac08, zac08_test, type = "class")
confusionMatrix(predat, zac08_test$segmentovacb)



impadagu08 <- varImp(adagu08)
impadbca08 <- varImp(adbca08)
impadbcs08 <- varImp(adbcs08)
impadcam08 <- varImp(adcam08)
impadchh08 <- varImp(adchh08)
impadchi08 <- varImp(adchi08)
impadcmx08 <- varImp(adcmx08)
impadcoa08 <- varImp(adcoa08)
impadcol08 <- varImp(adcol08)
impaddur08 <- varImp(addur08)
impademx08 <- varImp(ademx08)
impadgua08 <- varImp(adgua08)
impadgue08 <- varImp(adgue08)
impadhid08 <- varImp(adhid08)
impadjal08 <- varImp(adjal08)
impadmic08 <- varImp(admic08)
impadmor08 <- varImp(admor08)
impadnay08 <- varImp(adnay08)
impadnle08 <- varImp(adnle08)
impadoax08 <- varImp(adoax08)
impadpue08 <- varImp(adpue08)
impadqro08 <- varImp(adqro08)
impadque08 <- varImp(adque08)
impadsin08 <- varImp(adsin08)
impadslp08 <- varImp(adslp08)
impadson08 <- varImp(adson08)
impadtab08 <- varImp(adtab08)
impadtam08 <- varImp(adtam08)
impadtla08 <- varImp(adtla08)
impadver08 <- varImp(adver08)
impadyuc08 <- varImp(adyuc08)
impadzac08 <- varImp(adzac08)

agu008<-impadagu08 
bca008<-impadbca08 
bcs008<-impadbcs08 
cam008<-impadcam08 
chh008<-impadchh08
chi008<-impadchi08 
cmx008<-impadcmx08 
coa008<-impadcoa08 
col008<-impadcol08
dur008<-impaddur08 
emx008<-impademx08 
gua008<-impadgua08 
gue008<-impadgue08 
hid008<-impadhid08
jal008<-impadjal08 
mic008<-impadmic08 
mor008<-impadmor08 
nay008<-impadnay08 
nle008<-impadnle08
oax008<-impadoax08 
pue008<-impadpue08 
qro008<-impadqro08 
que008<-impadque08 
sin008<-impadsin08
son008<-impadson08 
slp008<-impadslp08 
tab008<-impadtab08 
tam008<-impadtam08 
tla008<-impadtla08
ver008<-impadver08 
yuc008<-impadyuc08 
zac008<-impadzac08



d108 <- data.frame(agu008, bca008, bcs008, cam008, chh008, chi008,
                   cmx008, coa008, col008,
                   dur008, emx008, gua008, gue008, hid008,
                   jal008, mic008, mor008, nay008, nle008, oax008,
                   pue008, qro008, que008, sin008,
                   son008, slp008, tab008, tam008, tla008,
                   ver008, yuc008, zac008)
names(d108) <- c("AGU", "BCA", "BCS", "CAM", "CHH", "CHI","CMX", "COA", "COL","DUR",
                 "EMX", "GUA", "GUE", "HID", "JAL", "MIC", "MOR", "NAY","NLE", "OAX",
                 "PUE", "QRO", "QUE", "SIN", "SON", "SLP", "TAB", "TAM", "TLA","VER", "YUC", "ZAC")


dt008 <- as.matrix(d108)

ppt08 <- prop.table(dt008, margin = 1)

barplot(ppt08)

write.csv(dt008,"pesos08.csv")

dt0008 <- t(dt008)

dcc08 <- as.data.frame(dt0008)

plot_num(dcc08)

con08 <- select(dcc08, AE:fbcf_ataf)



# Conglomerados

set.seed(12345678)

clustering08 <- kmeans(con08, centers = 10)

correlations08 <- cor(con08, clustering08$cluster)

print(correlations08)

fviz_nbclust(x = con08, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(con08, method = "euclidean"), nstart = 50)

km_clusters08 <- kmeans(x = con08, centers = 7, nstart = 50)

fviz_cluster(object = km_clusters08, data = con08, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Agrupación por capacidades productivas físicas: K-means") +
  theme_bw() +
  theme(legend.position = "none")


res.dist08 <- get_dist(con08, stand = FALSE, 
                     method = "euclidean")

fviz_dist(res.dist08, 
          gradient = list(low = "#00AFBB", mid = "white", 
                          high = "#FC4E07"))

corrplot(as.matrix(res.dist08), is.corr = FALSE, 
         method = "color",
         order = "hclust", type = "upper")

d08 <- as.matrix(res.dist08)

heatmap(d08, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2008")


res.hc08 <- hclust(res.dist08, "ward.D" )

agrup08 <- fviz_dend(res.hc08, cex = 0.5, k = 5, rect = TRUE,
          k_colors = "jco", 
          color_labels_by_k = TRUE,
          main = "Figura 6. Agrupación estatal por capacidades productivas año 2008",
          xlab = "Fuente: Elaboración propia con datos del Censo Económico 2009: INEGI",
          ylab = "Pesos en la matriz de distancia",
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

agrup08

grp08 <- cutree(res.hc08, k = 5)

fviz_cluster(list(data = con08, cluster = grp08),
             ellipse.type = "convex",  rect = TRUE,
             k_colors =  "ucscgb", 
             color_labels_by_k = TRUE,
             repel = T, show.clust.cent = T, 
             ggtheme = theme_minimal(), main = "Agrupación por capacidades productivas año 2008",
             xlab = "fbcf_ataf", ylab = "vacb_htpot")

plot(as.phylo(res.hc08), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc08), type = "unrooted", cex = 0.7,
     no.margin = T)


fviz_dend(res.hc, k=5, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc08, cex= 0.8, k=5,
          k_colors =  "uchicago", 
          color_labels_by_k = TRUE,
          type="circular", title = "Agrupación estatal por capacidades productivas año 2008",
          xlab = "Fuente: Elaboración propia con datos del Censo Económico año 2009: INEGI", 
          ylab = "Pesos en la matriz de distancias",
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

plot(as.phylo(res.hc08), type = "radial")
