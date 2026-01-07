data <- basecensosglobal

library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)
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
library(highcharter)
library(matrixcalc)
library(lattice)
library(PerformanceAnalytics)
library(GGally)
library(ggpubr)

tgenpromedio<- rpivotTable(data, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")
tgenpromedio



#Distribución



uepe <- dcast(data, Entidad, sum, value.var = "UE", margins=TRUE)
uepe %>% kable(., caption="Unidades Económicas por Entidad 2003-2018")

atafpe <- dcast(data, Entidad~TC, sum, value.var = "atafr", margins=TRUE)
atafpe %>% kable(., caption="Acervo Total de Activos Fijos por Entidad 2003-2018")

potpe <- dcast(data, Entidad~TC, sum, value.var = "POT", margins=TRUE)
potpe %>% kable(., caption="Población Ocupada Total por Entidad 2003-2018")

vacbpe <- dcast(data, Entidad~TC, sum, value.var = "vacbr", margins=TRUE)
vacbpe %>% kable(., caption="Valor Agregado Censal Bruto por Entidad 2003-2018")


psych::describe(data$POT)
psych::describe(data$vacbr)
psych::describe(data$atafr)



data$segmentopot <- as.factor(cut(data$POT, breaks=c(0, 10790, 34918, 107742, 607294), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

data$segmentovacb <- as.factor(cut(data$vacbr, breaks=c(-4849, 2566, 16597, 88452, 956694), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  

data$segmentoataf <- as.factor(cut(data$atafr, breaks=c(0, 2995, 16380, 66218, 487440), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))



datact <- select(data, segmentopot, segmentovacb, segmentoataf, ID, Entidad, AE, var_ue:vacb_tr_pot)



agu <- filter(datact, Entidad %in% c("AGU"))  %>% column_to_rownames(., var = 'ID')
bc <- filter(datact, Entidad %in% c("BC"))  %>% column_to_rownames(., var = 'ID')
bcs <- filter(datact, Entidad %in% c("BCS"))  %>% column_to_rownames(., var = 'ID')
cam <- filter(datact, Entidad %in% c("CAM"))  %>% column_to_rownames(., var = 'ID')
coa <- filter(datact, Entidad %in% c("COA"))  %>% column_to_rownames(., var = 'ID')
col <- filter(datact, Entidad %in% c("COL"))  %>% column_to_rownames(., var = 'ID')
chi <- filter(datact, Entidad %in% c("CHI"))  %>% column_to_rownames(., var = 'ID')
chh <- filter(datact, Entidad %in% c("CHH"))  %>% column_to_rownames(., var = 'ID')
cmx <- filter(datact, Entidad %in% c("CMX"))  %>% column_to_rownames(., var = 'ID')
dur <- filter(datact, Entidad %in% c("DUR"))  %>% column_to_rownames(., var = 'ID')
gua <- filter(datact, Entidad %in% c("GUA"))  %>% column_to_rownames(., var = 'ID')
gue <- filter(datact, Entidad %in% c("GUE"))  %>% column_to_rownames(., var = 'ID')
hid <- filter(datact, Entidad %in% c("HID"))  %>% column_to_rownames(., var = 'ID')
jal <- filter(datact, Entidad %in% c("JAL"))  %>% column_to_rownames(., var = 'ID')
emx <- filter(datact, Entidad %in% c("EMX"))  %>% column_to_rownames(., var = 'ID')
mic <- filter(datact, Entidad %in% c("MIC"))  %>% column_to_rownames(., var = 'ID')
mor <- filter(datact, Entidad %in% c("MOR"))  %>% column_to_rownames(., var = 'ID')
nay <- filter(datact, Entidad %in% c("NAY"))  %>% column_to_rownames(., var = 'ID')
nle <- filter(datact, Entidad %in% c("NLE"))  %>% column_to_rownames(., var = 'ID')
oax <- filter(datact, Entidad %in% c("OAX"))  %>% column_to_rownames(., var = 'ID')
pue <- filter(datact, Entidad %in% c("PUE"))  %>% column_to_rownames(., var = 'ID')
que <- filter(datact, Entidad %in% c("QUE"))  %>% column_to_rownames(., var = 'ID')
qro <- filter(datact, Entidad %in% c("QRO"))  %>% column_to_rownames(., var = 'ID')
slp <- filter(datact, Entidad %in% c("SLP"))  %>% column_to_rownames(., var = 'ID')
sin <- filter(datact, Entidad %in% c("SIN"))  %>% column_to_rownames(., var = 'ID')
son <- filter(datact, Entidad %in% c("SON"))  %>% column_to_rownames(., var = 'ID')
tab <- filter(datact, Entidad %in% c("TAB"))  %>% column_to_rownames(., var = 'ID')
tam <- filter(datact, Entidad %in% c("TAM"))  %>% column_to_rownames(., var = 'ID')
tla <- filter(datact, Entidad %in% c("TLA"))  %>% column_to_rownames(., var = 'ID')
ver <- filter(datact, Entidad %in% c("VER"))  %>% column_to_rownames(., var = 'ID')
yuc <- filter(datact, Entidad %in% c("YUC"))  %>% column_to_rownames(., var = 'ID')
zac <- filter(datact, Entidad %in% c("ZAC"))  %>% column_to_rownames(., var = 'ID')



datactg <- select(data, segmentopot, segmentovacb, segmentoataf, Entidad, AE, var_ue:vacb_tr_pot) 



set.seed(987654321)

trainindex = createDataPartition(datactg$ataf_vacb, p=0.75)$Resample1
data_train= datactg[trainindex, ]
data_test= datactg[-trainindex, ]



# Árboles de decisión

library(rpart)
library(rpart.plot)
library(rattle)
library(tree)


set.seed(1234567890)

arbol_clasificacion <- tree(formula = segmentovacb ~ .
                            , data = data_train,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 50)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenat <- rpart(segmentovacb ~ .
                      , 
                      data = data_train, method = "class", control = control.poda)
claatbb <- fancyRpartPlot(madgenat, main = "Clasificación del VACB real promedio 2003-2018", sub= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2013 y 2019 INEGI")  
predat <- predict(madgenat, data_test, type = "class")
confusionMatrix(predat, data_test[["segmentovacb"]])




