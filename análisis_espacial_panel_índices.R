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
library(GGally)
library(tidyverse)
library(rmgarch)
library(tseries)
library(lmtest)
library(foreign)
library(ggplot2)
library(gplots)
library(plm)
library(lubridate) 
library(ggcorrplot)
library(ggpubr)
library(plyr)
library(stats)
library(spdep)
library(splm)
library(sf)
library(sp)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(leaflet)
library(SDPDmod)
library(pspatreg)
library(spatialreg)
library(gridExtra)

mx <- read_sf("C:/Users/gezum/Downloads/Entidades_Federativas/Entidades_Federativas.shp")
head(mx)

rook.mx<-poly2nb(mx, row.names = mx$NOMGEO, queen = FALSE)
summary(rook.mx)

rook.mx.bi<-listw2mat(nb2listw(rook.mx, style ="B")) #Binary Matrix#
w <-listw2mat(nb2listw(rook.mx, style ="W")) #Row Standarized Matrix#


df <- base_índices
data <- data[, !colnames(data) %in% c("tcode")]
columnas_numericas <- sapply(data, is.numeric)
data_numericas <- data[, columnas_numericas]
data_estandarizada <- scale(data_numericas)
data[, columnas_numericas] <- data_estandarizada

data$tcode <- base_índices$tcode

head(data)

data_11 <- subset(df, AE == "11")
head(data_11)

data_11 <- plm::pdata.frame(data_11, index=c("NOMGEO", "tcode"))
order(data_11[,"NOMGEO"])


formlin <- ptf ~  cec + qijpot + qijataf + qijvacb +
  roa + roic + iaf + rat + mbp + mbg + 
  pbt_afup + pbt_atmep + pbt_atbi + pbt_atet + pbt_atecp + pbt_atmo + 
  pbt_cpo + pbt_cle + pbt_cee + pbt_rabmi + pbt_cspct + pbt_gsc + pbt_rrm + pbt_cag +
  pbt_htppvs + pbt_htpacd +
  fbcf_afup + fbcf_atmep + fbcf_atbi + fbcf_atet + fbcf_atecp + fbcf_atmo + 
  fbcf_ppvs + fbcf_pacd + fbcf_it
  

# Static panel model with spatial fixed effects

r1<-blmpSDPD(formula = formlin, 
               data = data_11, W = w,
               index = c("NOMGEO","tcode"),
               model = list("ols","sar","sdm","sem","sdem","slx"), 
               effect = "individual")
r1

# Static panel model with time fixed effects

res2<-blmpSDPD(formula = formlin, 
               data = data, W = W,
               index = c("NOMGEO","tcode"),
               model = list("ols","sar","sdm","sem","sdem","slx"), 
               effect = "time")
res2

# Static panel model with both spatial and time fixed effects

res3<-blmpSDPD(formula = formlin, 
               data = data, W = W,
               index = c("NOMGEO","tcode"),
               model = list("ols","sar","sdm","sem","sdem","slx"), 
               effect = "twoways", 
               prior = "beta")
res3

# Dynamic panel model with both spatial and time fixed effects with uniform prior

res4<-blmpSDPD(formula = formlin, 
               data = data, W = W,
               index = c("NOMGEO","tcode"),
               model = list("ols","sar","sdm","sem","sdem","slx"), 
               effect = "twoways",
               ldet = "mc",
               dynamic = T,
               prior = "uniform")
res4

# Dynamic spatial model with spatial fixed effects (with Lee-Yu transformation)

formlin <- ptf ~  cec + qijpot + qijataf + qijvacb +
  roa + roic + iaf + rat + mbp + mbg + 
  pbt_afup + pbt_atmep + pbt_atbi + pbt_atet + pbt_atecp + pbt_atmo + 
  pbt_cpo + pbt_cle + pbt_cee + pbt_rabmi + pbt_cspct + pbt_gsc + pbt_rrm + pbt_cag +
  pbt_htppvs + pbt_htpacd +
  fbcf_afup + fbcf_atmep + fbcf_atbi + fbcf_atet + fbcf_atecp + fbcf_atmo + 
  fbcf_ppvs + fbcf_pacd + fbcf_it

mod1<-SDPDm(formula = formlin, 
            data = data, W = W,
            index = c("NOMGEO","tcode"),
            model = "sdm", 
            effect = "twoways",
            LYtrans = T,
            dynamic = T,
            tlaginfo = list(ind = NULL, tl = T, stl = T))
summary(mod1)

imp  <- impactsSDPDm(mod1)
summary(imp)

# Using splm with spatial panel data

formlin <- QrVA ~  vapo33611 + vapo33612 + vapo33631 + vapo33632 +
  vapo33633 + vapo33634 + vapo33635 + vapo33636 + vapo33637 + vapo33639 +
  qijpo33611 + qijpo33612 + qijpo33611 + qijpo33612 + qijpo33631 + 
  qijpo33632 + qijpo33633 + qijpo33634 + qijpo33635 + qijpo33636 +
  qijpo33637 + qijpo33639 +  
  qivacb + QrPO

# Modelo de efectos aleatorios

mod2 <- spml(formula = formlin , 
             data = data, 
             listw = spdep::mat2listw(W),
             model = "random", 
             lag = TRUE, spatial.error = "none",
             index = c("NOMGEO","tcode"))

summary(mod2)

# Modelo sem

mod3 <- spml(formula = formlin , 
             data = data, 
             listw = spdep::mat2listw(W),
             model = "random", 
             lag = F, spatial.error = "kkp",
             index = c("NOMGEO","tcode"))

summary(mod3)

# Modelo efectos fijos

mod4 <- spml(formula = formlin, 
             data = data, 
             listw = spdep::mat2listw(W),
             model = "within", 
             lag = TRUE, 
             spatial.error= "b", 
             effect = "individual",
             method = "eigen",
             index = c("NOMGEO","tcode"))

summary(mod4)

# modelo con solo efectos fijos individuales

mod5 <- spml(formula = formlin, 
             data = data, 
             listw = spdep::mat2listw(W),
             model = "within", 
             effect = "individual",
             method = "eigen",
             index = c("NOMGEO","tcode"))

summary(mod5)

eff5 <- effects(mod5)
eff5

# modelo de error espacial con efectos fijos de período de tiempo

mod6 <- spml(formula = formlin, 
             data = data, 
             listw = spdep::mat2listw(W),
             model = "within", 
             effect = "time",
             method = "eigen",
             index = c("NOMGEO","tcode"))
summary(mod6)

eff6 <- effects(mod6)
eff6


# LM test

# LM1:  H0 es sin efectos aleatorios asumiendo que no hay autocorrelación espacial
test1 <- bsktest(x = formlin, data = data, 
                 listw = W,test = "LM1")
test1

# LM2:  H0 es sin autocorrelación espacial asumiendo que no efectos aleatorios.

test2 <- bsktest(x = formlin, data = data, listw = mat2listw(W), test = "LM2")
test2


test3 <- bsktest(x = formlin, data = data, listw = mat2listw(W),test = "CLMlambda")
test3 

test4 <- bsktest(x = formlin, data = data, listw = mat2listw(W),test = "CLMmu")
test4

test5 <- bsktest(x = formlin, data = data, listw = mat2listw(W),test = "LMH")
test5

# Test de Hausman espacial

teste1 <- sphtest(x = formlin, data = data, listw = mat2listw(W),spatial.model = "error", method = "GM")
teste1

teste2 <- sphtest(x = mod2, x2 = mod5)
teste2



tgenpromedio<- rpivotTable(data, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")
tgenpromedio

afupp <- dcast(data, AE~TC, sum, value.var = "afup_ataf", margins=F)
afupp %>% kable(., caption="Proporción de los Activos generados para uso propio con respecto de los Activos Fijos por Sector 2003-2018")

atmepp <- dcast(data, AE~TC, sum, value.var = "atmep_ataf", margins=F)
atmepp %>% kable(., caption="Proporción de los Activos en maquinaria y equipo con respecto de los Activos Fijos por Sector 2003-2018")

atbip <- dcast(data, AE~TC, sum, value.var = "atbi_ataf", margins=F)
atbip %>% kable(., caption="Proporción de los Activos en bienes inmuebles con respecto de los Activos Fijos por Sector 2003-2018")

atetp <- dcast(data, AE~TC, sum, value.var = "atet_ataf", margins=F)
atetp %>% kable(., caption="Proporción de los Activos en equipo de transporte con respecto de los Activos Fijos por Sector 2003-2018")

atecpp <- dcast(data, AE~TC, sum, value.var = "atecp_ataf", margins=F)
atecpp %>% kable(., caption="Especialización relativa de la Población Ocupada Total por Sector 2003-2018")

atmop <- dcast(data, AE~TC, sum, value.var = "atmo_ataf", margins=F)
atmop %>% kable(., caption="Proporción de los Activos en mobiliario de oficina con respecto de los Activos Fijos por Sector 2003-2018")

cecp <- dcast(data, AE~TC, sum, value.var = "cec", margins=F)
cecp %>% kable(., caption="Concentración de las Unidades Económicas por Sector 2003-2018")

tcpotp <- dcast(data, AE~TC, sum, value.var = "tcpot", margins=F)
tcpotp %>% kable(., caption="Crecimiento de la POT por Sector 2003-2018")


c2003 <- filter(data, TC == 2003)  
c2008 <- filter(data, TC == 2008)  
c2013 <- filter(data, TC == 2013)  
c2018 <- filter(data, TC == 2018)  

c2003$TC <- NULL
c2008$TC <- NULL
c2013$TC <- NULL
c2018$TC <- NULL


# Índice de concentración C4

c4_03 <- sum(sort(c2003$cec, decreasing = TRUE)[1:4])
c4_03

c4_08 <- sum(sort(c2008$cec, decreasing = TRUE)[1:4])
c4_08

c4_13 <- sum(sort(c2013$cec, decreasing = TRUE)[1:4])
c4_13

c4_18 <- sum(sort(c2018$cec, decreasing = TRUE)[1:4])
c4_18

# Índice de concentración C8
c8_03 <- sum(sort(c2003$cec, decreasing = TRUE)[1:8])
c8_03

c8_08 <- sum(sort(c2008$cec, decreasing = TRUE)[1:8])
c8_08

c8_13 <- sum(sort(c2013$cec, decreasing = TRUE)[1:8])
c8_13

c8_18 <- sum(sort(c2018$cec, decreasing = TRUE)[1:8])
c8_18

# Ratio de Herfindahl-Hirschman (HHI)

hhi_03 <- sum((c2003$cec^2) * 10000)
hhi_03

hhi_08 <- sum((c2008$cec^2) * 10000)
hhi_08

hhi_13 <- sum((c2013$cec^2) * 10000)
hhi_13

hhi_18 <- sum((c2018$cec^2) * 10000)
hhi_18

# Calcula la curva de Lorenz
datos_03 <- c2003[order(-c2003$cec), ]
datos_03$Lorenz <- cumsum(c2003$cec) / sum(c2003$cec)

ggplot(datos_03, aes(x = c2003$AE, y = Lorenz)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Sectores", y = "Cuota de mercado acumulada") +
  ggtitle("Gráfico de Lorenz")

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




