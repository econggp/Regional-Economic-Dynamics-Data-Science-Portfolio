data <- basecensosglobal



library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)

tgen<- rpivotTable(data, rows="Entidad", col="AE", aggregatorName="Average", 
                   vals="ATAF")

tgen

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



#Distribución

uep <- dcast(data, AE~TC, sum, value.var = "UE", margins=TRUE) 
uep %>% kable(., caption="Unidades Económicas por Sector 2003-2018")

atafp <- dcast(data, AE~TC, sum, value.var = "atafr", margins=TRUE)
atafp %>% kable(., caption="Acervo Total de Activos Fijos por Sector 2003-2018")

potp <- dcast(data, AE~TC, sum, value.var = "POT", margins=TRUE)
potp %>% kable(., caption="Población Ocupada Total por Sector 2003-2018")

vacbp <- dcast(data, AE~TC, sum, value.var = "vacbr", margins=TRUE)
vacbp %>% kable(., caption="Valor Agregado Censal Bruto por Sector 2003-2018")

uepe <- dcast(data, Entidad~TC, sum, value.var = "UE", margins=TRUE)
uepe %>% kable(., caption="Unidades Económicas por Entidad 2003-2018")

atafpe <- dcast(data, Entidad~TC, sum, value.var = "atafr", margins=TRUE)
atafpe %>% kable(., caption="Acervo Total de Activos Fijos por Entidad 2003-2018")

potpe <- dcast(data, Entidad~TC, sum, value.var = "POT", margins=TRUE)
potpe %>% kable(., caption="Población Ocupada Total por Entidad 2003-2018")

vacbpe <- dcast(data, Entidad~TC, sum, value.var = "vacbr", margins=TRUE)
vacbpe %>% kable(., caption="Valor Agregado Censal Bruto por Entidad 2003-2018")



(dvacb <- ggplot(data, aes(x=vacbr)) + 
  geom_histogram(color="white", fill="blue") +
  facet_wrap(~TC)+
  geom_freqpoly()+
  labs(title = "Valor Agregado Censal Bruto") +
  scale_fill_continuous(name="VACB"))

(dpot <- ggplot(data, aes(x=POT)) + 
  geom_histogram(color="black", fill="white") +
  facet_wrap(~TC)+
  geom_freqpoly()+
  labs(title = "Población Ocupada Total") +
  scale_fill_continuous(name="POT"))

(dataf <- ggplot(data, aes(x=atafr)) + 
  geom_histogram(color="white", fill="red") +
  facet_wrap(~TC)+
  geom_freqpoly()+
  labs(title = "Acervo Total de Activos Fijos") +
  scale_fill_continuous(name="ATAF"))

grid.arrange(dvacb, dataf, nrow=1, 
             top= "Distribución del VACB y del ATAF",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mue <- ddply(data, "TC", summarise, grp.mean=mean(UE))
ggplot(data, aes(UE, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mue, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Distribución de las Unidades Económicas 2003-2018",
       x = 'Unidades',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mataf <- ddply(data, "TC", summarise, grp.mean=mean(atafr))
ggplot(data, aes(atafr, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 1. Distribución de los acervos 2003-2018",
       x = 'Acervo (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mpot <- ddply(data, "TC", summarise, grp.mean=mean(POT))
ggplot(data, aes(POT, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Distribución de la Población Ocupada Total 2003-2018",
       x = 'Número de Empleados',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mvacb <- ddply(data, "TC", summarise, grp.mean=mean(vacb_ue))
ggplot(data, aes(vacb_ue, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 2. Distribución del Valor Agregado Censal Bruto por Unidad Económica 2003-2018",
       x = 'Valor Agregado (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


(vacbs <- xyplot(vacbr ~ TC|AE, data,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(pots <- xyplot(POT ~ TC|AE, data, 
                layout=c(4, 5),
                type=c('p', 'r'),
                auto.key=list(space='right')))
(atafs <- xyplot(ATAF ~ TC|AE, data,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(vacbe <- xyplot(VACB ~ TC|Entidad, data, 
                 layout=c(4, 8),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(pote <- xyplot(POT ~ TC|Entidad, data, 
                layout=c(4, 8),
                type=c('p', 'r'),
                auto.key=list(space='right')))
(atafe <- xyplot(ATAF ~ TC|Entidad, data, 
                 layout=c(4, 8),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))


grid.arrange(atafs, pots, vacbs, nrow=1, 
             top= "ATAF, POT y VACB por Sector 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(atafe, pote, vacbe, nrow=1, 
             top= "ATAF, POT y VACB por Entidad 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


#Especialización

(vacbesps <- xyplot(qijvacb>1 ~ TC|AE, data, 
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))
(potesps <- xyplot(qijpot>1 ~ TC|AE, data, 
                      layout=c(4, 5),
                      type=c('p', 'r'),
                      auto.key=list(space='right')))
(atafesps <- xyplot(qijataf>1 ~ TC|AE, data, 
                       layout=c(4, 5),
                       type=c('p', 'r'),
                       auto.key=list(space='right')))
(vacbespe <-xyplot(qijvacb>1 ~ TC|Entidad, data, 
                      layout=c(4, 8),
                      type=c('p', 'r'),
                      auto.key=list(space='right')))
(potespe <- xyplot(qijpot>1 ~ TC|Entidad, data, 
                      layout=c(4, 8),
                      type=c('p', 'r'),
                      auto.key=list(space='right')))
(atafespe <- xyplot(qijataf>1 ~ TC|Entidad, data, 
                       layout=c(4, 8),
                       type=c('p', 'r'),
                       auto.key=list(space='right')))


grid.arrange(atafesps, potesps, vacbesps, nrow=1, 
             top= "Especialización relativa por sector del ATAF, POT y VACB 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(atafespe, potespe, vacbespe, nrow=1, 
             top= "Especialización relativa por Entidad del ATAF, POT y VACB 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")



pijatafp <- dcast(data, AE~TC, sum, value.var = "pijataf", margins=TRUE)
pijatafp %>% kable(., caption="Especialización absoluta del Acervo Total de Activos Fijos por Sector 2003-2018")

pijpotp <- dcast(data, AE~TC, sum, value.var = "pijpot", margins=TRUE)
pijpotp %>% kable(., caption="Especialización absoluta de la Población Ocupada Total por Sector 2003-2018")

pijvacbp <- dcast(data, AE~TC, sum, value.var = "pijvacb", margins=TRUE)
pijvacbp %>% kable(., caption="Especialización absoluta del Valor Agregado Censal Bruto por Sector 2003-2018")

qijatafp <- dcast(data, AE~TC, sum, value.var = "qijataf", margins=TRUE)
qijatafp %>% kable(., caption="Especialización relativa del Acervo Total de Activos Fijos por Sector 2003-2018")

qijpotp <- dcast(data, AE~TC, sum, value.var = "qijpot", margins=TRUE)
qijpotp %>% kable(., caption="Especialización relativa de la Población Ocupada Total por Sector 2003-2018")

qijvacbp <- dcast(data, AE~TC, sum, value.var = "qijvacb", margins=TRUE)
qijvacbp %>% kable(., caption="Especialización relativa del Valor Agregado Censal Bruto por Sector 2003-2018")



(vacbpots <- xyplot(vacb_htpot>mean(vacb_htpot) ~ TC|AE, data,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(vacbpote <- xyplot(vacb_htpot>mean(vacb_htpot) ~ TC|Entidad, data, 
                layout=c(4, 8),
                type=c('p', 'r'),
                auto.key=list(space='right')))
(pbthtpots <- xyplot(pbt_htpot>mean(pbt_htpot) ~ TC|AE, data, 
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))
(pbthtpote <- xyplot(pbt_htpot>mean(pbt_htpot) ~ TC|Entidad, data, 
                    layout=c(4, 8),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))

grid.arrange(vacbpots, pbthtpots, nrow=1, 
             top= "Comportamiento de la Productividad por Sector 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(vacbpote, pbthtpote, nrow=1, 
             top= "Comportamiento de la Productividad por Entidad 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


(trhtpots <- xyplot(tr_htpot>mean(tr_htpot) ~ TC|AE, data, 
                     layout=c(4, 5),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))
(trhtpote <- xyplot(tr_htpot>mean(tr_htpot) ~ TC|Entidad, data, 
                     layout=c(4, 8),
                     type=c('p', 'r'),
                     auto.key=list(space='right')))

(fbcfhtpots <- xyplot(fbcf_htpot>mean(fbcf_htpot) ~ TC|AE, data, 
                  layout=c(4, 5),
                  type=c('p', 'r'),
                  auto.key=list(space='right')))
(fbcfhtpote <- xyplot(fbcf_htpot>mean(tr_htpot) ~ TC|Entidad, data, 
                  layout=c(4, 8),
                  type=c('p', 'r'),
                  auto.key=list(space='right')))

grid.arrange(trhtpots, fbcfhtpots, nrow=1, 
             top= "Comportamiento de los Costos por Sector 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

grid.arrange(trhtpote, fbcfhtpote, nrow=1, 
             top= "Comportamiento de los Costos por Entidad 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")



c2003 <- filter(data, TC == 2003)  
c2008 <- filter(data, TC == 2008)  
c2013 <- filter(data, TC == 2013)  
c2018 <- filter(data, TC == 2018)  

c2003$TC <- NULL
c2008$TC <- NULL
c2013$TC <- NULL
c2018$TC <- NULL

psych::describe(c2003$vacb_ataf)
psych::describe(c2003$POT)

psych::describe(c2008$vacb_ataf)
psych::describe(c2008$POT)

psych::describe(c2013$vacb_ataf)
psych::describe(c2013$POT)

psych::describe(c2018$VACB)
psych::describe(c2018$POT)




c2003$segmentopot <- as.factor(cut(c2003$POT, breaks=c(0, 9000, 28465, 129176, 532016), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

c2003$segmentovaaf <- as.factor(cut(c2003$vacb_ataf, breaks=c(-2.5, 0.78, 1.25, 5.78, 23.5), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  


c2008$segmentopot <- as.factor(cut(c2008$POT, breaks=c(0, 12305, 35239, 177591, 604575), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

c2008$segmentovaaf <- as.factor(cut(c2008$vacb_ataf, breaks=c(-1.6, 0.7, 1.02, 4.44, 23.2), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  


c2013$segmentopot <- as.factor(cut(c2013$POT, breaks=c(0, 11935, 35454, 184852, 673515), 
                                   include.lowest=TRUE, labels = c("Marginal","Bajo", "Emergente", "Consolidado")))

c2013$segmentovaaf <- as.factor(cut(c2013$vacb_ataf, breaks=c(-54.4, 0.76, 1.39, 12.8, 75.2), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  


c2018$segmentopot <- as.factor(cut(c2018$POT, breaks=c(0, 15340, 46435, 231727, 794561), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

c2018$segmentovaaf <- as.factor(cut(c2018$vacb_ataf, breaks=c(-0.19, 0.9, 5.68, 211.68, 2461), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  
c2018$segmentovacb <- as.factor(cut(c2018$VACB, breaks=c(-5006, 2674, 17075, 123977, 686977), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 


tvaaf2003 <- dcast(c2003, AE~segmentovaaf, sum, value.var = "vacb_ataf", margins=F)
tvaaf2003 %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2003")

tpot2003 <- dcast(c2003, AE~segmentopot, sum, value.var = "POT", margins=F)
tpot2003 %>% kable(., caption="Tabla de proporciones de la POT año 2003")



tvaaf2008 <- dcast(c2008, AE~segmentovaaf, sum, value.var = "vacb_ataf", margins=F)
tvaaf2008 %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2008")

tpot2008 <- dcast(c2008, AE~segmentopot, sum, value.var = "POT", margins=F)
tpot2008 %>% kable(., caption="Tabla de proporciones de la POT año 2008")



tvaaf2013 <- dcast(c2013, AE~segmentovaaf, sum, value.var = "vacb_ataf", margins=F)
tvaaf2013 %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2013")

tpot2013 <- dcast(c2013, AE~segmentopot, sum, value.var = "POT", margins=F)
tpot2013 %>% kable(., caption="Tabla de proporciones de la POT año 2013")



tvaaf2018 <- dcast(c2018, AE~segmentovaaf, sum, value.var = "vacb_ataf", margins=F)
tvaaf2018 %>% kable(., caption="Segmentación VACB/ATAF por Sector año 2018")

tpot2018 <- dcast(c2018, AE~segmentopot, sum, value.var = "POT", margins=F)
tpot2018 %>% kable(., caption="Segmentación de la POT por Sector año 2018")





mvaaf2003 <- mosaicplot(~segmentovaaf + AE, 
           data = c2003, 
           color = T, 
           las = 1,
           main="2003")
mpot2003 <- mosaicplot(~segmentopot + AE, 
                        data = c2003, 
                        color = T, 
                        las = 1,
                        main="2003")


mvaaf2008 <- mosaicplot(~segmentovaaf + AE, 
                        data = c2008, 
                        color = T, 
                        las = 1,
                        main="2008")
mpot2008 <- mosaicplot(~segmentopot + AE, 
                       data = c2008, 
                       color = T, 
                       las = 1,
                       main="2008")


mvaaf2013 <- mosaicplot(~segmentovaaf + AE, 
                        data = c2013, 
                        color = T, 
                        las = 1,
                        main="2013")
mpot2013 <- mosaicplot(~segmentopot + AE, 
                       data = c2013, 
                       color = T, 
                       las = 1,
                       main="2013")



mvaaf2018 <- mosaicplot(~segmentovaaf + AE, 
                        data = c2018, 
                        color = T, 
                        las = 1.5,
                        main="2018")
mpot2018 <- mosaicplot(~segmentopot + AE, 
                       data = c2018, 
                       color = T, 
                       las = 1,
                       main="2018")




tevaaf2003 <- table(c2003$Entidad,c2003$segmentovaaf)
round(addmargins(prop.table(tevaaf2003), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2003")

tepot2003 <- table(c2003$Entidad,c2003$segmentopot)
round(addmargins(prop.table(tepot2003), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones de la POT año 2003")



tevaaf2008 <- table(c2008$Entidad,c2008$segmentovaaf)
round(addmargins(prop.table(tevaaf2008), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2008")

tepot2008 <- table(c2008$Entidad,c2008$segmentopot)
round(addmargins(prop.table(tepot2008), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones de la POT año 2008")



tevaaf2013 <- table(c2013$Entidad,c2013$segmentovaaf)
round(addmargins(prop.table(tevaaf2013), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones VACB/ATAF año 2013")

tepot2013 <- table(c2013$Entidad,c2013$segmentopot)
round(addmargins(prop.table(tepot2013), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones de la POT año 2013")



tevaaf2018 <- table(c2018$Entidad,c2018$segmentovaaf)
round(addmargins(prop.table(tevaaf2018), c(1, 2)), 3) %>% kable(., caption="Segmentación VACB/ATAF por Entidad año 2018")

tepot2018 <- table(c2018$Entidad,c2018$segmentopot)
round(addmargins(prop.table(tepot2018), c(1, 2)), 3) %>% kable(., caption="Segmentación de la POT por Entidad año 2018")





mevaaf2003 <- mosaicplot(~segmentovaaf + Entidad, 
                         data = c2003, 
                         color = T, 
                         las = 1,
                         main="VACB/ATAF año 2003")
mepot2003 <- mosaicplot(~segmentopot + Entidad, 
                        data = c2003, 
                        color = T, 
                        las = 1,
                        main="POT año 2003")


mevaaf2008 <- mosaicplot(~segmentovaaf + Entidad, 
                         data = c2008, 
                         color = T, 
                         las = 1,
                         main="VACB/ATAF año 2008")
mepot2008 <- mosaicplot(~segmentopot + Entidad, 
                        data = c2008, 
                        color = T, 
                        las = 1,
                        main="POT año 2008")

mevaaf2013 <- mosaicplot(~segmentovaaf + Entidad, 
                         data = c2013, 
                         color = T, 
                         las = 1,
                         main="VACB/ATAF año 2013")
mepot2013 <- mosaicplot(~segmentopot + Entidad, 
                        data = c2013, 
                        color = T, 
                        las = 1,
                        main="POT año 2013")



mevaaf2018 <- mosaicplot(~segmentovaaf + Entidad, 
                         data = c2018, 
                         color = T, 
                         las = 1.5,
                         main="VACB/ATAF año 2018")
mepot2018 <- mosaicplot(~segmentopot + Entidad, 
                        data = c2018, 
                        color = T, 
                        las = 1,
                        main="POT año 2018")




segvaaf2003 <- treemap(c2003, index = c("Entidad","AE"), 
        vSize = "qijvacb", vColor= "segmentovaaf", type = "categorical",
        palette = "Set1",
        title="Segmentación por el Qij del VACB/ATAF año 2003")
segpot2003 <- treemap(c2003, index = c("Entidad","AE"), 
                       vSize = "qijpot", vColor= "segmentopot", type = "categorical",
                       palette = "Set1",
                       title="Segmentación por el Qij de la POT año 2003") 


segvaaf2008 <- treemap(c2008, index = c("Entidad","AE"), 
                       vSize = "qijvacb", vColor= "segmentovaaf", type = "categorical",
                       palette = "Set1",
                       title="Segmentación por el Qij del VACB/ATAF año 2008")
segpot2008 <- treemap(c2008, index = c("Entidad","AE"), 
                      vSize = "qijpot", vColor= "segmentopot", type = "categorical",
                      palette = "Set1",
                      title="Segmentación por el Qij de la POT año 2008") 


segvaaf2013 <- treemap(c2013, index = c("Entidad","AE"), 
                       vSize = "qijvacb", vColor= "segmentovaaf", type = "categorical",
                       palette = "Set1",
                       title="Segmentación por el Qij del VACB/ATAF año 2013")
segpot2013 <- treemap(c2013, index = c("Entidad","AE"), 
                      vSize = "qijpot", vColor= "segmentopot", type = "categorical",
                      palette = "Set1",
                      title="Segmentación por el Qij de la POT año 2013") 


segvaaf2018 <- treemap(c2018, index = c("Entidad","AE"), 
                       vSize = "qijvacb", vColor= "segmentovaaf", type = "categorical",
                       palette = "Set1",
                       title="Segmentación por el Qij del VACB/ATAF año 2018")
segpot2018 <- treemap(c2018, index = c("Entidad","AE"), 
                      vSize = "qijpot", vColor= "segmentopot", type = "categorical",
                      palette = "Set1",
                      title="Segmentación por el Qij de la POT año 2018") 



library(highcharter)

hchart(
  data_to_hierarchical(
    data =  c2018, 
    group_vars = c(segmentovaaf, ID), 
    size_var = vacb_ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB/ATAF por segmento año 2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2019, INEGI")

hchart(
  data_to_hierarchical(
    data =  c2018, 
    group_vars = c(segmentopot, ID), 
    size_var = POT),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución de la POT por segmento año 2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2019, INEGI")



tgen2003<- rpivotTable(c2003, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")
tgen2008<- rpivotTable(c2008, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")
tgen2013<- rpivotTable(c2013, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")
tgen2018<- rpivotTable(c2018, rows="Entidad", col="AE", aggregatorName="Average", vals="ATAF")


tgen2003
tgen2008
tgen2013
tgen2018



library(matrixcalc)

# Traza

matrix.trace(cov(select(c2003,var_ue:vacb_tr_pot)))
matrix.trace(cov(select(c2008, var_ue:vacb_tr_pot)))
matrix.trace(cov(select(c2013, var_ue:vacb_tr_pot)))
matrix.trace(cov(select(c2018, var_ue:vacb_tr_pot)))

# Coeficientes de Correlación

cor(select(c2003, var_ue:vacb_tr_pot))
cor(select(c2008, var_ue:vacb_tr_pot))
cor(select(c2013, var_ue:vacb_tr_pot))
cor(select(c2018, var_ue:vacb_tr_pot))

# Prueba Estadística

corr.test(select(c2003, var_ue:vacb_tr_pot))
corr.test(select(c2008, var_ue:vacb_tr_pot))
corr.test(select(c2013, var_ue:vacb_tr_pot))
corr.test(select(c2018, var_ue:vacb_tr_pot))

# Gráficos de Correlación

pairs.panels(select(c2003, POT, TR, PBT, VACB, HTPOT, 
                    CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT, 
                    ATAF, AFPUP, ATMEP, ATECP), stars=T, 
             main="Matriz de Dispersión, Histograma y Correlación Censo 2004")

cor.plot(select(c2003, POT, TR, PBT, VACB, HTPOT, 
                CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT, 
                ATAF, AFPUP, ATMEP, ATECP), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2004")

(cc2003 <- select(c2003, POT, TR, PBT, VACB, HTPOT, 
                  CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT, 
                  ATAF, AFPUP, ATMEP, ATECP) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2004"))



pairs.panels(select(c2008, POT, TR, PBT, VACB, HTPOT, 
                    CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                    ATAF, AFPUP, ATMEP, ATECP), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2009")

cor.plot(select(c2008, POT, TR, PBT, VACB, HTPOT, 
                CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT, 
                ATAF, AFPUP, ATMEP, ATECP), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2009")

(cc2008 <- select(c2008, POT, TR, PBT, VACB, HTPOT, 
                  CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                  ATAF, AFPUP, ATMEP, ATECP) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2009"))



pairs.panels(select(c2013, POT, TR, PBT, VACB, HTPOT, 
                    CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                    ATAF, AFPUP, ATMEP, ATECP), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2014")

cor.plot(select(c2013, POT, TR, PBT, VACB, HTPOT, 
                CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                ATAF, AFPUP, ATMEP, ATECP), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2014")

(cc2013 <- select(c2013, POT, TR, PBT, VACB, HTPOT, 
                  CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                  ATAF, AFPUP, ATMEP, ATECP) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2014"))



pairs.panels(select(c2018, POT, TR, PBT, VACB, HTPOT, 
                    CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                    ATAF, AFPUP, ATMEP, ATECP), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2019")

cor.plot(select(c2018, POT, TR, PBT, VACB, HTPOT, 
                CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                ATAF, AFPUP, ATMEP, ATECP), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2019")

(cc2018 <- select(c2018, POT, TR, PBT, VACB, HTPOT, 
                  CCLE, GSC, CA, CTAF, IT, FBCF, IPSPCT, CSPCT,
                  ATAF, AFPUP, ATMEP, ATECP) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2019"))


grid.arrange(cc2003, cc2008, cc2013, cc2018, nrow=2, 
             top= "Relaciones de correlación censales",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos  años 2004, 2009, 2014 y 2019, INEGI")





# Análisis de la segmentación

library(lattice)
library(PerformanceAnalytics)
library(GGally)
library(ggpubr)

ca2003 <- select(c2003, segmentovaaf, segmentopot, ID, Entidad, AE, var_ue:vacb_tr_pot,-vacb_ataf,-ataf_vacb) %>% column_to_rownames(., var = 'ID')
ca2008 <- select(c2008, segmentovaaf, segmentopot, ID, Entidad, AE, var_ue:vacb_tr_pot,-vacb_ataf,-ataf_vacb) %>% column_to_rownames(., var = 'ID')
ca2013 <- select(c2013, segmentovaaf, segmentopot, ID, Entidad, AE, var_ue:vacb_tr_pot,-vacb_ataf,-ataf_vacb) %>% column_to_rownames(., var = 'ID')
ca2018 <- select(c2018, segmentovacb, segmentopot, ID, Entidad, AE, var_ue:vacb_tr_pot) %>% column_to_rownames(., var = 'ID')

# Análisis regional

bajio2003 <- filter(ca2003, Entidad %in% c("AGU", "GUA", "JAL", "QUE", "SLP"))
bajio2008 <- filter(ca2008, Entidad %in% c("AGU", "GUA", "JAL", "QUE", "SLP"))
bajio2013 <- filter(ca2013, Entidad %in% c("AGU", "GUA", "JAL", "QUE", "SLP"))
bajio2018 <- filter(ca2018, Entidad %in% c("AGU", "GUA", "JAL", "QUE", "SLP"))

tbajio2018<- rpivotTable(bajio2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tbajio2018



centro2003 <- filter(ca2003, Entidad %in% c("CMX", "HID", "EMX", "MOR", "PUE", "TLA"))
centro2008 <- filter(ca2008, Entidad %in% c("CMX", "HID", "EMX", "MOR", "PUE", "TLA"))
centro2013 <- filter(ca2013, Entidad %in% c("CMX", "HID", "EMX", "MOR", "PUE", "TLA"))
centro2018 <- filter(ca2018, Entidad %in% c("CMX", "HID", "EMX", "MOR", "PUE", "TLA"))

tcentro2018<- rpivotTable(centro2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tcentro2018



golfo2003 <- filter(ca2003, Entidad %in% c("VER", "TAB", "CAM", "YUC", "QRO"))
golfo2008 <- filter(ca2008, Entidad %in% c("VER", "TAB", "CAM", "YUC", "QRO"))
golfo2013 <- filter(ca2013, Entidad %in% c("VER", "TAB", "CAM", "YUC", "QRO"))
golfo2018 <- filter(ca2018, Entidad %in% c("VER", "TAB", "CAM", "YUC", "QRO"))

tgolfo2018<- rpivotTable(golfo2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tgolfo2018



norcentral2003 <- filter(ca2003, Entidad %in% c("DUR", "NAY", "SIN", "ZAC"))
norcentral2008 <- filter(ca2008, Entidad %in% c("DUR", "NAY", "SIN", "ZAC"))
norcentral2013 <- filter(ca2013, Entidad %in% c("DUR", "NAY", "SIN", "ZAC"))
norcentral2018 <- filter(ca2018, Entidad %in% c("DUR", "NAY", "SIN", "ZAC"))

tnorcentral2018<- rpivotTable(norcentral2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tnorcentral2018



noreste2003 <- filter(ca2003, Entidad %in% c("COA", "NLE", "TAM"))
noreste2008 <- filter(ca2008, Entidad %in% c("COA", "NLE", "TAM"))
noreste2013 <- filter(ca2013, Entidad %in% c("COA", "NLE", "TAM"))
noreste2018 <- filter(ca2018, Entidad %in% c("COA", "NLE", "TAM"))

tnoreste2018<- rpivotTable(noreste2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tnoreste2018



noroeste2003 <- filter(ca2003, Entidad %in% c("BC", "BCS", "CHH", "SON"))
noroeste2008 <- filter(ca2008, Entidad %in% c("BC", "BCS", "CHH", "SON"))
noroeste2013 <- filter(ca2013, Entidad %in% c("BC", "BCS", "CHH", "SON"))
noroeste2018 <- filter(ca2018, Entidad %in% c("BC", "BCS", "CHH", "SON"))

tnoroeste2018<- rpivotTable(noroeste2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tnoroeste2018



pacifico2003 <- filter(ca2003, Entidad %in% c("COL", "MIC", "GUE", "OAX", "CHI"))
pacifico2008 <- filter(ca2008, Entidad %in% c("COL", "MIC", "GUE", "OAX", "CHI"))
pacifico2013 <- filter(ca2013, Entidad %in% c("COL", "MIC", "GUE", "OAX", "CHI"))
pacifico2018 <- filter(ca2018, Entidad %in% c("COL", "MIC", "GUE", "OAX", "CHI"))

tpacifico2018<- rpivotTable(pacifico2018, rows="Entidad", col="AE", aggregatorName="Average", vals="pijvacb")
tpacifico2018



set.seed(987654321)

trainindex = createDataPartition(bajio2003$tr_fbcf_pot, p=0.75)$Resample1
bajio_train2003= bajio2003[trainindex, ]
bajio_test2003= bajio2003[-trainindex, ]

trainindex=createDataPartition(bajio2008$tr_fbcf_pot, p=0.75)$Resample1
bajio_train2008= bajio2008[trainindex, ]
bajio_test2008= bajio2008[-trainindex, ]

trainindex=createDataPartition(bajio2013$tr_fbcf_pot, p=0.75)$Resample1
bajio_train2013= bajio2013[trainindex, ]
bajio_test2013= bajio2013[-trainindex, ]

trainindex=createDataPartition(bajio2018$tr_fbcf_pot, p=0.75)$Resample1
bajio_train2018= bajio2018[trainindex, ]
bajio_test2018= bajio2018[-trainindex, ]

trainindex = createDataPartition(centro2003$tr_fbcf_pot, p=0.75)$Resample1
centro_train2003= centro2003[trainindex, ]
centro_test2003= centro2003[-trainindex, ]

trainindex=createDataPartition(centro2008$tr_fbcf_pot, p=0.75)$Resample1
centro_train2008= centro2008[trainindex, ]
centro_test2008= centro2008[-trainindex, ]

trainindex=createDataPartition(centro2013$tr_fbcf_pot, p=0.75)$Resample1
centro_train2013= centro2013[trainindex, ]
centro_test2013= centro2013[-trainindex, ]

trainindex=createDataPartition(centro2018$tr_fbcf_pot, p=0.75)$Resample1
centro_train2018= centro2018[trainindex, ]
centro_test2018= centro2018[-trainindex, ]

trainindex = createDataPartition(golfo2003$tr_fbcf_pot, p=0.75)$Resample1
golfo_train2003= golfo2003[trainindex, ]
golfo_test2003= golfo2003[-trainindex, ]

trainindex=createDataPartition(golfo2008$tr_fbcf_pot, p=0.75)$Resample1
golfo_train2008= golfo2008[trainindex, ]
golfo_test2008= golfo2008[-trainindex, ]

trainindex=createDataPartition(golfo2013$tr_fbcf_pot, p=0.75)$Resample1
golfo_train2013= golfo2013[trainindex, ]
golfo_test2013= golfo2013[-trainindex, ]

trainindex=createDataPartition(golfo2018$tr_fbcf_pot, p=0.75)$Resample1
golfo_train2018= golfo2018[trainindex, ]
golfo_test2018= golfo2018[-trainindex, ]

trainindex = createDataPartition(norcentral2003$tr_fbcf_pot, p=0.75)$Resample1
norcentral_train2003= norcentral2003[trainindex, ]
norcentral_test2003= norcentral2003[-trainindex, ]

trainindex=createDataPartition(norcentral2008$tr_fbcf_pot, p=0.75)$Resample1
norcentral_train2008= norcentral2008[trainindex, ]
norcentral_test2008= norcentral2008[-trainindex, ]

trainindex=createDataPartition(norcentral2013$tr_fbcf_pot, p=0.75)$Resample1
norcentral_train2013= norcentral2013[trainindex, ]
norcentral_test2013= norcentral2013[-trainindex, ]

trainindex=createDataPartition(norcentral2018$fbcf_pot, p=0.75)$Resample1
norcentral_train2018= norcentral2018[trainindex, ]
norcentral_test2018= norcentral2018[-trainindex, ]

trainindex = createDataPartition(noreste2003$tr_fbcf_pot, p=0.75)$Resample1
noreste_train2003= noreste2003[trainindex, ]
noreste_test2003= noreste2003[-trainindex, ]

trainindex=createDataPartition(noreste2008$tr_fbcf_pot, p=0.75)$Resample1
noreste_train2008= noreste2008[trainindex, ]
noreste_test2008= noreste2008[-trainindex, ]

trainindex=createDataPartition(noreste2013$tr_fbcf_pot, p=0.75)$Resample1
noreste_train2013= noreste2013[trainindex, ]
noreste_test2013= noreste2013[-trainindex, ]

trainindex=createDataPartition(noreste2018$tr_fbcf_pot, p=0.75)$Resample1
noreste_train2018= noreste2018[trainindex, ]
noreste_test2018= noreste2018[-trainindex, ]

trainindex = createDataPartition(noroeste2003$tr_fbcf_pot, p=0.75)$Resample1
noroeste_train2003= noroeste2003[trainindex, ]
noroeste_test2003= noroeste2003[-trainindex, ]

trainindex=createDataPartition(noroeste2008$tr_fbcf_pot, p=0.75)$Resample1
noroeste_train2008= noroeste2008[trainindex, ]
noroeste_test2008= noroeste2008[-trainindex, ]

trainindex=createDataPartition(noroeste2013$tr_fbcf_pot, p=0.75)$Resample1
noroeste_train2013= noroeste2013[trainindex, ]
noroeste_test2013= noroeste2013[-trainindex, ]

trainindex=createDataPartition(noroeste2018$tr_fbcf_pot, p=0.75)$Resample1
noroeste_train2018= noroeste2018[trainindex, ]
noroeste_test2018= noroeste2018[-trainindex, ]

trainindex = createDataPartition(pacifico2003$tr_fbcf_pot, p=0.75)$Resample1
pacifico_train2003= pacifico2003[trainindex, ]
pacifico_test2003= pacifico2003[-trainindex, ]

trainindex=createDataPartition(pacifico2008$tr_fbcf_pot, p=0.75)$Resample1
pacifico_train2008= pacifico2008[trainindex, ]
pacifico_test2008= pacifico2008[-trainindex, ]

trainindex=createDataPartition(pacifico2013$tr_fbcf_pot, p=0.75)$Resample1
pacifico_train2013= pacifico2013[trainindex, ]
pacifico_test2013= pacifico2013[-trainindex, ]

trainindex=createDataPartition(pacifico2018$tr_fbcf_pot, p=0.75)$Resample1
pacifico_train2018= pacifico2018[trainindex, ]
pacifico_test2018= pacifico2018[-trainindex, ]



# Análisis descriptivo

psych::describe(bajio2003)
psych::describe(bajio2008)
psych::describe(bajio2013)
psych::describe(bajio2018)

psych::describe(centro2003)
psych::describe(centro2008)
psych::describe(centro2013)
psych::describe(centro2018)

psych::describe(noreste2003)
psych::describe(noreste2008)
psych::describe(noreste2013)
psych::describe(noreste2018)

psych::describe(noroeste2003)
psych::describe(noroeste2008)
psych::describe(noroeste2013)
psych::describe(noroeste2018)

psych::describe(pacifico2003)
psych::describe(pacifico2008)
psych::describe(pacifico2013)
psych::describe(pacifico2018)

psych::describe(golfo2003)
psych::describe(golfo2008)
psych::describe(golfo2013)
psych::describe(golfo2018)

# Análisis de correlación

# Variancia-Covariancia

(covb2003  <- cov(select(bajio2003, var_ue:vacb_tr_pot)))
(covb2008  <- cov(select(bajio2008, var_ue:vacb_tr_pot)))
(covb2013  <- cov(select(bajio2013, var_ue:vacb_tr_pot)))
(covb2018  <- cov(select(bajio2018, var_ue:vacb_tr_pot)))

(covc2003  <- cov(select(centro2003, var_ue:vacb_tr_pot)))
(covc2008  <- cov(select(centro2008, var_ue:vacb_tr_pot)))
(covc2013  <- cov(select(centro2013, var_ue:vacb_tr_pot)))
(covc2018  <- cov(select(centro2018, var_ue:vacb_tr_pot)))

(covne2003  <- cov(select(norcentral2003, var_ue:vacb_tr_pot)))
(covne2008  <- cov(select(norcentral2008, var_ue:vacb_tr_pot)))
(covne2013  <- cov(select(norcentral2013, var_ue:vacb_tr_pot)))
(covne2018  <- cov(select(norcentral2018, var_ue:vacb_tr_pot)))

(covne2003  <- cov(select(noreste2003, var_ue:vacb_tr_pot)))
(covne2008  <- cov(select(noreste2008, var_ue:vacb_tr_pot)))
(covne2013  <- cov(select(noreste2013, var_ue:vacb_tr_pot)))
(covne2018  <- cov(select(noreste2018, var_ue:vacb_tr_pot)))

(covno2003  <- cov(select(noroeste2003, var_ue:vacb_tr_pot)))
(covno2008  <- cov(select(noroeste2008, var_ue:vacb_tr_pot)))
(covno2013  <- cov(select(noroeste2013, var_ue:vacb_tr_pot)))
(covno2018  <- cov(select(noroeste2018, var_ue:vacb_tr_pot)))

(covp2003  <- cov(select(pacifico2003, var_ue:vacb_tr_pot)))
(covp2008  <- cov(select(pacifico2008, var_ue:vacb_tr_pot)))
(covp2013  <- cov(select(pacifico2013, var_ue:vacb_tr_pot)))
(covp2018  <- cov(select(pacifico2018, var_ue:vacb_tr_pot)))

(covg2003  <- cov(select(golfo2003, var_ue:vacb_tr_pot)))
(covg2008  <- cov(select(golfo2008, var_ue:vacb_tr_pot)))
(covg2013  <- cov(select(golfo2013, var_ue:vacb_tr_pot)))
(covg2018  <- cov(select(golfo2018, var_ue:vacb_tr_pot)))


# Árboles de decisión

library(rpart)
library(rpart.plot)
library(rattle)
library(tree)


set.seed(1234567890)

arbol_clasificacion <- tree(formula = segmentovacb ~ .
                            , data = bajio_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenat2018 <- rpart(segmentovacb ~ .
                       , 
                      data = bajio_train2018, method = "class", control = control.poda)
claatbb2018 <- fancyRpartPlot(madgenat2018, main = "Clasificación del VACB: Bajío",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predat2018 <- predict(madgenat2018, bajio_test2018, type = "class")
confusionMatrix(predat2018, bajio_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovacb ~ ., data = centro_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatc2018 <- rpart(segmentovacb ~ ., 
                       data = centro_train2018, method = "class", control = control.poda)
claatc2018 <- fancyRpartPlot(madgenatc2018, main = "Clasificación del VACB: Centro",
                               sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatc2018 <- predict(madgenatc2018, centro_test2018, type = "class")
confusionMatrix(predatc2018, centro_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovacb ~.
                            , data = golfo_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatg2018 <- rpart(segmentovacb ~ .-pijvacb-vacb_tr_pot-AE-pijataf-pijpot
                       -segmentopot-vacb_tr-vacb_fbcf_pot-vacb_pot-qijvacb
                       -ccle_pbt-tr_vacb-var_ca-vacb_pbt-afpup_ue, 
                       data = golfo_train2018, method = "class", control = control.poda)
claatg2018 <- fancyRpartPlot(madgenatg2018, main = "Clasificación del VACB: Golfo",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatg2018 <- predict(madgenatg2018, golfo_test2018, type = "class")
confusionMatrix(predatg2018, golfo_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovacb ~ .
                            , data = norcentral_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatnc2018 <- rpart(segmentovacb ~ .
                       
                      ,data = norcentral_train2018, method = "class", control = control.poda)
claatnc2018 <- fancyRpartPlot(madgenatnc2018, main = "Clasificación del VACB: Norcentral",
                               sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatnc2018 <- predict(madgenatnc2018, norcentral_test2018, type = "class")
confusionMatrix(predatnc2018, norcentral_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovacb ~ .
                            , data = noreste_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatne2018 <- rpart(segmentovacb ~ .-pijvacb-qijvacb-vacb_tr_pot-vacb_fbcf_pot
                        , 
                       data = noreste_train2018, method = "class", control = control.poda)
claatne2018 <- fancyRpartPlot(madgenatne2018, main = "Clasificación del VACB: Noreste",
                               sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatne2018 <- predict(madgenatne2018, noreste_test2018, type = "class")
confusionMatrix(predatne2018, noreste_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovacb ~ ., 
                            data = noroeste_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatno2018 <- rpart(segmentovacb ~ ., 
                        data = noroeste_train2018, method = "class", control = control.poda)
claatno2018 <- fancyRpartPlot(madgenatno2018, main = "Clasificación del VACB: Noroeste",
                               sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatno2018 <- predict(madgenatno2018, noroeste_test2018, type = "class")
confusionMatrix(predatno2018, noroeste_test2018[["segmentovacb"]])



arbol_clasificacion <- tree(formula = segmentovaaf ~ ., data = pacifico_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenatp2018 <- rpart(segmentovacb ~  .-pijvacb, 
                        data = pacifico_train2018, method = "class", control = control.poda)
claatp2018 <- fancyRpartPlot(madgenatp2018, main = "Clasificación del VACB: Pacifico",
                               sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predatp2018 <- predict(madgenatp2018, pacifico_test2018, type = "class")
confusionMatrix(predatp2018, pacifico_test2018[["segmentovacb"]])



set.seed(16121979)


arbol_clasificacion <- tree(formula = segmentopot ~ ., data = bajio_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpob2018 <- rpart(segmentopot ~ ., 
                      data = bajio_train2018, method = "class", control = control.poda)
clapotb2018 <- fancyRpartPlot(madgenpob2018, main = "Clasificación de la POT: Bajio",
                             sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpob2018 <- predict(madgenpob2018, bajio_test2018, type = "class")
confusionMatrix(predpob2018, bajio_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .
                            , data = centro_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpoc2018 <- rpart(segmentopot ~ .
                       , 
                       data = centro_train2018, method = "class", control = control.poda)
clapotc2018 <- fancyRpartPlot(madgenpoc2018, main = "Clasificación de la POT: Centro",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpoc2018 <- predict(madgenpoc2018, centro_test2018, type = "class")
confusionMatrix(predpoc2018, centro_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .,data = golfo_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpog2018 <- rpart(segmentopot ~ .
                       , 
                       data = golfo_train2018, method = "class", control = control.poda)
clapotg2018 <- fancyRpartPlot(madgenpog2018, main = "Clasificación de la POT: Golfo",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpog2018 <- predict(madgenpog2018, golfo_test2018, type = "class")
confusionMatrix(predpog2018, golfo_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .-tr_vacb_pot-vacb_tr_pot
                            , data = norcentral_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenponc2018 <- rpart(segmentopot ~ .-tr_vacb_pot-vacb_tr_pot
                        , 
                        data = norcentral_train2018, method = "class", control = control.poda)
clapotnc2018 <- fancyRpartPlot(madgenponc2018, main = "Clasificación de la POT: Norcentral",
                                sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")  
predponc2018 <- predict(madgenponc2018, norcentral_test2018, type = "class")
confusionMatrix(predponc2018, norcentral_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .-pijpot
                            , data = noreste_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpone2018 <- rpart(segmentopot ~  .-pijpot
                        , 
                       data = noreste_train2018, method = "class", control = control.poda)
clapotne2018 <- fancyRpartPlot(madgenpone2018, main = "Clasificación de la POT: Noreste",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpone2018 <- predict(madgenpone2018, noreste_test2018, type = "class")
confusionMatrix(predpone2018, noreste_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .
                            , 
                            data = noroeste_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpono2018 <- rpart(segmentopot ~ .
                        , 
                       data = noroeste_train2018, method = "class", control = control.poda)
clapotno2018 <- fancyRpartPlot(madgenpono2018, main = "Clasificación de la POT: Noroeste",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpono2018 <- predict(madgenpono2018, noroeste_test2018, type = "class")
confusionMatrix(predpono2018, noroeste_test2018[["segmentopot"]])



arbol_clasificacion <- tree(formula = segmentopot ~ .
                            , data = pacifico_train2018,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 100)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
madgenpop2018 <- rpart(segmentopot ~ .
                       , 
                       data = pacifico_train2018, method = "class", control = control.poda)
clapotp2018 <- fancyRpartPlot(madgenpop2018, main = "Clasificación de la POT: Pacifico",
                              sub= "Fuente: Elaboración propia con datos del Censo Económico año 2019 INEGI")
predpop2018 <- predict(madgenpop2018, pacifico_test2018, type = "class")
confusionMatrix(predpop2018, pacifico_test2018[["segmentopot"]])










