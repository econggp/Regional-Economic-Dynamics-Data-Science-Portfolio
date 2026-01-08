data <- base_índices

library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)

tgen<- rpivotTable(data, rows="EF", col="AE", aggregatorName="Average", 
                   vals="ataf")

tgen

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



#Distribución

uep <- dcast(data, AE~TC, sum, value.var = "UE", margins=TRUE) 
uep %>% kable(., caption="Unidades Económicas por Sector 2003-2018")

atafp <- dcast(data, AE~TC, sum, value.var = "ataf", margins=TRUE)
atafp %>% kable(., caption="Acervo Total de Activos Fijos por Sector 2003-2018")

potp <- dcast(data, AE~TC, sum, value.var = "pot", margins=TRUE)
potp %>% kable(., caption="Población Ocupada Total por Sector 2003-2018")

vacbp <- dcast(data, AE~TC, sum, value.var = "vacb", margins=TRUE)
vacbp %>% kable(., caption="Valor Agregado Censal Bruto por Sector 2003-2018")

uepe <- dcast(data, EF~TC, sum, value.var = "UE", margins=TRUE)
uepe %>% kable(., caption="Unidades Económicas por Entidad 2003-2018")

atafpe <- dcast(data, EF~TC, sum, value.var = "ataf", margins=TRUE)
atafpe %>% kable(., caption="Acervo Total de Activos Fijos por Entidad 2003-2018")

potpe <- dcast(data, EF~TC, sum, value.var = "pot", margins=TRUE)
potpe %>% kable(., caption="Población Ocupada Total por Entidad 2003-2018")

vacbpe <- dcast(data, EF~TC, sum, value.var = "vacb", margins=TRUE)
vacbpe %>% kable(., caption="Valor Agregado Censal Bruto por Entidad 2003-2018")



(dvacb <- ggplot(data, aes(x=vacb)) + 
    geom_histogram(color="white", fill="blue") +
    facet_wrap(~TC)+
    geom_freqpoly()+
    labs(title = "Valor Agregado Censal Bruto") +
    scale_fill_continuous(name="VACB"))

(dpot <- ggplot(data, aes(x=pot)) + 
    geom_histogram(color="black", fill="white") +
    facet_wrap(~TC)+
    geom_freqpoly()+
    labs(title = "Población Ocupada Total") +
    scale_fill_continuous(name="POT"))

(dataf <- ggplot(data, aes(x=ataf)) + 
    geom_histogram(color="white", fill="red") +
    facet_wrap(~TC)+
    geom_freqpoly()+
    labs(title = "Acervo Total de Activos Fijos") +
    scale_fill_continuous(name="ATAF"))

grid.arrange(dvacb, dpot,dataf, nrow=1, 
             top= "Distribución del VACB, POT y ATAF",
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

mataf <- ddply(data, "TC", summarise, grp.mean=mean(ataf))
ggplot(data, aes(ataf, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 1. Distribución de los acervos 2003-2018",
       x = 'Acervo (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mpot <- ddply(data, "TC", summarise, grp.mean=mean(pot))
ggplot(data, aes(pot, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Distribución de la Población Ocupada Total 2003-2018",
       x = 'Número de Empleados',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mvacb <- ddply(data, "TC", summarise, grp.mean=mean(vacb))
ggplot(data, aes(vacb, fill=factor(TC)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 2. Distribución del Valor Agregado Censal Bruto 2003-2018",
       x = 'Valor Agregado (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


(vacbs <- xyplot(vacb ~ TC|AE, data,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(pots <- xyplot(pot ~ TC|AE, data, 
                layout=c(4, 5),
                type=c('p', 'r'),
                auto.key=list(space='right')))
(atafs <- xyplot(ataf ~ TC|AE, data,
                 layout=c(4, 5),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(vacbe <- xyplot(vacb ~ TC|EF, data, 
                 layout=c(4, 8),
                 type=c('p', 'r'),
                 auto.key=list(space='right')))
(pote <- xyplot(pot ~ TC|EF, data, 
                layout=c(4, 8),
                type=c('p', 'r'),
                auto.key=list(space='right')))
(atafe <- xyplot(ataf ~ TC|EF, data, 
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
(vacbespe <-xyplot(qijvacb>1 ~ TC|EF, data, 
                   layout=c(4, 8),
                   type=c('p', 'r'),
                   auto.key=list(space='right')))
(potespe <- xyplot(qijpot>1 ~ TC|EF, data, 
                   layout=c(4, 8),
                   type=c('p', 'r'),
                   auto.key=list(space='right')))
(atafespe <- xyplot(qijataf>1 ~ TC|EF, data, 
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



(vacbppvs <- xyplot(vacb_htppvs>mean(vacb_htppvs) ~ TC|AE, data,
                    layout=c(4, 5),
                    type=c('p', 'r'),
                    auto.key=list(space='right')))
(vacbpacd <- xyplot(vacb_htpacd>mean(vacb_htpacd) ~ TC|AE, data,
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


cag2003 <- filter(data, TC == 2003)  
cag2008 <- filter(data, TC == 2008)  
cag2013 <- filter(data, TC == 2013)  
cag2018 <- filter(data, TC == 2018)  

cag2003$TC <- NULL
cag2008$TC <- NULL
cag2013$TC <- NULL
cag2018$TC <- NULL

cag2003 <- mutate_all(cag2003, ~replace(., is.na(.), 0))
cag2008 <- mutate_all(cag2008, ~replace(., is.na(.), 0))
cag2013 <- mutate_all(cag2013, ~replace(., is.na(.), 0))
cag2018 <- mutate_all(cag2018, ~replace(., is.na(.), 0))

psych::describe(cag2003$ataf)
psych::describe(cag2003$pot)
psych::describe(cag2003$vacb)

psych::describe(cag2008$ataf)
psych::describe(cag2008$pot)
psych::describe(cag2008$vacb)

psych::describe(cag2013$ataf)
psych::describe(cag2013$pot)
psych::describe(cag2013$vacb)

psych::describe(cag2018$ataf)
psych::describe(cag2018$pot)
psych::describe(cag2018$vacb)


cag2003$segmentoataf <- as.factor(cut(cag2003$ataf, breaks=c(0, 1047, 6204, (6204+17420), 172671), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 
cag2003$segmentopot <- as.factor(cut(cag2003$pot, breaks=c(0, 9000, 28465, 129176, 532017), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))
cag2003$segmentovacb <- as.factor(cut(cag2003$vacb, breaks=c(-4680, 988, 5841, (5841+22127), 348991), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 



cag2008$segmentoataf <- as.factor(cut(cag2008$ataf, breaks=c(0, 1950, 9894, (9894+27439), 400706), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 

cag2008$segmentopot <- as.factor(cut(cag2008$pot, breaks=c(0, 12305, 35239, (35239+71176), 604576), 
                                     include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

cag2008$segmentovacb <- as.factor(cut(cag2008$vacb, breaks=c(-12530, 1579, 9136, (9136+39117), 578081), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 



cag2013$segmentoataf <- as.factor(cut(cag2013$ataf, breaks=c(0, 1873, 13750, (13750+79299), 1706696), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 

cag2013$segmentopot <- as.factor(cut(cag2013$pot, breaks=c(0, 11936, 36911, (36911+75866), 673517), 
                                     include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

cag2013$segmentovacb <- as.factor(cut(cag2013$vacb, breaks=c(-3305, 1625, 10170, (10170+38524), 571402), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 



cag2018$segmentoataf <- as.factor(cut(cag2018$ataf, breaks=c(0, 3231, 19788, (19788+82548), 1526730), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 

cag2018$segmentopot <- as.factor(cut(cag2018$pot, breaks=c(0, 15340, 46435, (46435+92646), 794563), 
                                     include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

cag2018$segmentovacb <- as.factor(cut(cag2018$vacb, breaks=c(-5006, 2674, 17075, (17075+53451), 686978), 
                                      include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado"))) 




tataf2003 <- dcast(cag2003, AE~segmentoataf, sum, value.var = "ataf", margins=F)
tataf2003 %>% kable(., caption="Tabla de proporciones del ATAF año 2003")

tpot2003 <- dcast(cag2003, AE~segmentopot, sum, value.var = "pot", margins=F)
tpot2003 %>% kable(., caption="Tabla de proporciones de la POT año 2003")

tvacb2003 <- dcast(cag2003, AE~segmentovacb, sum, value.var = "vacb", margins=F)
tvacb2003 %>% kable(., caption="Tabla de proporciones del VACB año 2003")


tataf2008 <- dcast(cag2008, AE~segmentoataf, sum, value.var = "ataf", margins=F)
tataf2008 %>% kable(., caption="Tabla de proporciones del ATAF año 2008")

tpot2008 <- dcast(cag2008, AE~segmentopot, sum, value.var = "pot", margins=F)
tpot2008 %>% kable(., caption="Tabla de proporciones de la POT año 2008")

tvacb2008 <- dcast(cag2003, AE~segmentovacb, sum, value.var = "vacb", margins=F)
tvacb2008 %>% kable(., caption="Tabla de proporciones del VACB año 2008")


tataf2013 <- dcast(cag2013, AE~segmentoataf, sum, value.var = "ataf", margins=F)
tataf2013 %>% kable(., caption="Tabla de proporciones del ATAF año 2013")

tpot2013 <- dcast(cag2013, AE~segmentopot, sum, value.var = "pot", margins=F)
tpot2013 %>% kable(., caption="Tabla de proporciones de la POT año 2013")

tvacb2013 <- dcast(cag2003, AE~segmentovacb, sum, value.var = "vacb", margins=F)
tvacb2013 %>% kable(., caption="Tabla de proporciones del VACB año 2013")


tataf2018 <- dcast(cag2018, AE~segmentoataf, sum, value.var = "ataf", margins=F)
tataf2018 %>% kable(., caption="Tabla de proporciones del ATAF año 2018")

tpot2018 <- dcast(cag2018, AE~segmentopot, sum, value.var = "pot", margins=F)
tpot2018 %>% kable(., caption="Tabla de proporciones de la POT año 2003")

tvacb2018 <- dcast(cag2018, AE~segmentovacb, sum, value.var = "vacb", margins=F)
tvacb2018 %>% kable(., caption="Tabla de proporciones del VACB año 2018")




mataf2003 <- mosaicplot(~segmentoataf + AE, 
                        data = cag2003, 
                        color = T, 
                        las = 1,
                        main="2003")
mpot2003 <- mosaicplot(~segmentopot + AE, 
                       data = cag2003, 
                       color = T, 
                       las = 1,
                       main="2003")

mvacb2003 <- mosaicplot(~segmentovacb + AE, 
                       data = cag2003, 
                       color = T, 
                       las = 1,
                       main="2003")


mataf2008 <- mosaicplot(~segmentoataf + AE, 
                        data = cag2008, 
                        color = T, 
                        las = 1,
                        main="2008")
mpot2008 <- mosaicplot(~segmentopot + AE, 
                       data = cag2008, 
                       color = T, 
                       las = 1,
                       main="2008")

mvacb2008 <- mosaicplot(~segmentovacb + AE, 
                        data = cag2008, 
                        color = T, 
                        las = 1,
                        main="2008")

mataf2013 <- mosaicplot(~segmentoataf + AE, 
                        data = cag2013, 
                        color = T, 
                        las = 1,
                        main="2013")
mpot2013 <- mosaicplot(~segmentopot + AE, 
                       data = cag2013, 
                       color = T, 
                       las = 1,
                       main="2013")

mvacb2013 <- mosaicplot(~segmentovacb + AE, 
                        data = cag2013, 
                        color = T, 
                        las = 1,
                        main="2013")


mataf2018 <- mosaicplot(~segmentoataf + AE, 
                        data = cag2018, 
                        color = T, 
                        las = 1,
                        main="2018")
mpot2018 <- mosaicplot(~segmentopot + AE, 
                       data = cag2018, 
                       color = T, 
                       las = 1,
                       main="2018")

mvacb2018 <- mosaicplot(~segmentovacb + AE, 
                        data = cag2018, 
                        color = T, 
                        las = 1,
                        main="2018")



teataf2003 <- table(cag2003$EF,cag2003$segmentoataf)
round(addmargins(prop.table(teataf2003), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones del ATAF año 2003")

tepot2003 <- table(cag2003$EF,cag2003$segmentopot)
round(addmargins(prop.table(tepot2003), c(1, 2)), 3) %>% kable(., caption="Tabla de proporciones de la POT año 2003")



meataf2003 <- mosaicplot(~segmentoataf + EF, 
                         data = cag2003, 
                         color = T, 
                         las = 1,
                         main="ATAF año 2003")
mepot2003 <- mosaicplot(~segmentopot + EF, 
                        data = cag2003, 
                        color = T, 
                        las = 1,
                        main="POT año 2003")


library(highcharter)

hchart(
  data_to_hierarchical(
    data =  cag2003, 
    group_vars = c(segmentoataf, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del ATAF por segmento año 2003</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2004, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2003, 
    group_vars = c(segmentopot, ID), 
    size_var = pot),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución de la POT por segmento año 2003</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2004, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2003, 
    group_vars = c(segmentovacb, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB por segmento año 2003</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2004, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2008, 
    group_vars = c(segmentoataf, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del ATAF por segmento año 2008</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2009, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2008, 
    group_vars = c(segmentopot, ID), 
    size_var = pot),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución de la POT por segmento año 2008</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2009, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2008, 
    group_vars = c(segmentovacb, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB por segmento año 2008</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2009, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2013, 
    group_vars = c(segmentoataf, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del ATAF por segmento año 2013</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2014, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2013, 
    group_vars = c(segmentopot, ID), 
    size_var = pot),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución de la POT por segmento año 2013</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2014, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2013, 
    group_vars = c(segmentovacb, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB por segmento año 2013</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2014, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2018, 
    group_vars = c(segmentoataf, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del ATAF por segmento año 2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2019, INEGI")


hchart(
  data_to_hierarchical(
    data =  cag2018, 
    group_vars = c(segmentopot, ID), 
    size_var = pot),
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


hchart(
  data_to_hierarchical(
    data =  cag2018, 
    group_vars = c(segmentovacb, ID), 
    size_var = ataf),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB por segmento año 2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos del Censo Económico año 2019, INEGI")



cagu2003 <- select(cag2003, ID, var_ue:segmentovacb) %>% column_to_rownames(., var = 'ID')
cagu2008 <- select(cag2008, ID, var_ue:segmentovacb) %>% column_to_rownames(., var = 'ID')
cagu2013 <- select(cag2013, ID, var_ue:segmentovacb) %>% column_to_rownames(., var = 'ID')
cagu2018 <- select(cag2018, ID, var_ue:segmentovacb) %>% column_to_rownames(., var = 'ID')





library(matrixcalc)

# Traza

matrix.trace(cov(select(cagu2003, pijpot:vacb_it)))
matrix.trace(cov(select(cagu2008, pijpot:vacb_it)))
matrix.trace(cov(select(cagu2013, pijpot:vacb_it)))
matrix.trace(cov(select(cagu2018, pijpot:vacb_it)))

# Coeficientes de Correlación

cor(select(cagu2003, pijpot:vacb_it))
cor(select(cagu2008, pijpot:vacb_it))
cor(select(cagu2013, pijpot:vacb_it))
cor(select(cagu2018, pijpot:vacb_it))

# Prueba Estadística

corr.test(select(cagu2003, pijpot:vacb_it))
corr.test(select(cagu2008, pijpot:vacb_it))
corr.test(select(cagu2013, pijpot:vacb_it))
corr.test(select(cagu2018, pijpot:vacb_it))

# Gráficos de Correlación

pairs.panels(select(cagu2003, pijpot:vacb_it), stars=T, 
             main="Matriz de Dispersión, Histograma y Correlación Censo 2004")

cor.plot(select(cagu2003, pijpot:vacb_it), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2004")

(cc2003 <- select(cagu2003, pijpot:vacb_it) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2004"))



pairs.panels(select(cagu2008, var_ue:vacb_it), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2009")

cor.plot(select(cagu2008, var_ue:vacb_it), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2009")

(cc2008 <- select(cagu2008, var_ue:vacb_it) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2009"))



pairs.panels(select(cagu2013, var_ue:vacb_it), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2014")

cor.plot(select(cagu2013, var_ue:vacb_it), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2014")

(cc2013 <- select(cagu2013, var_ue:vacb_it) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2014"))



pairs.panels(select(cagu2018, var_ue:vacb_it), stars=T, main="Matriz de Dispersión, Histograma y Correlación Censo 2019")

cor.plot(select(cagu2018, var_ue:vacb_it), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación Censo año 2019")

(cc2018 <- select(cagu2018, var_ue:vacb_it) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Censo año 2019"))


grid.arrange(cc2003, cc2008, cc2013, cc2018, nrow=2, 
             top= "Relaciones de correlación censales",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos  años 2004, 2009, 2014 y 2019, INEGI")


plot_num(select(cagu2003, pijpot:vacb_it))



#Conglomerados

set.seed(12345678)

clustering03 <- kmeans(select(cagu2003, pijpot:vacb_it), centers = 10)

correlations03 <- cor(select(cagu2003, pijpot:vacb_it), clustering03$cluster)

print(correlations03)



km_clusters03 <- kmeans(x = select(cagu2003, pijpot:vacb_it), centers = 7, nstart = 50)




res.dist03 <- get_dist(select(cagu2003, pijpot:vacb_it), stand = FALSE, 
                     method = "euclidean")
 
           


d03 <- as.matrix(res.dist03)

heatmap(d03, xlab = "Entidades",
        ylab = "Entidades", 
        main = "Mapa de Calor similitud de capacidades productivas año 2003")


res.hc03 <- hclust(res.dist03, "ward.D")

agrup03 <- fviz_dend(res.hc03, cex = 0.5, k = 9, rect = TRUE,
                     k_colors = "ucscgb", 
                     color_labels_by_k = TRUE,
                     main = "Agrupación estatal por capacidades productivas año 2003",
                     ylab = "Pesos en la matriz de distancia",
                     xlab = "Fuente: Elaboración propia con datos del Censo Económico 2019: INEGI", 
                     sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

agrup03

grp03 <- cutree(res.hc03, k = 9)

fviz_cluster(list(data = select(cagu2003, pijpot:vacb_it), cluster = grp03),
             ellipse.type = "convex",  rect = TRUE,
             k_colors = "uchicago", 
             color_labels_by_k = TRUE,
             repel = T, show.clust.cent = T, 
             ggtheme = theme_minimal(), main = "Agrupación por capacidades productivas año 2018",
             xlab = "fbcf_ataf", ylab = "vacb_htpot")

plot(as.phylo(res.hc03), type = "cladogram", cex = 0.8, 
     label.offset = 0.7)

plot(as.phylo(res.hc03), type = "unrooted", cex = 0.7,
     no.margin = T)


fviz_dend(res.hc03, k=9, k_colors="jco",
          type="phylogenic", repel=T)


fviz_dend(res.hc03, cex= 0.8, k= 9,
          k_colors =    "lancet", 
          color_labels_by_k = TRUE,
          type="circular", title = "Agrupación estatal por capacidades productivas año 2018",
          xlab = "Entidades Federativas", 
          sub = "Fuente: Elaboración propia con datos del Censo Económico año 2019: INEGI")

plot(as.phylo(res.hc03), type = "radial")















