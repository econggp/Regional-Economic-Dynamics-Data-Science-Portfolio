data <- basecensosglobal

library(devtools)
install_github("ramnathv/htmlwidgets")
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)

tgenpromedio<- rpivotTable(data, rows="Entidad", col="AE", aggregatorName="Average", 
                   vals="ATAF")

tgenpromedio

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




(dvacb <- ggplot(data, aes(x=vacbr)) + 
  geom_histogram(color="white", fill="blue") +
  facet_wrap(~AE)+
  geom_freqpoly()+
  labs(title = "Valor Agregado Censal Bruto real promedio 2003-2018") +
  scale_fill_continuous(name="VACB"))

(dpot <- ggplot(data, aes(x=POT)) + 
  geom_histogram(color="black", fill="white") +
  facet_wrap(~AE)+
  geom_freqpoly()+
  labs(title = "Población Ocupada Total promedio 2003-2018") +
  scale_fill_continuous(name="POT"))

(dataf <- ggplot(data, aes(x=atafr)) + 
  geom_histogram(color="black", fill="white") +
  facet_wrap(~AE)+
  geom_freqpoly()+
  labs(title = "Acervo Total de Activos Fijos reales promedio 2003-2018") +
  scale_fill_continuous(name="ATAF"))

(dfbcf <- ggplot(data, aes(x=fbcfr)) + 
    geom_histogram(color="white", fill="red") +
    facet_wrap(~AE)+
    geom_freqpoly()+
    labs(title = "Formación Bruta de Capital Fijo real promedio 2003-2018") +
    scale_fill_continuous(name="FBCF"))


grid.arrange(dataf, dfbcf, dvacb, nrow=1, 
             top= "Distribución promedio variables censales 2003-2018",
             bottom= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mataf <- ddply(data, "AE", summarise, grp.mean=mean(atafr))
ggplot(data, aes(atafr, fill=factor(AE)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 1. Distribución de los acervos reales promedio 2003-2018",
       x = 'Acervo (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mpot <- ddply(data, "AE", summarise, grp.mean=mean(POT))
ggplot(data, aes(POT, fill=factor(AE)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Distribución de la Población Ocupada Total promedio 2003-2018",
       x = 'Número de Empleados',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mvacb <- ddply(data, "AE", summarise, grp.mean=mean(vacbr))
ggplot(data, aes(vacbr, fill=factor(AE)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 2. Distribución del Valor Agregado Censal Bruto real promedio 2003-2018",
       x = 'Valor Agregado (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")

mfbcf <- ddply(data, "AE", summarise, grp.mean=mean(fbcfr))
ggplot(data, aes(fbcfr, fill=factor(AE)))+
  geom_histogram(position="identity")+
  geom_vline(data=mataf, aes(xintercept=grp.mean),
             linetype="dashed")+
  labs(title = "Figura 3. Distribución de la Formación Bruta de Capital Fijo real promedio 2003-2018",
       x = 'Formación Bruta de Capital Fijo (Millones de Pesos)',
       y = 'Conteo',
       caption = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014 y 2019: INEGI")


#Productividad

psych::describe(data$vacbr)
psych::describe(data$POT)
psych::describe(data$atafr)
psych::describe(data$fbcfr)


data$segmentopot <- as.factor(cut(data$POT, breaks=c(0, 10790, 34918, 107742, 607294), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

data$segmentoatafr <- as.factor(cut(data$atafr, breaks=c(0, 2996, 16381, 66219, 487440), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  


data$segmentovacbr <- as.factor(cut(data$vacbr, breaks=c(-4851, 2567, 16598, 88454, 956694), 
                                   include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))

data$segmentofbcfr <- as.factor(cut(data$fbcfr, breaks=c(-2620, 162, 1139, 5714, 66060), 
                                    include.lowest=TRUE, labels = c("Marginal", "Bajo", "Emergente", "Consolidado")))  


matafr <- mosaicplot(~segmentoatafr + AE, 
           data = data, 
           color = T, 
           las = 1,
           main="Distribución sectorial promedio de los acervos fijos totales 2003-2018")
mpot <- mosaicplot(~segmentopot + AE, 
                        data = data, 
                        color = T, 
                        las = 1,
                        main="Distribución del empleo promedio 2003-2018")


mvacbr <- mosaicplot(~segmentovacbr + AE, 
                        data = data, 
                        color = T, 
                        las = 1,
                        main="Distribución del valor agregado promedio 2003-2018")
mfbcfr <- mosaicplot(~segmentofbcfr + AE, 
                       data = data, 
                       color = T, 
                       las = 1,
                       main="Distribución del capital fijo promedio 2003-2018")


segvacbr <- treemap(data, index = c("Entidad","AE"), 
        vSize = "vacbr", vColor= "segmentovacbr", type = "categorical",
        palette = "Set1",
        title="Segmentación del valor agregado real promedio 2003-2018")

segpot <- treemap(data, index = c("Entidad","AE"), 
                       vSize = "POT", vColor= "segmentopot", type = "categorical",
                       palette = "Set1",
                       title="Segmentación del empleo promedio 2003-2018") 

segatafr <- treemap(data, index = c("Entidad","AE"), 
                       vSize = "atafr", vColor= "segmentoatafr", type = "categorical",
                       palette = "Set1",
                       title="Segmentación de los acervos reales promedio 2003-2018")

segfbcfr <- treemap(data, index = c("Entidad","AE"), 
                      vSize = "fbcfr">0, vColor= "segmentofbcfr", type = "categorical",
                      palette = "Set1",
                      title="Segmentación del capital fijo real promedio 2003-2018") 

library(highcharter)

hchart(
  data_to_hierarchical(
    data =  data, 
    group_vars = c(segmentovacbr, ID), 
    size_var = vacbr),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del VACB real promedio 2003-2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014, 2019: INEGI")

hchart(
  data_to_hierarchical(
    data =  data, 
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
  hc_title(text = "<b>Distribución de la POT promedio 2003-2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014, 2019: INEGI")

hchart(
  data_to_hierarchical(
    data =  data, 
    group_vars = c(segmentoatafr, ID), 
    size_var = atafr),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución de los acervos reales promedio 2003-2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014, 2019: INEGI")

hchart(
  data_to_hierarchical(
    data =  data, 
    group_vars = c(segmentofbcfr, ID), 
    size_var = fbcfr),
  type = "treemap",
  dataLabels = list(
    style = list(
      fontSize = "20px",
      textOutline = FALSE
    )
  )
) |>
  hc_title(text = "<b>Distribución del capital fijo real promedio 2003-2018</b>") |>
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014, 2019: INEGI")

library(matrixcalc)

# Traza

matrix.trace(cov(select(data,var_ue:vacb_tr_pot)))

# Coeficientes de Correlación

cor(select(data, var_ue:vacb_tr_pot))


# Prueba Estadística

corr.test(select(data, var_ue:vacb_tr_pot))

# Gráficos de Correlación

pairs.panels(select(data, POT, TR, pbtr, vacbr, HTPOT, 
                    ccler, gscr, car, ctafr, itr, fbcfr, ipspctr, cspctr, 
                    atafr, afpupr, atmepr, atecpr), stars=T, 
             main="Matriz de Dispersión, Histograma y Correlación variables censales reales promedio 2003-2018")

cor.plot(select(data, POT, TR, pbtr, vacbr, HTPOT, 
                ccler, gscr, car, ctafr, itr, fbcfr, ipspctr, cspctr, 
                atafr, afpupr, atmepr, atecpr), diag = T, show.legend = T, upper=F,
         pch=21, main="Matriz de Correlación variables censales reales promedio 2003-2018")

(cc <- select(data, POT, TR, pbtr, vacbr, HTPOT, 
                  ccler, gscr, car, ctafr, itr, fbcfr, ipspctr, cspctr, 
                  atafr, afpupr, atmepr, atecpr) %>% correlate() %>% 
    network_plot(min_cor = .51, colors = c("red", "blue"), legend = "range")
  + labs(title = "Relaciones de correlación de las variables censales reales promedio 2003-2018"))


# Análisis de la segmentación

library(lattice)
library(PerformanceAnalytics)
library(GGally)
library(ggpubr)

ca <- select(data, segmentovacbr, segmentopot, segmentoatafr, segmentofbcfr, ID, Entidad, AE, var_ue:vacb_tr_pot) %>% column_to_rownames(., var = 'ID')

# Análisis regional

agu <- filter(ca, Entidad %in% c("AGU"))
bc <- filter(ca, Entidad %in% c("BC"))
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



set.seed(987654321)

trainindex = createDataPartition(agu$tr_fbcf_pot, p=0.75)$Resample1
agu_train= agu[trainindex, ]
agu_test= agu[-trainindex, ]

trainindex = createDataPartition(bc$tr_fbcf_pot, p=0.75)$Resample1
bc_train= bc[trainindex, ]
bc_test= bc[-trainindex, ]

trainindex = createDataPartition(bcs$tr_fbcf_pot, p=0.75)$Resample1
bcs_train= bcs[trainindex, ]
bcs_test= bcs[-trainindex, ]

trainindex = createDataPartition(cam$tr_fbcf_pot, p=0.75)$Resample1
cam_train= cam[trainindex, ]
cam_test= cam[-trainindex, ]

trainindex = createDataPartition(coa$tr_fbcf_pot, p=0.75)$Resample1
coa_train= coa[trainindex, ]
coa_test= coa[-trainindex, ]

trainindex = createDataPartition(col$tr_fbcf_pot, p=0.75)$Resample1
col_train= col[trainindex, ]
col_test= col[-trainindex, ]

trainindex = createDataPartition(chi$tr_fbcf_pot, p=0.75)$Resample1
chi_train= chi[trainindex, ]
chi_test= chi[-trainindex, ]

trainindex = createDataPartition(chh$tr_fbcf_pot, p=0.75)$Resample1
chh_train= chh[trainindex, ]
chh_test= chh[-trainindex, ]

trainindex = createDataPartition(cmx$tr_fbcf_pot, p=0.75)$Resample1
cmx_train= cmx[trainindex, ]
cmx_test= cmx[-trainindex, ]

trainindex = createDataPartition(dur$tr_fbcf_pot, p=0.75)$Resample1
dur_train= dur[trainindex, ]
dur_test= dur[-trainindex, ]

trainindex = createDataPartition(gua$tr_fbcf_pot, p=0.75)$Resample1
gua_train= gua[trainindex, ]
gua_test= gua[-trainindex, ]

trainindex = createDataPartition(gue$tr_fbcf_pot, p=0.75)$Resample1
gue_train= gue[trainindex, ]
gue_test= gue[-trainindex, ]

trainindex = createDataPartition(hid$tr_fbcf_pot, p=0.75)$Resample1
hid_train= hid[trainindex, ]
hid_test= hid[-trainindex, ]

trainindex = createDataPartition(jal$tr_fbcf_pot, p=0.75)$Resample1
jal_train= jal[trainindex, ]
jal_test= jal[-trainindex, ]

trainindex = createDataPartition(emx$tr_fbcf_pot, p=0.75)$Resample1
emx_train= emx[trainindex, ]
emx_test= emx[-trainindex, ]

trainindex = createDataPartition(mic$tr_fbcf_pot, p=0.75)$Resample1
mic_train= mic[trainindex, ]
mic_test= mic[-trainindex, ]

trainindex = createDataPartition(mor$tr_fbcf_pot, p=0.75)$Resample1
mor_train= mor[trainindex, ]
mor_test= mor[-trainindex, ]

trainindex = createDataPartition(nay$tr_fbcf_pot, p=0.75)$Resample1
nay_train= nay[trainindex, ]
nay_test= nay[-trainindex, ]

trainindex = createDataPartition(nle$tr_fbcf_pot, p=0.75)$Resample1
nle_train= nle[trainindex, ]
nle_test= nle[-trainindex, ]

trainindex = createDataPartition(oax$tr_fbcf_pot, p=0.75)$Resample1
oax_train= oax[trainindex, ]
oax_test= oax[-trainindex, ]

trainindex = createDataPartition(pue$tr_fbcf_pot, p=0.75)$Resample1
pue_train= pue[trainindex, ]
pue_test= pue[-trainindex, ]

trainindex = createDataPartition(que$tr_fbcf_pot, p=0.75)$Resample1
que_train= que[trainindex, ]
que_test= que[-trainindex, ]

trainindex = createDataPartition(qro$tr_fbcf_pot, p=0.75)$Resample1
qro_train= qro[trainindex, ]
qro_test= qro[-trainindex, ]

trainindex = createDataPartition(slp$tr_fbcf_pot, p=0.75)$Resample1
slp_train= slp[trainindex, ]
slp_test= slp[-trainindex, ]

trainindex = createDataPartition(sin$tr_fbcf_pot, p=0.75)$Resample1
sin_train= sin[trainindex, ]
sin_test= sin[-trainindex, ]

trainindex = createDataPartition(son$tr_fbcf_pot, p=0.75)$Resample1
son_train= son[trainindex, ]
son_test= son[-trainindex, ]

trainindex = createDataPartition(tab$tr_fbcf_pot, p=0.75)$Resample1
tab_train= tab[trainindex, ]
tab_test= tab[-trainindex, ]

trainindex = createDataPartition(tam$tr_fbcf_pot, p=0.75)$Resample1
tam_train= tam[trainindex, ]
tam_test= tam[-trainindex, ]

trainindex = createDataPartition(tla$tr_fbcf_pot, p=0.75)$Resample1
tla_train= tla[trainindex, ]
tla_test= tla[-trainindex, ]

trainindex = createDataPartition(ver$tr_fbcf_pot, p=0.75)$Resample1
ver_train= ver[trainindex, ]
ver_test= ver[-trainindex, ]

trainindex = createDataPartition(yuc$tr_fbcf_pot, p=0.75)$Resample1
yuc_train= yuc[trainindex, ]
yuc_test= yuc[-trainindex, ]

trainindex = createDataPartition(zac$tr_fbcf_pot, p=0.75)$Resample1
zac_train= zac[trainindex, ]
zac_test= zac[-trainindex, ]


# Árboles de decisión

library(rpart)
library(rpart.plot)
library(rattle)
library(tree)


set.seed(1234567890)

arbol_clasificacion <- tree(formula = segmentovacbr ~ .-segmentoatafr-segmentofbcfr
                            -var_pbt-afpup_fbcf-AE-pijvacb-tr_pot, data = agu_train,
                            mincut  = 1, minsize = 2, mindev  = 0)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 30)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
control.poda <- rpart.control(maxdepth = size_optimo, minsplit = 3)
adagu <- rpart(segmentovacbr ~ . -segmentoatafr-segmentofbcfr
               -var_pbt-afpup_fbcf-AE-pijvacb-tr_pot, data = agu_train, method = "class", control = control.poda)
gadagu <- fancyRpartPlot(adagu, main = "Clasificación del VACB en Aguascalientes",
                              sub= "Fuente: Elaboración propia con datos de los Censos Económicos años 2004, 2009, 2014, 2019: INEGI")  
predat <- predict(adagu, agu_test, type = "class")
confusionMatrix(predat, agu_test[["segmentovacbr"]])



