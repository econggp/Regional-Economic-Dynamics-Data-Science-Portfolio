library(splm)
library(spdep)
library(sp)
library(spOata)
library(plm)


data("Insurance")
class(Insurance)

# matrix de pesos

data("itaww")
lw=spdep::mat2listw(itaww)
class(lw)
lw


# modelo pool

pool <- plm(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), model = "pooling")
summary(pool)

# prueba de dependencia espacial en panel
# H0: pool mejor alternativa

slmtest(rgdp ~ bank + rirs, data = Insurance, listw = lw, test = "lme")
slmtest(rgdp ~ bank + rirs, data = Insurance, listw = lw, test = "lml")
slmtest(rgdp ~ bank + rirs, data = Insurance, listw = lw, test = "rlml")
slmtest(rgdp ~ bank + rirs, data = Insurance, listw = lw, test = "rlme")


# efectos aleatorios sin rezago espacial

rem <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
            model = "random", listw = lw, spatial.error = "n", lag = F)
summary(rem)


# efectos aleatorios con rezago espacial

remsl <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
            model = "random", listw = lw, spatial.error = "n", lag = T)
summary(remsl)


# efectos aleatorios con error espacial

remse <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
            model = "random", listw = lw, spatial.error = "b", lag = F)
summary(remse)


# efectos aleatorios con error y rezago espacial

remsel <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
            model = "random", listw = lw, spatial.error = "b", lag = T)
summary(remsel)


# efectos fijos sin componente espacial

fem <- plm(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), model = "within")
summary(fem)


# efectos fijos con rezago espacial

femsl <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
               model = "within", listw = lw, spatial.error = "n", lag = T)
summary(femsl)


# efectos fijos con error espacial

femse <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
              model = "within", listw = lw, spatial.error = "b", lag = F)
summary(femse)


# efectos fijos con error y rezago espacial

femsel <- spml(rgdp ~ bank + rirs, data = Insurance, index = c("code", "year"), 
              model = "within", listw = lw, spatial.error = "b", lag = T)
summary(femsel)
