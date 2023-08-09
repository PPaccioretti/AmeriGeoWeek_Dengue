# library(pscl)
# library(sf)
# library(MASS)
# library(dplyr)
# library(nlme)
# library(ggplot2)
# library("lattice")
# library(lme4)
# library(patchwork)
# library(car)
# library(modelsummary)
# library(performance)

ggplot(datos_dengue, aes(casos)) +
  geom_histogram() +
  ggplot(datos_dengue, aes(tasa)) +
  geom_histogram()



datos_dengue |>
  st_drop_geometry() |>
  group_by(NOM_CANT, anio) |>
  summarize(prop_datosCero = mean(casos == 0)) |>
  filter(prop_datosCero < 0.05 | prop_datosCero > 0.97) |> 
  View()

datos_dengue |>
  st_drop_geometry() |>
  group_by(NOM_CANT) |>
  summarize(prop_datosCero = mean(casos == 0)) |>
  filter(prop_datosCero < 0.05 | prop_datosCero > 0.97) |> 
  View()


ggplot(Frec_casos_canton,
       aes(prop_datosCero)) +
  geom_histogram() +
  theme(legend.position = 'none')

# Modelo Generalizados ----
## Poisson ----
denguepois_AEWe <-
  glm(
    casos ~ 1 + u10 + v10 + d2m + t2m + e + cvh + lai_hv + lai_lv + cvl + stl1 +
      tp  + swvl1 + anio + semana + NOM_CANT,
    data = datos_dengue,
    family = poisson(link = "log")
  )
summary(denguepois_AEWe)

deviance(denguepois_AEWe) / denguepois_AEWe$df.residual


## Poisson + Offset----
denguepois_offset_AEWe <-
  glm(
    casos ~ 1 + u10 + v10 + d2m + t2m + e + cvh + lai_hv + lai_lv + cvl + stl1 +
      tp  + swvl1 + anio + semana + NOM_CANT + offset(log(poblacion)),
    data = datos_dengue,
    family = poisson(link = "log")
  )
summary(denguepois_offset_AEWe)

deviance(denguepois_offset_AEWe) / denguepois_offset_AEWe$df.residual



## Poisson + Offset + Interacciones----
denguepois_offset_interacc <-
  glm(
    casos ~ 1 + u10 + v10 + d2m + t2m + e + cvh + lai_hv + lai_lv + cvl + stl1 +
      tp  + swvl1 + anio + semana + NOM_CANT + (anio + semana)^2 + offset(log(poblacion)),
    data = datos_dengue,
    family = poisson(link = "log")
  )
summary(denguepois_offset_interacc)

deviance(denguepois_offset_interacc) / denguepois_offset_interacc$df.residual


datos_dengue$predicho_glm_poisOffst <- 
  predict(denguepois_offset_interacc,
          type = "response")


ggplot(datos_dengue, aes(sem_anio, predicho_glm_poisOffst, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none')


## Poisson + Offset----
denguepois_offset_interaccion3 <-
  glm(
    casos ~ 1 + u10 + v10 + d2m + t2m + e + cvh + lai_hv + lai_lv + cvl + stl1 +
      tp  + swvl1 + (anio + semana + NOM_CANT)^3 + offset(log(poblacion)),
    data = datos_dengue,
    family = poisson(link = "log")
  )
summary(denguepois_offset_interaccion3)

deviance(denguepois_offset_interaccion3) / denguepois_offset_interaccion3$df.residual


datos_dengue$predicho_glm_poisOffst_intrcc3 <- 
  predict(denguepois_offset_interaccion3, type = "response")


ggplot(datos_dengue, aes(sem_anio, casos, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none') +

# ggplot(datos_dengue, aes(sem_anio, predicho_glm_poisOffst, color = NOM_CANT)) +
#   geom_line() +
#   theme(legend.position = 'none')

ggplot(datos_dengue, aes(sem_anio, predicho_glm_poisOffst_intrcc3, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none')


ggplot(datos_dengue, aes(casos, predicho_glm_poisOffst, color = NOM_CANT)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none')

## Binomial Negativa ----

dengueCRnb <- MASS::glm.nb(
  casos ~ 1 + u10 + v10 + d2m + t2m + e + cvh + lai_hv + lai_lv + cvl + stl1 +
    tp  + swvl1 + anio + semana + NOM_CANT + (anio + semana + NOM_CANT)^3 + offset(log(poblacion)),
  data = datos_dengue
)

summary(dengueCRnb)

deviance(dengueCRnb) / dengueCRnb$df.residual

datos_dengue$predicho_glm_nb <- predict(dengueCRnb, type = "response")

ggplot(datos_dengue, aes(sem_anio, casos, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none') +
  
  ggplot(datos_dengue, aes(sem_anio, predicho_glm_nb, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none') +
  
  ggplot(datos_dengue,
         aes(sem_anio, predicho_glm_poisOffst, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none')


ggplot(datos_dengue, aes(casos, predicho_glm_nb, color = NOM_CANT)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none') +
  
  ggplot(datos_dengue,
         aes(casos, predicho_glm_poisOffst, color = NOM_CANT)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none')

