# Load Packages ----
library(ggplot2)
library(patchwork)

# Graficos descriptivos ----
ggplot(datos_dengue_anual_sf) +
  geom_sf(aes(fill = tasa)) +
  facet_wrap(vars(anio), ncol = 3)


ggplot(datos_dengue,
       aes(sem_anio, casos, group = NOM_CANT, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = "none") +
  
  ggplot(datos_dengue,
         aes(sem_anio, tasa, group = NOM_CANT, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = "none")

cantones_tasa_alta <- 
  datos_dengue |> 
  st_drop_geometry() |> 
  group_by(NOM_CANT) |> 
  summarise(cantones_tasa_alta = any(tasa > 75)) 

datos_dengue_tasa_alta <- merge(
  datos_dengue,
  cantones_tasa_alta
)


datos_dengue_tasa_alta |> 
  filter(cantones_tasa_alta) |> 
  ggplot(aes(sem_anio, tasa, group = NOM_CANT, color = NOM_CANT)) +
  geom_line() 


