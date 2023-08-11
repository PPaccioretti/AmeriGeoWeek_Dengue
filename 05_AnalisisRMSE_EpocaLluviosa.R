st_read('results/datos_dengue_finalCurso.gpkg')

# Mayo a Octubre -> Lluvioso
datos_dengue$epoca_lluviosa <- 
  as.Date(sprintf("%04d-5-1", 
                  datos_dengue$anio),
          format = "%Y-%m-%d") < datos_dengue$sem_anio &
  
  as.Date(sprintf("%04d-10-31", 
                  datos_dengue$anio),
          format = "%Y-%m-%d")[1] > 
  as.Date(datos_dengue$sem_anio[1], format = "%Y-%m-%d")

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2))
}

rmse(
  datos_dengue$tasa,
  datos_dengue$predict_rf)

misRMSE <- 
  datos_dengue |> 
  st_drop_geometry() |> 
  group_by(NOM_CANT, anio, epoca_lluviosa) |> 
  summarise(rmse = rmse(tasa, predict_rf))


misRMSE_sf <- merge(polig_cantones,
                    misRMSE)


miPlot <- misRMSE_sf |> 
  filter(anio == 2020) |> 
  ggplot(aes(fill = rmse, canton = NOM_CANT)) +
  geom_sf() +
  facet_wrap(vars(epoca_lluviosa))

plotly::ggplotly(miPlot)


misCasos <- 
  datos_dengue |> 
  st_drop_geometry() |> 
  group_by(NOM_CANT, anio, epoca_lluviosa) |> 
  summarise(rmse = rmse(tasa, predict_rf),
            casos_totales = sum(casos))

misCasos_sf <- merge(polig_cantones,
                    misCasos)


miPlot_casos <- misCasos_sf |> 
  filter(anio == 2020) |> 
  ggplot(aes(fill = casos_totales, canton = NOM_CANT)) +
  geom_sf() +
  facet_wrap(vars(epoca_lluviosa))



miPlot_casos + 
  miPlot + 
  patchwork::plot_layout(ncol = 1)


