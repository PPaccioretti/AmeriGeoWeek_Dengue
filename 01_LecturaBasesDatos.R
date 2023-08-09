# Load Packages ----
library(sf)
library(readxl)
library(tidyr)
library(dplyr)
library(terra)
library(ggplot2)
library(patchwork)
# Lectura base de datos dengue ----
hojas <-
  excel_sheets('data/Costa-Rica_weekly-canton_dengue_2019-2023.xlsx')
mihoja <- hojas[3]
datos_dengue <- lapply(hojas, function(mihoja) {
  datos_dengue <-
    read_excel(
      'data/Costa-Rica_weekly-canton_dengue_2019-2023.xlsx',
      skip = 6,
      sheet = mihoja,
      col_types = c('text',
                    rep('numeric', 55))
    ) |>
    na.omit()
  # Se eliminan las sumas por provincia
  datos_dengue <- datos_dengue[-c(2, 23, 39, 48, 59, 71, 83),]
  
  # datos_dengue[datos_dengue == 'N/D'] <- NA
  
  names(datos_dengue)[1] <- 'NOM_CANT'
  datos_dengue <- pivot_longer(
    datos_dengue,
    cols = !c(NOM_CANT, Nº, TASA),
    names_to = 'semana',
    values_to = 'casos'
  )
  datos_dengue$semana <-
    as.numeric(as.character(datos_dengue$semana))
  datos_dengue$anio <- mihoja
  is_total <- datos_dengue[['NOM_CANT']] == "TOTAL DEL PAIS"
  
  if (mihoja == "2023") {
    datos_dengue$casos[datos_dengue$semana > 25] <- NA
  }
  
  datos_dengue[['NOM_CANT']] <-
    gsub("CORONADO",
         "VAZQUEZ DE CORONADO",
         datos_dengue[['NOM_CANT']])
  
  datos_dengue[['NOM_CANT']] <-
    gsub("AGUIRRE",
         "QUEPOS",
         datos_dengue[['NOM_CANT']])
  
  datos_dengue[['NOM_CANT']] <-
    gsub("ALFARO RUIZ",
         "ZARCERO",
         datos_dengue[['NOM_CANT']])
  
  datos_dengue_total <- datos_dengue[is_total, ]
  datos_dengue_canton <- datos_dengue[!is_total, ]
  
  list('total' = datos_dengue_total,
       'canton' = datos_dengue_canton)
})


datos_dengue_total <- lapply(datos_dengue, '[[', 'total') |>
  do.call(rbind, args = _)

datos_dengue_canton <- lapply(datos_dengue, '[[', 'canton') |>
  do.call(rbind, args = _) |> 
  mutate(lugar = tolower(NOM_CANT)) 

# Lectura base de datos Poblacion ----

hojas <-
  excel_sheets('data/repoblacev2011-2025-03_2.xlsx')
hojas <- hojas[hojas %in% 2019:2023]

datos_poblacion <- lapply(hojas, function(mihoja) {
  pop_por_distrito <-
    read_xlsx(
      'data/repoblacev2011-2025-03_2.xlsx',
      skip = 7,
      sheet = mihoja,
      trim_ws = FALSE
    ) |>
    dplyr::select(1:2) |>
    rename(lugar = `...1`,
           poblacion = `...2`) |>
    mutate(lugar = tolower(lugar),
           anio = as.numeric(mihoja))
  
  misFilasSelect <- seq_len(which(grepl(
    'Hombre',
    pop_por_distrito[[1]],
    ignore.case = TRUE
  )) - 2)
  
  pop_por_distrito <- pop_por_distrito[misFilasSelect,]
  
  # fila_total <- 2
  # fila_provincia <- c(3, 145, 274, 334, 392, 463, 532)
  fila_canton <-
    c(4, 16, 20, 34, 44, 48, 56, 63, 71, 78, 84, 90, 96, 102, 106, 
      111, 117, 121, 126, 138, 146, 161, 175, 184, 189, 198, 207, 215, 
      221, 227, 241, 249, 255, 265, 269, 275, 287, 293, 302, 306, 319, 
      323, 329, 335, 341, 348, 357, 364, 370, 375, 379, 383, 386, 393, 
      399, 407, 417, 422, 427, 433, 438, 446, 453, 458, 464, 480, 486, 
      496, 500, 507, 511, 516, 522, 524, 529, 443, 533, 538, 546, 553, 
      558, 562)
  
  pop_por_distrito_unico <- pop_por_distrito[fila_canton, ]
  
  custom_rules <- "ñ > \\~;
                 Ñ > \\^;
                 ::Latin-ASCII;
                 \\~ > ñ;
                 \\^ > Ñ;
                  AGUIRRE > QUEPOS;
                  aguirre > quepos"
  pop_por_distrito_unico$lugar <- 
    stringi::stri_trans_general(
      str = pop_por_distrito_unico$lugar, 
      id = custom_rules, 
      rules = TRUE)
  
  pop_por_distrito_unico
  
})

datos_poblacion <- do.call(rbind, datos_poblacion)

datos_dengue_canton_pop <- 
  datos_dengue_canton |> 
  merge(datos_poblacion,
        sort = FALSE) |> 
  select(-lugar)
  

# Lectura base de datos climática ----
datos_terra <-
  rast('data/ERA5 2019-2023 Costa Rica Environmental Variables.nc')

my_lyers <- gsub("=.*", "\\1",
                 names(datos_terra),
                 perl = TRUE)
table(my_lyers)

my_rasters_weekly <- lapply(unique(my_lyers), function(x) {
  select <- my_lyers == x
  my_band <- datos_terra[[select]]
  
  myDates <- format(time(my_band), format = '%U %Y')
  
  if (x == "tp_expver") {
    my_fun = 'sum'
  } else {
    my_fun = 'mean'
  }
  y <- tapp(my_band, myDates, my_fun, na.rm = TRUE)
  names(y) <- paste(x, names(y), sep = "-")
  varnames(y) <- varnames(my_band)
  longnames(y) <- longnames(my_band)
  units(y) <- unique(units(my_band))
  y
})

my_rasters_weekly <- rast(my_rasters_weekly)


# Lectura de cantones en formato espacial ----

polig_cantones <- st_read('data/cantones.gpkg')

polig_cantones[['NOM_CANT']] <-
  gsub("AGUIRRE",
       "QUEPOS",
       polig_cantones[['NOM_CANT']])

polig_cantones[['NOM_CANT']] <-
  gsub("ALFARO RUIZ",
       "ZARCERO",
       polig_cantones[['NOM_CANT']])


# Union de archivos ----
my_climatic_data <- 
  terra::extract(my_rasters_weekly,
                 vect(polig_cantones),
                 fun = mean) |> 
  dplyr::select(-ID)

polig_cantones_clima <- cbind(polig_cantones,
                              my_climatic_data) |> 
  dplyr::select(-ID)

poligonos_cantones_clima <-
  polig_cantones_clima |>
  pivot_longer(
    cols = matches("\\d{4}"),
    names_to = c("variable", "semana", "anio"),
    names_pattern = "(.*)_expver\\.X(.*)\\.(\\d{4})",
    values_to = "valor"
  ) |>
  pivot_wider(names_from = "variable",
              values_from = "valor",
              values_fill = NA) |> 
  mutate(semana = as.numeric(semana),
         anio = as.numeric(anio))


datos_dengue <-
  merge(poligonos_cantones_clima,
        datos_dengue_canton_pop) |> 
  filter(!is.na(casos)) |> 
  # subset(!is.na(datos_dengue_canton_pop$casos)) |>
  dplyr::select(
    -c(
      "Nº",
      "TASA",
      "OBJECTID",
      "COD_PROV",
      "COD_CANT",
      # "COD_DIST",
      "CODIGO",
      "NOM_PROV",
      "NOM_DIST"
    )
  ) |> 
  mutate(semana = as.numeric(semana),
         anio = as.numeric(anio),
         sem_anio = as.Date(sprintf("%04d-%02d-1", 
                                    anio, 
                                    semana),
                            format = "%Y-%U-%u"),
         tasa = casos / poblacion * 100000
  )

datos_dengue_anual <- 
  datos_dengue |>
  st_drop_geometry() |> 
  group_by(NOM_CANT, anio, ) |> 
  summarise(tasa = sum(casos) / unique(poblacion) * 100000) 

datos_dengue_anual_sf <- merge(polig_cantones,
                               datos_dengue_anual)


