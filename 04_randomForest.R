# Load Packages ----
library(caret)
library(randomForest)
library(parallel)
library(doParallel)


y = 'tasa'
my_x = c(
  'u10',
  'v10',
  'd2m',
  't2m',
  'e',
  'cvh',
  'lai_hv',
  'lai_lv',
  'cvl',
  'stl1',
  'tp',
  'swvl1',
  'anio',
  'semana',
  'NOM_CANT'
)

my_xs <- st_drop_geometry(datos_dengue)[, my_x]


if (!file.exists('results/rf_tasa_yx_cv.RDS')) {
  # create the cluster for caret to use
  no_cores <-  max(1, detectCores() - 1)
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  
  fitControl <- trainControl(method = "cv",
                             number = 5,
                             allowParallel = TRUE)
  seed <- 7
  train_rf_yx <- train(
    y = datos_dengue$tasa,
    x = my_xs,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE,
    importance = TRUE,
    metric = 'RMSE',
    na.action = na.omit
  )
  stopCluster(cl)
  saveRDS(train_rf_yx, file = "rf_tasa_yx_cv.RDS")
} else {
  train_rf_yx <- readRDS('results/rf_tasa_yx_cv.RDS')
}

train_rf_yx

train_rf_yx$finalModel

importancia <-
  as.data.frame(randomForest::importance(train_rf_yx$finalModel))
importancia <-
  cbind("Variable" = rownames(importancia), importancia)
importancia <-
  importancia[order(importancia$`%IncMSE`, decreasing = T), ]

ggplot(data = importancia,
       aes(
         x = reorder(Variable, `%IncMSE`),
         y = `%IncMSE`,
         fill = `%IncMSE`
       )) +
  labs(x = "Variable",
       y = "Aporte de la variable",
       fill = "") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

datos_dengue$predict_rf <-
  predict(train_rf_yx,
          newdata = st_drop_geometry(datos_dengue))


ggplot(datos_dengue, aes(sem_anio, tasa, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none') +
  
  ggplot(datos_dengue, aes(sem_anio, predict_rf, color = NOM_CANT)) +
  geom_line() +
  theme(legend.position = 'none')



partialPlot(train_rf_yx$finalModel,
            st_drop_geometry(datos_dengue),
            'anio')

partialPlot(train_rf_yx$finalModel,
            st_drop_geometry(datos_dengue),
            'anio')

png(
  filename = "results/partialPlots.png",
  width = 1100,
  height = 700)
op <- par(mfrow = c(4, 4))
for (i in seq_along(my_x)) {
  partialPlot(
    train_rf_yx$finalModel,
    st_drop_geometry(datos_dengue),
    my_x[i],
    xlab = my_x[i],
    main = paste("Partial Dependence on", my_x[i]),
    ylim = c(2, 8)
  )
}
par(op)
dev.off()
