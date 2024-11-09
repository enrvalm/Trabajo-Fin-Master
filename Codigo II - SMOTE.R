
# SMOTE

library(caret)
library(DMwR)  # Para SMOTE
library(dplyr) 

# Cargar el dataset
telco_data <- read.csv("D:/Master BD&CD/TFM/datos.csv", sep = ";")




str(telco_data)


# Convertir los valores 0 y 1 a "No" y "Yes" para la variable SeniorCitizen   
telco_data <- telco_data %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, "No", "Yes"))


# Convertir las variables categóricas a factores
telco_data$SeniorCitizen <- as.factor(telco_data$SeniorCitizen)
categorical_columns <- c("gender", "Partner", "Dependents", "PhoneService", "MultipleLines", 
                         "InternetService", "OnlineSecurity", "OnlineBackup", 
                         "DeviceProtection", "TechSupport", "StreamingTV", 
                         "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "Churn")

# Convertir todas las columnas categóricas a factores
telco_data[categorical_columns] <- lapply(telco_data[categorical_columns], as.factor)
str(telco_data)



# Remover columnas innecesarias como customerID
telco_data <- telco_data %>% select(-customerID)

colSums(is.na(telco_data))

# Reemplazar valores vacíos en la columna "TotalCharges" y convertirla a numérica
telco_data$TotalCharges <- as.numeric(as.character(telco_data$TotalCharges))
telco_data$TotalCharges[is.na(telco_data$TotalCharges)] <- mean(telco_data$TotalCharges, na.rm = TRUE)

# Creación de nueva variable FirstMonth que nos indica con Yes si el cliente está en el primer mes
# de contrato.

# Si TotalCharges = MonthlyCharges estamos frente al primer mes de contrato del cliente, se puede crear 
# una variable llamada FirstMonth de tal manera que se binarice (1,0) y con Churn veamos el porcentaje 
# de los clientes que tras su primer – y único mes – se dan de baja

telco_data <- telco_data %>%
  mutate(FirstMonth = ifelse(MonthlyCharges == TotalCharges, "Yes", "No"))
telco_data$FirstMonth <- as.factor(telco_data$FirstMonth)


colSums(is.na(telco_data))
str(telco_data)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Variable objetivo (Target)
table(telco_data$Churn)
cat(paste0(round(prop.table(table(telco_data$Churn)) * 100, 2),"%"))
# No      Yes 
# 5174    1869 
# 73.46%  26.54%
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 5174/1869 = 2.77
# perc.over: A number that drives the decision of how many extra cases from the minority class 
# are generated (known as over-sampling).

# Por tanto se necesita usar perc.over de 277

# Aplicar SMOTE para balancear las clases en la variable objetivo 'Churn'
set.seed(123)
# help("SMOTE")
telco_data_smote <- SMOTE(Churn ~ ., data = telco_data, perc.over = 200, perc.under = 139)

# telco_data_smote <- SMOTE(Churn ~ ., data = telco_data, perc.over = 200, perc.under = 150)

# Variable objetivo (Target)
table(telco_data_smote$Churn)
cat(paste0(round(prop.table(table(telco_data_smote$Churn)) * 100, 2),"%"))


numeric_vars <- telco_data_smote %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)


# Visualización de la correlación
# install.packages("GGally")
library("GGally")
ggcorr(numeric_vars, label = TRUE)


par(mfrow = c(1, 3))

# Detectar outliers en MonthlyCharges
boxplot(telco_data_smote$MonthlyCharges, main = "Boxplot de MonthlyCharges", col = "#E36C0A")

# Detectar outliers en TotalCharges
boxplot(telco_data_smote$TotalCharges, main = "Boxplot de TotalCharges", col = "#E36C0A")

# Detectar outliers en Tenure
boxplot(telco_data_smote$tenure, main = "Boxplot de Tenure", col = "#E36C0A")


# EXPLICAR PQ DECIDO MANTENER OUTLIERS
#  * SOLO UNA PRESENTA
# . Algoritmos robustos a outliers
# Algunos algoritmos, como los árboles de decisión, Random Forest y Gradient Boosting, son inherentemente
# robustos a outliers debido a la forma en que dividen los datos. Si planeas usar modelos como estos, los 
# outliers no deberían ser un problema serio. Puedes continuar sin preocuparte por ellos.

# Árboles de decisión: Estos no se ven muy afectados por outliers porque dividen los datos en segmentos
# basados en condiciones específicas y no se basan en suposiciones sobre la distribución de los datos.
# Random Forest: Como es un conjunto de múltiples árboles de decisión, mantiene la misma ventaja de no 
# ser muy sensible a los outliers.

# SI SE TRANSFORMA A LOGARITMICA SE QUITAN LOS OUTLIERS PERO TOTAL CHARGES ME QUEDA EN ESCALA LOGARITMICA
# Y EN LOS DATOS EN CRUDO NO ERA ASÍ. ADEMAS SI LAS 3 NUMER PRESENTARAN OUTLIERS PODRÍA DEJAR TODAS
# EN DICHA ESACALA, PERO NO VOY A DEJAR UNA DE ESA MANERA Y EL RESTO EN NUM


Q1 <- quantile(telco_data_smote$TotalCharges, 0.25, na.rm = TRUE)
Q3 <- quantile(telco_data_smote$TotalCharges, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para detectar outliers
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR


# Contar cuántos valores están por debajo del límite inferior y por encima del límite superior
outliers_below <- sum(telco_data_smote$TotalCharges < lower_limit, na.rm = TRUE)
outliers_above <- sum(telco_data_smote$TotalCharges > upper_limit, na.rm = TRUE)


# Número total de outliers
total_outliers <- outliers_below + outliers_above

# Mostrar el resultado
cat("Número de outliers por debajo del límite inferior:", outliers_below, "\n")
cat("Número de outliers por encima del límite superior:", outliers_above, "\n")
cat("Número total de outliers:", total_outliers, "\n")

# ADEMAS NO SON NI 400
(396/10802)*100 #Y REPRESENTAN EL 3.66%

colSums(is.na(telco_data_smote))
str(telco_data_smote)


# 
# DIVISION DE TRAIN / TEST
set.seed(123)  # Para reproducibilidad

trainIndex <- createDataPartition(telco_data_smote$Churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- telco_data_smote[ trainIndex,]
dataTest  <- telco_data_smote[-trainIndex,]





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                               APLICACION DE MODELOS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reg Logistica:

# Entrenar el modelo de regresión logística
logistic_model <- glm(Churn ~ ., data = dataTrain, family = binomial)
summary(logistic_model)


# Predecir en el conjunto de prueba
logistic_pred <- predict(logistic_model, newdata = dataTest, type = "response")
# Convertir probabilidades a clase (0 o 1)
logistic_class <- ifelse(logistic_pred > 0.5, "Yes", "No")

# Evaluar el modelo: Matriz de confusión
confusionMatrix(as.factor(logistic_class), dataTest$Churn)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 1:
# •	Variable dependiente: Churn
# •	Variables independientes: TODAS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


variables_indep <- list(
  "gender",
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "tenure",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  "MonthlyCharges",
  "TotalCharges",
  "FirstMonth"
)

for(i in 1:length(variables_indep)){
  
  # Construir la fórmula dinámicamente
  formula_str <- paste("Churn ~", variables_indep[[i]])
  
  # Ajustar el modelo logístico
  Churn <- glm(as.formula(formula_str), data = dataTrain, family = binomial())
  
  # Imprimir el p-valor
  print(summary(Churn)$coefficients[,4])
  cat("\n")
}


# Eliminamos en una primera instancia aquellas variables que no sea significativas a nivel 25%.
# Por tanto esas no deben ser incluídas:
# MonthlyCarges


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 2:
# Ajustamos una regresión multivariable con las variables consideradas como ‘inclusivas’ en el paso 1, es decir,
# debemos introducir todas menos las anteriores comentadas.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

variables_indep_paso2 <- list(
  "gender",
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "tenure",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  # "MonthlyCharges",
  "TotalCharges",
  "FirstMonth"
)



# Construir la fórmula con todas las variables independientes
formula_str <- paste("Churn ~", paste(variables_indep_paso2, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso2 <- glm(as.formula(formula_str), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso2)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 3:
# Me quedo con las variables significativas a nivel 0.05
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


variables_indep_paso3 <- list(
  # "gender",
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "tenure",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  # "OnlineBackup",
  # "DeviceProtection",
  "TechSupport",
  # "StreamingTV",
  # "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  # "MonthlyCharges",
  # "TotalCharges",
  "FirstMonth"
)

# Construir la fórmula con todas las variables independientes
formula_str3 <- paste("Churn ~", paste(variables_indep_paso3, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso3 <- glm(as.formula(formula_str3), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso3)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 4:
# Añado al paso anterior una a una las variables no añadidas en el paso 1  MonthlyCharges
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

variables_indep_paso4_a <- list(
  # "gender",
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "tenure",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  # "OnlineBackup",
  # "DeviceProtection",
  "TechSupport",
  # "StreamingTV",
  # "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  "MonthlyCharges",
  # "TotalCharges",
  "FirstMonth"
)

# Construir la fórmula con todas las variables independientes
formula_str4_a <- paste("Churn ~", paste(variables_indep_paso4_a, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso4_a <- glm(as.formula(formula_str4_a), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso4_a)

# MonthlyCharges AHORA SI ES SIGNIFICATIVA





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(OddsPlotty)

plotty<- OddsPlotty::odds_plot(Churn_paso4_a,
                               # title = 'OR asociado a la frecuencia de estar decaído',
                               point_col = 'blue',
                               error_bar_colour = 'black',
                               point_size = 4,
                               error_bar_width = .8,
                               h_line_color = 'red')
plot <- plotty$odds_plot
plot <- plot + ggthemes::theme_few() + theme(legend.position = 'NULL') +
  labs(x='')

plotty$odds_data
print(plotty$odds_data, n = Inf)
plot +
  geom_text(label=round(plotty$odds_plot$data$OR, digits = 2),
            size=4,hjust=-.3, vjust=1.5, color='gray28') +
  scale_x_discrete(label=c('Item84_TabacoSí'='Fuma tabaco',
                           'Consumo_otrasdrogasSi'='Consume drogas ilícitas',
                           'Item84_AlcoholSí'='Consume alcohol')) +
  scale_y_discrete()

# 
# > levels(dataTrain$Churn)
# [1] "No"  "Yes" => Referencia es NO









# Predecir en el conjunto de prueba
# logistic_pred <- predict(logistic_model, newdata = dataTest, type = "response")

logistic_pred_Churn4_a <- predict(Churn_paso4_a, newdata = dataTest, type = "response")

# Convertir probabilidades a clase (0 o 1)
logistic_class <- ifelse(logistic_pred_Churn4_a > 0.5, "Yes", "No")

# Evaluar el modelo: Matriz de confusión
confusionMatrix(as.factor(logistic_class), dataTest$Churn)


# Curva ROC y AUC del modelo final de regresion logistica
library(pROC)
roc_logistic <- roc(dataTest$Churn, logistic_pred_Churn4_a)
auc(roc_logistic)

# Graficar la curva ROC
plot(roc_logistic, main = "Curva ROC - Regresión Logística")
texto <- auc(roc_logistic)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "green", cex = 1.5)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Árbol de Decisión
library(rpart)
library(rpart.plot)
tree_model <- rpart(Churn ~ ., data = dataTrain, method = "class")

# Definir colores para los nodos según el resultado "No" o "Yes"
node_colors <- ifelse(tree_model$frame$yval == 1, "#a6cee3", "#b2df8a") # Color azul para "No", verde para "Yes"

# Graficar el árbol con etiquetas y colores personalizados
rpart.plot(
  tree_model,
  type = 2,                   # Tipo de gráfico (2 significa etiquetas debajo de las ramas)
  extra = 104,                # Muestra la probabilidad y porcentaje en cada nodo
  box.palette = list("#b2df8a", "#a6cee3"), # Paleta de colores: verde y azul
  branch.lty = 3,             # Tipo de línea de las ramas (3 para línea punteada)
  nn = TRUE                   # Etiquetas de nodo
)
legend(
  x = 0.85, y = 0.9,
  legend = c("No", "Yes"),    # Etiquetas de la leyenda
  fill = c("#a6cee3", "#b2df8a"), # Colores correspondientes (Azul para No, Verde para Yes)
  border = "black",           # Borde negro en los cuadros de la leyenda
  title = "Churn"             # Título de la leyenda
)

# Predicciones
tree_predictions <- predict(tree_model, newdata = dataTest, type = "class")

# Evaluación
confusionMatrix(tree_predictions, dataTest$Churn)



# Calcular y plotear la curva ROC
tree_predictions_train <- predict(tree_model, newdata = dataTest, type = "prob")

roc_curve_final <- roc(dataTest$Churn, tree_predictions_train[,2])
plot(roc_curve_final)
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)





# K FOLDS

control <- trainControl(method = "cv", number = 10)

# Entrenar el modelo de árbol de decisión usando validación cruzada
set.seed(123)  # Fijar semilla para reproducibilidad
arbol_cv <- train(Churn ~ ., data = dataTrain, method = "rpart", trControl = control)

# Imprimir los resultados de la validación cruzada
print(arbol_cv)

# Graficar el árbol con etiquetas y colores personalizados
rpart.plot(
  arbol_cv$finalModel,
  type = 2,                   # Tipo de gráfico (2 significa etiquetas debajo de las ramas)
  extra = 104,                # Muestra la probabilidad y porcentaje en cada nodo
  box.palette = list("#b2df8a", "#a6cee3"), # Paleta de colores: verde y azul
  branch.lty = 3,             # Tipo de línea de las ramas (3 para línea punteada)
  nn = TRUE                   # Etiquetas de nodo
)
legend(
  x = 0.85, y = 0.94,
  legend = c("No", "Yes"),    # Etiquetas de la leyenda
  fill = c("#a6cee3", "#b2df8a"), # Colores correspondientes (Azul para No, Verde para Yes)
  border = "black",           # Borde negro en los cuadros de la leyenda
  title = "Churn"             # Título de la leyenda
)

# Predicciones
tree_predictions_10K <- predict(arbol_cv, newdata = dataTest, type = "prob")

# Calcular y plotear la curva ROC
roc_curve_final <- roc(dataTest$Churn, tree_predictions_10K[,2])
plot(roc_curve_final)
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)



# Predicciones de clase (no probabilidades)
tree_predictions_classes_ARB <- predict(arbol_cv, newdata = dataTest)

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(tree_predictions_classes_ARB, dataTest$Churn)

# Imprimir la matriz de confusión
print(confusion_matrix)




grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.01))
# grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.05))
# grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.02))
# grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.03))
# grid <- expand.grid(cp = seq(0.01, 0.2, by = 0.03))

arbol_tuned_SMOTE <- train(Churn ~ ., data = dataTrain, method = "rpart", trControl = control, tuneGrid = grid)

# Ver el mejor parámetro de cp
print(arbol_tuned_SMOTE$bestTune)

# Graficar el árbol con etiquetas y colores personalizados
rpart.plot(
  arbol_tuned_SMOTE$finalModel,
  type = 2,                   # Tipo de gráfico (2 significa etiquetas debajo de las ramas)
  extra = 104,                # Muestra la probabilidad y porcentaje en cada nodo
  box.palette = list("#b2df8a", "#a6cee3"), # Paleta de colores: verde y azul
  branch.lty = 3,             # Tipo de línea de las ramas (3 para línea punteada)
  nn = TRUE                   # Etiquetas de nodo
)
legend(
  x = 0.85, y = 0.94,
  legend = c("No", "Yes"),    # Etiquetas de la leyenda
  fill = c("#a6cee3", "#b2df8a"), # Colores correspondientes (Azul para No, Verde para Yes)
  border = "black",           # Borde negro en los cuadros de la leyenda
  title = "Churn"             # Título de la leyenda
)


# Predicciones
tree_predictions_HIPER <- predict(arbol_tuned_SMOTE, newdata = dataTest, type = "prob")

# Calcular y plotear la curva ROC
roc_curve_final <- roc(dataTest$Churn, tree_predictions_HIPER[,2])
plot(roc_curve_final)
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)



# Predicciones de clase (no probabilidades)
tree_predictions_classes_RANDOM <- predict(arbol_tuned_SMOTE, newdata = dataTest)

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(tree_predictions_classes_RANDOM, dataTest$Churn)

# Imprimir la matriz de confusión
print(confusion_matrix)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Random Forest
# # install.packages("randomForest")
library(randomForest)
set.seed(123)  # Fijar semilla para reproducibilidad

rf_model <- randomForest(Churn ~ ., data = dataTrain, importance = TRUE, ntree = 500)

# # Predicciones
rf_predictions <- predict(rf_model, newdata = dataTest)
# 
# # Evaluación
confusionMatrix(rf_predictions, dataTest$Churn)

# Importancia de las características
importance(rf_model)
varImpPlot(rf_model, main = "Random Forest - Train/Test")




rf_predictions_prob <- predict(rf_model, newdata = dataTest, type = "prob")


roc_curve_rf <- roc(dataTest$Churn, rf_predictions_prob[,2])
plot(roc_curve_rf, main= "Random Forest - Train/Test")
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)







# Control para 10-fold cross-validation
control_cv <- trainControl(method = "cv", number = 10)

# Entrenar el modelo usando validación cruzada
set.seed(123)  # Fijar semilla para reproducibilidad
rf_cv_model <- train(Churn ~ ., data = dataTrain, method = "rf", trControl = control_cv)

# Predicciones en el conjunto de prueba
rf_cv_predictions <- predict(rf_cv_model, newdata = dataTest)

# Matriz de confusión
confusion_matrix_rf_cv <- confusionMatrix(rf_cv_predictions, dataTest$Churn)
print(confusion_matrix_rf_cv)

# Importancia de las características
importance(rf_cv_model)
varImpPlot(rf_cv_model$finalModel, main = "Random Forest - 10K-Folds")



rf_cv_predictions_prob_10k <- predict(rf_cv_model, newdata = dataTest, type = "prob")

# Calcular y graficar la curva ROC
roc_curve_rf <- roc(dataTest$Churn, rf_cv_predictions_prob_10k[,2])
plot(roc_curve_rf,main = "Random Forest - 10K-Folds")
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)







# Búsqueda de Hiperparámetros (Hyperparameter Tuning):
# Para ajustar los hiperparámetros de los modelos de machine learning, se puede 
# emplear Grid Search o Bayesian Search. El paquete caret facilita la implementación de Grid Search.
# Grid Search usando caret:

# Definir el grid de hiperparámetros
grid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6, 7 ,8, 9, 10))
# grid <- expand.grid(mtry = c(4,6,8,10))

# Control para 10-fold cross-validation
control_cv <- trainControl(method = "cv", number = 10)

# Entrenar el modelo usando validación cruzada
set.seed(123)  # Fijar semilla para reproducibilidad
rf_cv_modelHiper <- train(Churn ~ ., data = dataTrain, method = "rf", trControl = control_cv, tuneGrid = grid)

# Predicciones en el conjunto de prueba
rf_cv_predictions_hiper <- predict(rf_cv_modelHiper, newdata = dataTest)

# Matriz de confusión
confusion_matrix_rf_cv_hip <- confusionMatrix(rf_cv_predictions_hiper, dataTest$Churn)
print(confusion_matrix_rf_cv_hip)




rf_cv_predictions_hiper_curv <- predict(rf_cv_modelHiper, newdata = dataTest, type = "prob")

# Calcular y graficar la curva ROC
roc_curve_rf <- roc(dataTest$Churn, rf_cv_predictions_hiper_curv[,2])
plot(roc_curve_rf,main = "Random Forest - 10K-Folds - Hiperparam")
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)


# Importancia de las características
varImpPlot(rf_cv_modelHiper$finalModel, main = "Random Forest - 10K-Folds - Hiperparam")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 









# # # # # # # # # # # # # # # # # # # # # NAIVE BAYES # # # # # # # # # # # # # # # # # 


library(e1071)

set.seed(123)  

nb_model <- naiveBayes(Churn ~ ., data = dataTrain)

# Realizar predicciones en el conjunto de prueba
nb_predictions <- predict(nb_model, newdata = dataTest)

# Matriz de confusión
confusion_matrix <- confusionMatrix(nb_predictions, dataTest$Churn)
print(confusion_matrix)

nb_prob <- predict(nb_model, newdata = dataTest, type = "raw")

# Calcular y graficar la curva ROC
roc_curve <- roc(dataTest$Churn, nb_prob[, 2])  # Proporción de la clase "Yes"
plot(roc_curve, main = "Naive Bayes - Train/Test")
texto <- auc(roc_curve)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)





# Control para 10-fold cross-validation
library(naivebayes)
# control <- trainControl(method = "cv", number = 10)

control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)


naivB_cv_model <- train(Churn ~ ., data = dataTrain, method = "naive_bayes", trControl = control)
print(naivB_cv_model)
# The final values used for the model were laplace = 0, usekernel = TRUE and adjust = 1.

naivB_cv_predictions <- predict(naivB_cv_model, newdata = dataTest)

# Matriz de confusión
confusion_matrix_nb_cv <- confusionMatrix(naivB_cv_predictions, dataTest$Churn)
print(confusion_matrix_nb_cv)


naivB_cv_prob <- predict(naivB_cv_model, newdata = dataTest, type = "prob")

# Calcular y graficar la curva ROC
naivB_cv_roc_curve <- roc(dataTest$Churn, naivB_cv_prob[, 2])  # Proporción de la clase "Yes"
plot(naivB_cv_roc_curve, main = "Naive Bayes - 10K-Folds")
texto <- auc(naivB_cv_roc_curve)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)








# Búsqueda de Hiperparámetros (Hyperparameter Tuning):
# Para ajustar los hiperparámetros de los modelos de machine learning, se puede 
# emplear Grid Search o Bayesian Search. El paquete caret facilita la implementación de Grid Search.
# Grid Search usando caret:

# Definir el grid de hiperparámetros
grid <- expand.grid(laplace = c(0, 0.025, 0.5), usekernel = c(TRUE, FALSE), adjust = c(0, 0.5, 1))

# Control para 10-fold cross-validation
# control_cv <- trainControl(method = "cv", number = 10)

# Entrenar el modelo usando validación cruzada
set.seed(123)  # Fijar semilla para reproducibilidad
naivB_cv_modelHiper <- train(Churn ~ ., data = dataTrain, method = "naive_bayes", trControl = control, tuneGrid = grid)

print(naivB_cv_modelHiper)

# Predicciones en el conjunto de prueba
naivB_cv_predictions_hiper <- predict(naivB_cv_modelHiper, newdata = dataTest)

# Matriz de confusión
confusion_matrix_naivB_cv_hip <- confusionMatrix(naivB_cv_predictions_hiper, dataTest$Churn)
print(confusion_matrix_naivB_cv_hip)




naivB_cv_predictions_hiper_curv <- predict(naivB_cv_modelHiper, newdata = dataTest, type = "prob")

# Calcular y graficar la curva ROC
roc_curve_naivB_cv <- roc(dataTest$Churn, naivB_cv_predictions_hiper_curv[,2])
plot(roc_curve_naivB_cv,main = "Naive Bayes - 10K-Folds - Hiperparam")
texto <- auc(roc_curve_naivB_cv)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
