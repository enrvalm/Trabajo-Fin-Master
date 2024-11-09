
# Enrique Valentín Manzano



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Carga de datos
datos_original <- read.csv("D:/Master BD&CD/TFM/datos.csv", sep = ";")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Eliminacion de customerID:
library(dplyr)
datos <- datos_original
datos <- datos %>% select(-customerID)
# remove(datos_original)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Primer análisis general de la base de datos:
head(datos, 5)

summary(datos)
colSums(is.na(datos))
sum(datos == "")
# OJO con ese NA de TotalCharges (previsiblemente se eliminarán esas 11 filas)

# Va relacionado con que el minimo de Tenure: meses que el cliente ha permanecido en la compañía sea 0)
which.min(datos$tenure)
print(datos[366,])

which(is.na(datos)) # coincide con las filas donde TotalCharges es NA, solamente tenemos NA para esta variable.

str(datos)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Eliminar los NA de la variable TotalCharges por su media, así no pierdo demasiada información
# datos <- datos[!is.na(datos$TotalCharges), ]
datos$TotalCharges <- as.numeric(as.character(datos$TotalCharges))
datos$TotalCharges[is.na(datos$TotalCharges)] <- mean(datos$TotalCharges, na.rm = TRUE)
summary(datos)
colSums((is.na(datos)))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





# Convertir los valores 0 y 1 a "No" y "Yes" para la variable SeniorCitizen   
datos <- datos %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, "No", "Yes"))

head(datos, 5)
str(datos)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Tablas de frecuencias:
Tablas_Freq <- function(data) {
  for (var in colnames(data)) {
    if (!is.numeric(data[[var]])) {
      cat("\n", "Tabla de frecuencias para la variable", var)
      freq_table <- table(data[[var]])
      print(freq_table)

      cat("\n", "Frecuencia relativa:\n", paste0(round(prop.table(freq_table) * 100, 2),"%", "\t"))
      cat("\n\n\n")
    }
  }
}

Tablas_Freq(datos)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Variable objetivo (Target)
table(datos$Churn)
cat(paste0(round(prop.table(table(datos$Churn)) * 100, 2),"%"))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALISIS UNIVARIADO

library(dplyr)

valores_distintos <- datos_original %>% 
  summarise(n_distinct(customerID))
valores_distintos


library(paletteer)  
library(ggplot2)
library(patchwork)


pastel_colors <- paletteer_d("RColorBrewer::Pastel1")

G2 <- ggplot(datos_original, aes(x = PhoneService, fill = PhoneService)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de servicio telefónico", x = "Phone Service", y = "Frecuencia")

G8 <- ggplot(datos_original, aes(x = MultipleLines, fill = MultipleLines)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No phone service" = pastel_colors[3])) +
  labs(title = "Distribución de múltiples líneas", x = "Multiple Lines", y = "Frecuencia")

G9 <- ggplot(datos_original, aes(x = InternetService, fill = InternetService)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "DSL" = pastel_colors[2], "Fiber optic" = pastel_colors[3])) +
  labs(title = "Distribución de servicio de internet contratado", x = "Internet Service", y = "Frecuencia")

G10 <- ggplot(datos_original, aes(x = OnlineSecurity, fill = OnlineSecurity)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de seguridad online", x = "Online Security", y = "Frecuencia")

G11 <- ggplot(datos_original, aes(x = OnlineBackup, fill = OnlineBackup)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de respaldo online", x = "Online Backup", y = "Frecuencia")


G12 <- ggplot(datos_original, aes(x = DeviceProtection, fill = DeviceProtection)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de protección contra dispositivos", x = "Device Protection", y = "Frecuencia")

G13 <- ggplot(datos_original, aes(x = TechSupport, fill = TechSupport)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de soporte técnico", x = "Tech Support", y = "Frecuencia")

G14 <- ggplot(datos_original, aes(x = StreamingTV, fill = StreamingTV)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de contenido multimedia por internet en TV", x = "Streaming TV", y = "Frecuencia")

G15 <- ggplot(datos_original, aes(x = StreamingMovies, fill = StreamingMovies)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2], "No internet service" = pastel_colors[3])) +
  labs(title = "Distribución de contenido multimedia por internet para películas", x = "Streaming Movies", y = "Frecuencia")

G16 <- ggplot(datos_original, aes(x = Contract, fill = Contract)) +
  geom_bar() +
  scale_fill_manual(values = c("Month-to-month" = pastel_colors[1], "One year" = pastel_colors[2], "Two year" = pastel_colors[3])) +
  labs(title = "Distribución del tipo de contrato del cliente", x = "Contract", y = "Frecuencia")

G17 <- ggplot(datos_original, aes(x = PaymentMethod, fill = PaymentMethod)) +
  geom_bar() +
  scale_fill_manual(values = c("Credit card (automatic)" = pastel_colors[1], "Bank transfer (automatic)" = pastel_colors[2], "Mailed check" = pastel_colors[3], "Electronic check" = pastel_colors[4])) +
  labs(title = "Distribución del método de pago del cliente", x = "Payment Method", y = "Frecuencia")

G3 <- ggplot(datos_original, aes(x = PaperlessBilling, fill = PaperlessBilling)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de facturación electrónica", x = "Paperless Billing", y = "Frecuencia")

G5 <- ggplot(datos_original, aes(x = SeniorCitizen, fill = SeniorCitizen)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de cliente (persona mayor)", x = "Senior Citizen", y = "Frecuencia")

G6 <- ggplot(datos_original, aes(x = Partner, fill = Partner)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de clientes con pareja", x = "Partner", y = "Frecuencia")

G7 <- ggplot(datos_original, aes(x = Dependents, fill = Dependents)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de clientes con gente a su cargo", x = "Dependents", y = "Frecuencia")

G4 <- ggplot(datos_original, aes(x = gender, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Female" = pastel_colors[1], "Male" = pastel_colors[2])) +
  labs(title = "Distribución del género", x = "Gender", y = "Frecuencia")

G1 <- ggplot(datos_original, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = c("No" = pastel_colors[1], "Yes" = pastel_colors[2])) +
  labs(title = "Distribución de abandono de la compañía", x = "Churn", y = "Frecuencia")


G2 + G8 + G9 + G10
G11 + G12 + G13 + G14
G15 + G16 + G17 + G3
G4 + G5 + G6 + G7
G1





G18 <- ggplot(datos_original, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "#E36C0A") +
  labs(title = "Distribución de meses en la compañía", x = "Tenure", y = "Frecuencia")

G19 <- ggplot(datos_original, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 1, fill = "#E36C0A") +
  labs(title = "Distribución de cargos mensuales", x = "Monthly Charges", y = "Frecuencia")

G20 <- ggplot(datos_original, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 1, fill = "#E36C0A") +
  labs(title = "Distribución de cargos totales", x = "Total Charges", y = "Frecuencia")

G18 + G19 + G20




par(mfrow = c(1, 3))

# Detectar outliers en Tenure
boxplot(datos_original$tenure, main = "Boxplot de Tenure", col = "#E36C0A")

# Detectar outliers en MonthlyCharges
boxplot(datos_original$MonthlyCharges, main = "Boxplot de MonthlyCharges", col = "#E36C0A")

# Detectar outliers en TotalCharges
boxplot(datos_original$TotalCharges, main = "Boxplot de TotalCharges", col = "#E36C0A")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALISIS BIVARIADO

numeric_vars <- datos %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Seleccionamos solo las columnas numéricas
numericas <- datos[, sapply(datos, is.numeric)]

# Calculamos la matriz de correlación
correlacion_numericas <- cor(numericas, method = "pearson")  
print(correlacion_numericas)


library(corrplot)
corrplot(correlacion_numericas, method = "color", type = "upper", tl.col = "black")




# V Cramer
library(vcd)

categoricas <- datos[, sapply(datos, is.factor)]

cramer_matrix <- matrix(NA, ncol = ncol(categoricas), nrow = ncol(categoricas),
                        dimnames = list(names(categoricas), names(categoricas)))

for (i in 1:ncol(categoricas)) {
  for (j in i:ncol(categoricas)) {
    cramer_matrix[i, j] <- assocstats(table(categoricas[, i], categoricas[, j]))$cramer
  }
}

print(cramer_matrix)

library(reshape2)
cramer_long <- melt(cramer_matrix, na.rm = TRUE)

# matriz con ggplot2
ggplot(data = cramer_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  theme_minimal() +
  labs(title = "Cramer's V Correlation Heatmap",
       x = "Variables", y = "Variables", fill = "Cramer's V") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Convertir variables categóricas en factores

datos$gender <- as.factor(datos$gender)
datos$SeniorCitizen <- as.factor(datos$SeniorCitizen)
datos$Partner <- as.factor(datos$Partner)
datos$Dependents <- as.factor(datos$Dependents)
datos$PhoneService <- as.factor(datos$PhoneService)
datos$MultipleLines <- as.factor(datos$MultipleLines)
datos$InternetService <- as.factor(datos$InternetService)
datos$OnlineSecurity <- as.factor(datos$OnlineSecurity)
datos$OnlineBackup <- as.factor(datos$OnlineBackup)
datos$DeviceProtection <- as.factor(datos$DeviceProtection)
datos$TechSupport <- as.factor(datos$TechSupport)
datos$StreamingTV <- as.factor(datos$StreamingTV)
datos$StreamingMovies <- as.factor(datos$StreamingMovies)
datos$Contract <- as.factor(datos$Contract)
datos$PaperlessBilling <- as.factor(datos$PaperlessBilling)
datos$PaymentMethod <- as.factor(datos$PaymentMethod)
datos$Churn <- as.factor(datos$Churn)

str(datos)

# Creación de nueva variable FirstMonth que nos indica con Yes si el cliente está en el primer mes
# de contrato.

# Si TotalCharges = MonthlyCharges estamos frente al primer mes de contrato del cliente, se puede crear 
# una variable llamada FirstMonth de tal manera que se binarice (1,0) y con Churn veamos el porcentaje 
# de los clientes que tras su primer – y único mes – se dan de baja

datos <- datos %>%
  mutate(FirstMonth = ifelse(MonthlyCharges == TotalCharges, "Yes", "No"))


# Convertir FirstMonth en factor
# typeof(datos$FirstMonth)
datos$FirstMonth <- as.factor(datos$FirstMonth)
typeof(datos$FirstMonth)
str(datos)
which(is.na(datos$FirstMonth))


# CON ESTA NUEVA VARIABLE PUEDE SER INTERESANTE ESTUDIAR EL % DE AQUELLOS QUE EN SU PRIMER MES SE VAN DE LA EMPRESA
ggplot(datos, aes(x = FirstMonth, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Relación entre Primer Mes y Churn", x = "Primer mes", y = "Frecuencia")



# Calcular el porcentaje de churn en el primer mes
first_month_churn <- datos %>%
  filter(FirstMonth == "Yes") %>%
  summarise(ChurnRate = mean(as.numeric(Churn) - 1))  # as.numeric(Churn) - 1 para convertir "Yes"/"No" a 1/0

print(first_month_churn)
# 0.6199021 => Significa que el 62% de los que se encuentran en el primer mes de contrato terminan dejando la compañía tras
# ese mes transcurrido

tabla_casos <- table(datos$FirstMonth, datos$Churn)

#       No  Yes
# No  4941 1489
# Yes  233  380 
# => PrimerMes: Yes y Churn: Yes => 380
# => PrimerMes: Yes y Churn: No  => 233 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# División de los datos
library(caret)
set.seed(123)  # Para reproducibilidad

trainIndex <- createDataPartition(datos$Churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- datos[ trainIndex,]
dataTest  <- datos[-trainIndex,]

# Otra forma:
# index_diab <- sample(x = nrow(diab), size= nrow(diab)*0.8)
# diab_train <- diab[index_diab, ]
# diab_test <- diab[-index_diab, ]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reg Logistica:

# Entrenar el modelo de regresión logística
logistic_model <- glm(Churn ~ ., data = dataTrain, family = binomial)
summary(logistic_model)




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
# gender, PhoneService.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 2:
# Ajustamos una regresión multivariable con las variables consideradas como ‘inclusivas’ en el paso 1, es decir,
# debemos introducir todas menos las anteriores comentadas.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

variables_indep_paso2 <- list(
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "tenure",
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



# Construir la fórmula con todas las variables independientes
formula_str <- paste("Churn ~", paste(variables_indep_paso2, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso2 <- glm(as.formula(formula_str), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso2)



# Ahora debo eliminar las variables a niveles significativos tradicionales (0.05) que no aportan información al
# modelo, teniamos del modelo completo como significiativas


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Paso 3:
# Me quedo con las variables significativas a nivel 0.05
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


variables_indep_paso3 <- list(
  "SeniorCitizen",
  "tenure",
  "MultipleLines",
  "InternetService", #antes no
  # "StreamingTV",
  # "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  # "MonthlyCharges",
  "TotalCharges", #antesno
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
# Añado al paso anterior una a una las variables no añadidas en el paso 1  (gender, PhoneService)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

variables_indep_paso4_a <- list(
  "SeniorCitizen",
  "tenure",
  "MultipleLines",
  "InternetService", #antes no
  # "StreamingTV",
  # "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  # "MonthlyCharges",
  "TotalCharges", #antesno
  "FirstMonth",
  "gender"
)

# Construir la fórmula con todas las variables independientes
formula_str4_a <- paste("Churn ~", paste(variables_indep_paso4_a, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso4_a <- glm(as.formula(formula_str4_a), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso4_a)

# GENDER SIGUE SIN SER SIGNIFICATIVA



variables_indep_paso4_b <- list(
  "SeniorCitizen",
  "tenure",
  "MultipleLines",
  "InternetService", #antes no
  # "StreamingTV",
  # "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  # "MonthlyCharges",
  "TotalCharges", #antesno
  "FirstMonth",
  "PhoneService"
)

# Construir la fórmula con todas las variables independientes
formula_str4_b <- paste("Churn ~", paste(variables_indep_paso4_b, collapse = " + "))

# Ajustar el modelo logístico con glm
Churn_paso4_b <- glm(as.formula(formula_str4_b), data = dataTrain, family = binomial())

# Resumen del modelo
summary(Churn_paso4_b)


# PhoneService SIGUE SIN SER SIGNIFICATIVA



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(OddsPlotty)

plotty<- OddsPlotty::odds_plot(Churn_paso3,
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



# Dado que el odds ratio (OR) es 1.82 para PaperlessBillingYes, esto significa que los clientes que tienen la
# facturación electrónica (PaperlessBilling = Yes) tienen un 82% más de probabilidades de irse de la compañía
# (ya que la referencia es No irse) en comparación con aquellos que no tienen facturación
# electrónica (PaperlessBilling = No), que es la categoría de referencia.

# Entonces, los clientes con facturación electrónica tienen un riesgo 82% mayor de churn (dejar la compañía)
# que aquellos sin facturación electrónica.


# Como vemos, lo más crítico es cuando los clientes se encuentran en el primer mes, ya que tienen hasta
# un 163% más de riesgo que aquellos que han pasado el primer mes de contrato, de dejar la compañía.

# Por el contrario, aquellos que tienen un contrato de un año y dos con respecto a los que lo tienen mes a mes,
# tienen un 59% y 86% menos de riesgo de irse de la compañía.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #






# OJO NO LIARSE, PIENSO QUE DEBO USAR EL CHURN_paso3

logistic_pred_Churn3 <- predict(Churn_paso3, newdata = dataTest, type = "response")

# Convertir probabilidades a clase (0 o 1)
logistic_class <- ifelse(logistic_pred_Churn3 > 0.5, "Yes", "No")

# Evaluar el modelo: Matriz de confusión
confusionMatrix(as.factor(logistic_class), dataTest$Churn)




# Curva ROC y AUC del modelo final de regresion logistica
library(pROC)
roc_logistic <- roc(dataTest$Churn, logistic_pred_Churn3)
auc(roc_logistic)

# Graficar la curva ROC
plot(roc_logistic, col = "blue", main = "Curva ROC - Regresión Logística")

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

# Predicciones de clase (no probabilidades)
tree_predictions_classes_PRUEBA <- predict(arbol_cv, newdata = dataTest)

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(tree_predictions_classes_PRUEBA, dataTest$Churn)

# Imprimir la matriz de confusión
print(confusion_matrix)



# Predicciones
tree_predictions_10K <- predict(arbol_cv, newdata = dataTest, type = "prob")

# Calcular y plotear la curva ROC
roc_curve_final <- roc(dataTest$Churn, tree_predictions_10K[,2])
plot(roc_curve_final)
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)








grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.01))
# grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.02))
# grid <- expand.grid(cp = seq(0.01, 0.11, by = 0.03))
# grid <- expand.grid(cp = seq(0.01, 0.2, by = 0.03))

arbol_tuned <- train(Churn ~ ., data = dataTrain, method = "rpart", trControl = control, tuneGrid = grid)

# Ver el mejor parámetro de cp
print(arbol_tuned$bestTune)

# Graficar el árbol con etiquetas y colores personalizados
rpart.plot(
  arbol_tuned$finalModel,
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
tree_predictions_HIPER <- predict(arbol_tuned, newdata = dataTest, type = "prob")

# Calcular y plotear la curva ROC
roc_curve_final <- roc(dataTest$Churn, tree_predictions_HIPER[,2])
plot(roc_curve_final)
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "brown", cex = 1.5)



# Predicciones de clase (no probabilidades)
tree_predictions_classes_PRUEBA_HIP <- predict(arbol_tuned, newdata = dataTest)

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(tree_predictions_classes_PRUEBA_HIP, dataTest$Churn)

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
plot(randomForest(Churn ~ ., data = dataTrain, keep.forest=TRUE, ntree=500), log="y")

# # Predicciones
rf_predictions <- predict(rf_model, newdata = dataTest)
# 
# # Evaluación
confusionMatrix(rf_predictions, dataTest$Churn)

# Importancia de las características
importance(rf_model)
varImpPlot(rf_model, main = "Random Forest - Train/Test")

# MEAN DECREASE ACCURACY:
# El grafico muestra el impacto en la precisión, se pueden apreciar las variables más importantes, 
# como tenure, TotalCharges, Contract y MonthlyCharges, tienen un alto valor de disminución media 
# en la precisión. 
# Es decir, estas variables son cruciales para predecir si un cliente abandonará o no.
# Ya en menor medida tendríamos internetservice, techsupport, onlinesecurity, firstmonth y paperlessbilling.
# Y variables como gender, phoneService, dependents...no sirven para predecir
# MEAN DECREASE GINI:
# El grafico muestra que variables como TotalCharges, MonthlyCharges, y tenure tienen un alto 
# impacto en el índice Gini, lo que sugiere que contribuyen a crear nodos más puros y, 
# por tanto, a una mejor clasificación.
# En algo de menor medida contract y paymentmethod.



# Por tanto, las variables TotalCharges, tenure, MonthlyCharges, y Contract 
# son las más importantes tanto en la precisión del modelo como en la pureza de las divisiones,
# y por ende, son claves para la predicción del churn en este conjunto de datos.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 
# 



rf_predictions_prob <- predict(rf_model, newdata = dataTest, type = "prob")


roc_curve_rf <- roc(dataTest$Churn, rf_predictions_prob[,2])
plot(roc_curve_rf, main= "Random Forest - Train/Test")
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)








# Control para 10-fold cross-validation
control_cv <- trainControl(method = "cv", number = 10)

# Entrenar el modelo usando validación cruzada
set.seed(123)  # Fijar semilla para reproducibilidad
rf_cv_model <- train(Churn ~ ., data = dataTrain, method = "rf", trControl = control_cv)
# plot(train(Churn ~ ., data = dataTrain, method = "rf", trControl = control_cv, ntree=500), log="y")

# Predicciones en el conjunto de prueba
rf_cv_predictions <- predict(rf_cv_model, newdata = dataTest)

# Matriz de confusión
confusion_matrix_rf_cv <- confusionMatrix(rf_cv_predictions, dataTest$Churn)
print(confusion_matrix_rf_cv)


# Predicciones de clase (no probabilidades)
tree_predictions_classes_PRUEBA_HIP <- predict(arbol_tuned, newdata = dataTest)

# Calcular la matriz de confusión
confusion_matrix <- confusionMatrix(tree_predictions_classes_PRUEBA_HIP, dataTest$Churn)

# Imprimir la matriz de confusión
print(confusion_matrix)


# Importancia de las características
importance(rf_cv_model)
varImpPlot(rf_cv_model$finalModel, main = "Random Forest - 10K-Folds")



rf_cv_predictions_prob_10k <- predict(rf_cv_model, newdata = dataTest, type = "prob")

# Calcular y graficar la curva ROC
roc_curve_rf <- roc(dataTest$Churn, rf_cv_predictions_prob_10k[,2])
plot(roc_curve_rf,main = "Random Forest - 10K-Folds")
texto <- auc(roc_curve_final)
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)







# Búsqueda de Hiperparámetros (Hyperparameter Tuning):
# Para ajustar los hiperparámetros de los modelos de machine learning, se puede 
# emplear Grid Search o Bayesian Search. El paquete caret facilita la implementación de Grid Search.
# Grid Search usando caret:

# Definir el grid de hiperparámetros
grid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6, 7 ,8, 9, 10))

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
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(texto, 4)), col = "darkolivegreen3", cex = 1.5)


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

