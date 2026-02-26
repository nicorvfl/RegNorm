# ----------------------------------------
#            FULL MODEL
# ----------------------------------------
# Este sería el full full modelo. 
Full <- lm(
  ravlt_learning ~
    ageC + ageC2 + ageC3 + age_log_100 +
    sex + years_education +
    ageC:years_education +
    ageC:sex +
    years_education:sex,
  data = df2)
summary(Full)

print("Acá hacemos el stepwise backward")
ModeloStep <- step(
  Full,
  direction = "backward")
print("Le pedimos los valores")
summary(ModeloStep)

# La fórmula del modelo es:
# y ~ ageC + sex + years_education

# En función de la cantidad de predictores,
# calculamos Bonferroni.
# PASO 1
modelofinal <- lm(ravlt_learning ~ ageC + sex + years_education,
                  data = df2)
summary(modelofinal)
confint(modelofinal, level = 0.95)

# ----------------------------------------
#       DIAGNÓSTICO Y PREDICTORES
# ----------------------------------------

predictores_diagnostico <- Stage.1(Dataset = df2,
                                   Model = ravlt_learning ~ ageC +
                                     sex + years_education)
summary(predictores_diagnostico)

par(mfrow = c(2, 2))  
plot(modelofinal)
par(mfrow = c(1, 1)) 

# ----------------------------------------
#        COMPARACIÓN DE MODELOS
# ----------------------------------------
library(flexplot)
reduced <- lm(ravlt_learning ~ 1, data = df2)
full    <- modelofinal
model.comparison(full, reduced)

# ----------------------------------------
#           CROSS-VALIDATION
# ----------------------------------------
library(caret)
set.seed(1996) 
ctrl_cv <- trainControl(
  method = "cv",
  number = 10)

set.seed(1996)
cv_step <- train(
  ravlt_learning ~ ageC + sex + years_education,
  data = df2,
  method = "lm",
  trControl = ctrl_cv)

#Métricas por separado
cv_step
cv_step$results$Rsquared
cv_step$results$RMSE
cv_step$results$MAE

# ----------------------------------------------------------
#                 Asimetría y curtosis
# ----------------------------------------------------------

library(moments)
residuos <- residuals(modelofinal)
skewness(residuos, na.rm = TRUE)
kurtosis(residuos, na.rm = TRUE)

# ----------------------------------------------------------
#                       Calculator
# ----------------------------------------------------------

summary(modelofinal)$sigma


Stage.2.AutoScore(Stage.1.Model=predictores_diagnostico,
                  Assume.Normality = FALSE,
                  Folder="C:/Users/Nico/Desktop/Fleni/Proyectos/RegNorm/", 
                  NameFile="RAVLT_Learning_PREDICCION.xlsx")



#  PERCENTILES
percentile_table_from_model <- function(model, test_name,
                                        probs = c(.01,.02,.05,.10,.15,.25,.50,.75,.85,.90,.95,.98,.99),
                                        include_delta = TRUE,
                                        quantile_type = 8) {
  r <- residuals(model)
  
  
  out <- data.frame(
    Test = test_name,
    Percentile = probs * 100,
    Residual = as.numeric(quantile(r, probs = probs, na.rm = TRUE, type = quantile_type))
  )
  
  
  out
}

tabla_1test <- percentile_table_from_model(modelofinal, test_name = "RAVLT L")
tabla_1test