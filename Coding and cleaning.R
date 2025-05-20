#Importo librerías
library(ggplot2)
library(NormData)
library(dplyr)
library(tidyverse)
library(gridExtra)

df_v1 <- read.csv("C:/Users/Nico/Desktop/Fleni/Proyectos/RegNorm/base-regnorm.csv")

#Limpieza de acuerdo a criterios de inclusión
df <- df_v1 %>%
  mutate(
    sex = as.factor(Sexo),
    age = as.numeric(age),
    ageC = age - mean(age, na.rm = TRUE),
    ageC2 = (age - mean(age, na.rm = TRUE))^2,  
    ageC3 = (age - mean(age, na.rm = TRUE))^3,
    moca = as.numeric(moca),
    years_education = as.numeric(str_remove(years_education, "\\D")),
    education_level = ifelse(education_level == "hasta 7 (inclusive)" | 
                         education_level == "8 a 12", "Low",
                       ifelse(education_level == "13 a 15" |
                                education_level == "16", "Medium", "High")),
    education_level = factor(education_level, levels = c("Low", "Medium", "High")),
    ravlt_learning = as.numeric(ravlt_learning),
    ravlt_delayed = as.numeric(ravlt_delayed),
    bnt = as.numeric(bnt),
    dig_dir = as.numeric(dig_dir),
    dig_inv = as.numeric(dig_inv),
    tmta = as.numeric(tmt_a),
    tmtb = as.numeric(tmt_b),
    faq = as.numeric(faq),
    flu_fon = as.numeric(flu_fon),
    flu_sem = as.numeric(flu_sem),
    fab = as.numeric(fab),
    craft_ri = as.numeric(craft_ri),
    craft_rd = as.numeric(craft_rd),
    craft_rd_z = as.numeric(craft_rd_z),
    craft_ri_z = as.numeric(craft_ri_z),
    copia_rey = as.numeric(rey_copia)
  ) %>%
  filter(
    moca > 25,
    craft_ri_z > -1,
    craft_rd_z > -1,
    years_education < 25) %>% #no recuerdo por qué lo de educación
  filter(row_number() != 42) %>%
  select(
    sex, age, moca, education_level, ravlt_learning, ravlt_delayed,
    dig_inv, dig_dir, tmta, tmtb, fab, flu_fon, flu_sem,
    craft_ri, craft_rd, craft_ri_z, ageC, ageC2, ageC3,
    craft_rd_z, years_education
  ) %>%
  drop_na()

#Cálculo de Mahalanobis
calcular_mahalanobis <- function(data) {
  mu <- colMeans(data)
  S <- cov(data)
  d2 <- mahalanobis(data, center = mu, cov = S)
  return(d2)
  }

#Memoria
MemoriaMahalanobis <- df %>% select(ravlt_learning, ravlt_delayed)
df$maha_memoria <- calcular_mahalanobis(MemoriaMahalanobis)

#Lenguaje
LenguajeMahalanobis <- df %>% select(flu_fon, flu_sem)
df$maha_lenguaje <- calcular_mahalanobis(LenguajeMahalanobis)

#Atención
AtencionMahalanobis <- df %>% select(dig_dir, dig_inv)
df$maha_atencion <- calcular_mahalanobis(AtencionMahalanobis)

#Velocidad de procesamiento
VelocidadMahalanobis <- df %>% select(tmta, tmtb)
df$maha_ejecutiva <- calcular_mahalanobis(VelocidadMahalanobis)


#Selecciono
cutoff <- qchisq(0.99, df = 2)

#Van flags
df <- df %>%
  mutate(
    outlier_memoria = maha_memoria > cutoff,
    outlier_lenguaje = maha_lenguaje > cutoff,
    outlier_atencion = maha_atencion > cutoff,
    outlier_ejecutiva = maha_ejecutiva > cutoff)

df <- df %>%
  filter(
    outlier_memoria == "FALSE",
    outlier_lenguaje == "FALSE",
    outlier_atencion == "FALSE",
    outlier_ejecutiva == "FALSE")

nrow(df) #1458



