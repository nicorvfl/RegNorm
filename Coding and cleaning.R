# Librerías
library(ggplot2)
library(NormData)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(corrplot)

#me armo una función para filtrar
limpiar_base <- function(df) {
  
  #convierto a numerico
  VariablesNum <- c("moca", "ravlt_learning", "ravlt_delayed", "bnt", "dig_dir", 
                           "dig_inv", "tmt_a", "tmt_b", "faq", "flu_fon", "flu_sem", 
                           "fab", "craft_ri", "craft_rd", "craft_rd_z", 
                           "craft_ri_z", "rey_copia")
  
  df %>%
    mutate(
      sex = as.factor(Sexo),
      age = as.numeric(age),
      ageC = age - mean(age, na.rm = TRUE),
      ageC2 = ageC^2,
      ageC3 = ageC^3,
      years_education = as.numeric(str_remove(years_education, "\\D")),
      education_level = case_when(
        education_level %in% c("hasta 7 (inclusive)", "8 a 12") ~ "Low",
        education_level %in% c("13 a 15", "16") ~ "Medium",
        TRUE ~ "High"
      ),
      education_level = factor(education_level, levels = c("Low", "Medium", "High"))
    ) %>%
    mutate(across(all_of(VariablesNum), ~ as.numeric(.))) %>%
    filter(
      moca > 25,
      craft_ri_z > -1,
      craft_rd_z > -1,
      years_education < 25
    ) %>%
    filter(row_number() != 42) %>%
    select(
      sex, age, moca, education_level, ravlt_learning, ravlt_delayed,
      dig_inv, dig_dir, tmta = tmt_a, tmtb = tmt_b, fab, flu_fon, flu_sem,
      craft_ri, craft_rd, craft_ri_z, ageC, ageC2, ageC3,
      craft_rd_z, years_education
    ) %>%
    drop_na()
}

#función pa' limpiar de forma multivariada
aplicar_mahalanobis <- function(df, vars, nombre_outlier, cutoff = qchisq(0.99, df = 2)) {
  subdata <- df %>% select(all_of(vars))
  d2 <- mahalanobis(subdata, center = colMeans(subdata), cov = cov(subdata))
  df[[paste0("maha_", nombre_outlier)]] <- d2
  df[[paste0("outlier_", nombre_outlier)]] <- d2 > cutoff
  cat("Outliers en", nombre_outlier, ":", sum(df[[paste0("outlier_", nombre_outlier)]]), "\n")
  return(df)
}

#vamos con pipeline
df_v1 <- read.csv("C:/Users/Nico/Desktop/Fleni/Proyectos/RegNorm/base-regnorm.csv")
df <- limpiar_base(df_v1)

#miro el correlograma para armarme mahalanobis
cognitivas <- df %>% select(ravlt_learning, ravlt_delayed, flu_fon, flu_sem, dig_dir, dig_inv, tmta, tmtb)
cor_matrix <- cor(cognitivas, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_matrix, 2))
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

#mahalanobis
df <- aplicar_mahalanobis(df, c("ravlt_learning", "ravlt_delayed"), "memoria")
df <- aplicar_mahalanobis(df, c("flu_fon", "flu_sem"), "lenguaje")
df <- aplicar_mahalanobis(df, c("dig_dir", "dig_inv"), "atencion")
df <- aplicar_mahalanobis(df, c("tmta", "tmtb"), "ejecutiva")

#a lo brusco, los vuelo a todos
df <- df %>%
  filter(!outlier_memoria, !outlier_lenguaje, !outlier_atencion, !outlier_ejecutiva)
#me quedo con 1458


