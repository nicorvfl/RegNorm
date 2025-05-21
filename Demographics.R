install.packages("flextable")
library(knitr)
library(flextable)
library(gt)

#Función para calcular demográficos
Mean_Sex <- function(df) {
  results <- df %>%
    group_by(sex) %>% 
    
    summarise(
      MediaAge = mean(age, na.rm = TRUE),
      DesviacionAge = sd(age, na.rm = TRUE),
      PorcentajeSex = 100 * n() / nrow(df),
      CantidadSex = n(),
      MediaEd = mean(years_education, na.rm = TRUE),
      DesviacionEd = sd(years_education, na.rm = TRUE)
    ) %>%
    
    ungroup() 
  
  return(kable(results, 
               digits = 3,
               format = "markdown"))
}

Mean_Sex(df)

MeanEduc <- function(df) {
  df %>%
    group_by(sex, education_level) %>%
    summarise(
      Numero = n(),
      PorcentajeSex = 100 * n() / nrow(df),
      .groups = "drop"
    )
}

MeanEduc(df)
