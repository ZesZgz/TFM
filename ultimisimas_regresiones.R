library(tidyverse)
library(haven)
library(stargazer)
library(margins)
library(plm)
library(car)

ESS <- read_csv("ESS.csv") %>% rename(clase8_r = class8_r)

M1 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M2 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M3 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M4 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M5 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M6 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M7 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M8 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M9 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M10 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + ansiedad_material, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M11 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M12 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M13 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + ansiedad_material + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M14 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M15 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material + eje_economico, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M16 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + ansiedad_material + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M17 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
M18 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

M19 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material *  eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)


full_model_1 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + ansiedad_material + eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
full_model_2 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
full_model_3 <- plm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)

full_model_IZQ <- plm(IZQ ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)
full_model_DT <- plm(DT ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, index = c("pais"), model = "within", effect = "individual", family = "binomial", weights = peso)


full_model <- list(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, full_model_1, full_model_2, full_model_3, full_model_IZQ, full_model_DT)

stargazer(full_model, type="text", out="Regresiones_finales.html")









library(ggplot2)

# Obtener los coeficientes y los intervalos de confianza del modelo
coeficientes <- coef(full_model_2)
intervalos_confianza <- confint(full_model_2)

# Crear un data frame con los coeficientes, los límites inferiores y superiores de los intervalos de confianza
df_coeficientes <- data.frame(variable = names(coeficientes),
                              coeficiente = coeficientes,
                              ci_lower = intervalos_confianza[, 1],
                              ci_upper = intervalos_confianza[, 2])

# Añadir una columna al data frame indicando si el coeficiente es significativo
df_coeficientes$significativo <- ifelse(df_coeficientes$ci_lower > 0 | df_coeficientes$ci_upper < 0, "Significativo", "No significativo")

# Crear el gráfico de puntos con barras de error y colores según significancia
df_coeficientes %>%
  ggplot(aes(x = variable, y = coeficiente)) +
  geom_point(color = ifelse(df_coeficientes$significativo == "Significativo", "purple", "black")) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.7, color = ifelse(df_coeficientes$significativo == "Significativo", "purple", "black")) +
  xlab("Variable") +
  ylab("Coeficiente") +
  ggtitle("Coeficientes del modelo con intervalos de confianza") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Coeficiente", breaks = seq(-1, 1, 0.1), limits = c(-0.2, 0.4)) +
  scale_x_discrete(name="", labels = rev(c("Mujer", "Urbe", "Rural", "Periferia", "Eje económico", "Eje cultural", "Educación universitaria", "Educación secundaria superior", "Educación secundaria básica", "Educación primaria", "Edad", "Confianza social", "Profesional técnico", "Trabajador sociocultural", "Pequeño propietario", "Trabajador de servicios", "Autónomo y grandes empleadores", "Obreros industriales", "Clerks", "Ansiedad material")))



