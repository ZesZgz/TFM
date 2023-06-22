library(tidyverse)
library(haven)
library(stargazer)
library(margins)
library(plm)
library(car)
library(MASS)
library(effects)
library(ggtext)
library(ggthemes)


ESS <- read_csv("ESS.csv") %>% rename(clase8_r = class8_r)


full_model_1 <- glm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase_capitalismo + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, family = "binomial", weights = peso)
full_model_2 <- glm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase8_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, family = "binomial", weights = peso)
full_model_3 <- glm(DRP ~ sexo + edad + habitat + educacion + confianza_social + clase16_r + ansiedad_material +  eje_economico  + eje_cultural, data = ESS, family = "binomial", weights = peso)


a <- as.data.frame(car::vif(full_model_2))


print(chisq.test(table(ESS$educacion, ESS$clase8_r)))
print(chisq.test(table(ESS$educacion, ESS$clase16_r)))


ESS %>%
  dplyr::select(eje_economico, ansiedad_material) %>%
  ggplot(aes(x=eje_economico, y=ansiedad_material))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_minimal()

cor.test(ESS$eje_economico, ESS$ansiedad_material)

ansiedad_y_economia <- lm(ansiedad_material ~ eje_economico, data = ESS)





ESS %>%
  dplyr::select(eje_economico, eje_cultural) %>%
  ggplot(aes(x=eje_economico, y=eje_cultural))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  theme_minimal()+
  theme(plot.title=element_markdown())+
  labs(title="<b>Correlación entre el eje cultural y el eje económico</b>")+
  scale_y_continuous(name="Eje cultural", limits=c(0,1))+
  scale_x_continuous(name="Eje económico", limits=c(0,1))


ESS %>%
  dplyr::select(eje_economico, ansiedad_material) %>%
  ggplot(aes(x=eje_economico, y=ansiedad_material))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  theme_minimal()+
  theme(plot.title=element_markdown())+
  labs(title="<b>Correlación entre la ansiedad material y el eje económico</b>")+
  scale_y_continuous(name="Ansiedad material", limits=c(0,1))+
  scale_x_continuous(name="Eje económico", limits=c(0,1))
  
  

cor.test(ESS$eje_economico, ESS$eje_cultural)


ESS %>%
  dplyr::select(confianza_social, eje_cultural) %>%
  ggplot(aes(x=confianza_social, y=eje_cultural))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  theme_minimal()+
  theme(plot.title=element_markdown())+
  labs(title="<b>Correlación entre el eje cultural y el confianza social</b>")+
  scale_y_continuous(name="Eje cultural", limits=c(0,1))+
  scale_x_continuous(name="Confianza social", limits=c(0,1))

cor.test(ESS$confianza_social, ESS$eje_cultural)

t.test(eje_cultural ~ clase8_r, data = ESS)
summary(aov(eje_cultural ~ clase8_r*educacion, data = ESS))

summary(full_model_2)


marginales <- effect(full_model_2)



