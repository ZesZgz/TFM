library(tidyverse) # Para la sintaxis
library(haven) # Para abrir archivos
library(caret)
library(scales)
library(dplyr)
library(questionr)
library(labelled)
library(essurvey)

#### CARGAMOS LAS BASES DE DATOS ###############################################

ESS4 <-  read_sav("row_data/ESS4.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Austria=prtvtaat, recuerdo_Belgica=prtvtbbe, recuerdo_Suiza=prtvtbch, recuerdo_Dinamarca=prtvtbdk, recuerdo_Finlandia=prtvtafi, recuerdo_Francia=prtvtbfr, recuerdo_NL=prtvtcnl, recuerdo_Noruega=prtvtno, recuerdo_Suecia=prtvtse,
                economia=gincdif, empleo_todos=gvjbevn, multiculturalismo=imueclt, LGTB=freehms,confianza_social=ppltrst,
                iscoco, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, pedir_dinero=brwmny, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=edulvla,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca"))) %>%
  mutate(recuerdo_Austria=as.character(as_factor(recuerdo_Austria, levels = "labels"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Dinamarca=as.character(as_factor(recuerdo_Dinamarca, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Austria),   recuerdo_Austria,
                           ifelse(!is.na(  recuerdo_Belgica),   recuerdo_Belgica, 
                                  ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                                         ifelse(!is.na(  recuerdo_Dinamarca),   recuerdo_Dinamarca,
                                                ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                                       ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                                              ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                                     ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega,  recuerdo_Suecia))))))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(empleo_todos = 1-(as.numeric(empleo_todos)/10)) %>%
  mutate(multiculturalismo = 1-(as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia, empleo_todos), na.rm = TRUE)) %>%
  mutate(oleada = 4) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Formación profesional",
                               educacion == 5 ~ "Superiores",
                               TRUE ~ NA))

ESS5 <-  read_sav("row_data/ESS5.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Belgica=prtvtcbe, recuerdo_Suiza=prtvtcch, recuerdo_Dinamarca=prtvtbdk, recuerdo_Finlandia=prtvtbfi, recuerdo_Francia=prtvtbfr, recuerdo_NL=prtvtdnl, recuerdo_Noruega=prtvtano, recuerdo_Suecia=prtvtase,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms,confianza_social=ppltrst,
                iscoco, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, pedir_dinero=brwmny, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=eisced,
                dweight,pspwght,pweight) %>%
  filter(pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Dinamarca=as.character(as_factor(recuerdo_Dinamarca, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo = coalesce(recuerdo_Belgica, recuerdo_Suiza, recuerdo_Dinamarca, recuerdo_Finlandia, recuerdo_Francia, recuerdo_NL, recuerdo_Noruega, recuerdo_Suecia)) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, tradicionalismo,  LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 5) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))

ESS6 <-  read_sav("row_data/ESS6.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Belgica=prtvtcbe, recuerdo_Suiza=prtvtdch, recuerdo_Dinamarca=prtvtcdk, recuerdo_Finlandia=prtvtcfi, recuerdo_Francia=prtvtcfr, recuerdo_NL=prtvtenl, recuerdo_Noruega=prtvtano, recuerdo_Suecia=prtvtbse,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, europeismo=euftf,confianza_social=ppltrst,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=eisced,
                habitat=domicil, fuente_ingresos=hincsrca,
                isco08, emplrel, emplno,
                dweight,pspwght,pweight) %>%
  filter(pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Dinamarca=as.character(as_factor(recuerdo_Dinamarca, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Belgica),   recuerdo_Belgica, 
                           ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                                  ifelse(!is.na(  recuerdo_Dinamarca),   recuerdo_Dinamarca,
                                         ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                                ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                                       ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                              ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega,  recuerdo_Suecia)))))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(europeismo = (as.numeric(europeismo)/10)) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., europeismo, multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 6)%>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))

ESS7 <-  read_sav("row_data/ESS7.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Austria=prtvtbat, recuerdo_Belgica=prtvtcbe, recuerdo_Suiza=prtvtech, recuerdo_Dinamarca=prtvtcdk, recuerdo_Finlandia=prtvtcfi, recuerdo_Francia=prtvtcfr, recuerdo_NL=prtvtfnl, recuerdo_Noruega=prtvtbno, recuerdo_Suecia=prtvtbse,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, europeismo=euftf,confianza_social=ppltrst,
                isco08, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=eisced,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca"))) %>%
  mutate(recuerdo_Austria=as.character(as_factor(recuerdo_Austria, levels = "labels"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Dinamarca=as.character(as_factor(recuerdo_Dinamarca, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Austria),   recuerdo_Austria,
                           ifelse(!is.na(  recuerdo_Belgica),   recuerdo_Belgica, 
                                  ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                                         ifelse(!is.na(  recuerdo_Dinamarca),   recuerdo_Dinamarca,
                                                ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                                       ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                                              ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                                     ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega,  recuerdo_Suecia))))))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(europeismo = (as.numeric(europeismo)/10)) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., europeismo, multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 7)%>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))

ESS8 <-  read_sav("row_data/ESS8.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Austria=prtvtbat, recuerdo_Belgica=prtvtcbe, recuerdo_Suiza=prtvtfch, recuerdo_Finlandia=prtvtdfi, recuerdo_Francia=prtvtcfr, recuerdo_NL=prtvtfnl, recuerdo_Noruega=prtvtbno, recuerdo_Suecia=prtvtbse, recuerdo_Italia=prtvtbit, recuerdo_Reino_Unido=prtvtbgb, recuerdo_Alemania_Occ=prtvede1, recuerdo_Alemania_Ori=prtvede2,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, confianza_social=ppltrst,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=eisced,
                habitat=domicil, fuente_ingresos=hincsrca,
                isco08, emplrel, emplno,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH" | pais=="IT" | pais=="GB" | pais=="DE") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca", "IT"="Italia", "GB"="Reino Unido", "DE"="Alemania"))) %>%
  mutate(recuerdo_Austria=as.character(as_factor(recuerdo_Austria, levels = "labels"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo_Italia=as.character(as_factor(recuerdo_Italia, levels = "labels"))) %>%
  mutate(recuerdo_Alemania_Occ=as.character(as_factor(recuerdo_Alemania_Occ, levels = "labels"))) %>%
  mutate(recuerdo_Alemania_Ori=as.character(as_factor(recuerdo_Alemania_Ori, levels = "labels"))) %>%
  mutate(recuerdo_Reino_Unido=as.character(as_factor(recuerdo_Reino_Unido, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Austria),   recuerdo_Austria,
                           ifelse(!is.na(  recuerdo_Belgica),   recuerdo_Belgica, 
                                  ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                                         ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                                ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                                       ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                              ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega,  
                                                                     ifelse(!is.na(recuerdo_Suecia), recuerdo_Suecia,
                                                                            ifelse(!is.na(recuerdo_Italia), recuerdo_Italia,
                                                                                   ifelse(!is.na(recuerdo_Alemania_Occ), recuerdo_Alemania_Occ,
                                                                                          ifelse(!is.na(recuerdo_Alemania_Ori), recuerdo_Alemania_Ori, recuerdo_Reino_Unido)))))))))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 8) %>%
  mutate(sexo = as.numeric(sexo)) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))



### CARGAMOS LA ESS9 ###########################################################

ESS9 <-  read_sav("row_data/ESS9.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Austria=prtvtcat, recuerdo_Belgica=prtvtdbe, recuerdo_Dinamarca=prtvtddk, recuerdo_Suiza=prtvtgch, recuerdo_Finlandia=prtvtdfi, recuerdo_Francia=prtvtdfr, recuerdo_NL=prtvtgnl, recuerdo_Noruega=prtvtbno, recuerdo_Suecia=prtvtcse, recuerdo_Italia=prtvtcit, recuerdo_Espana=prtvtees, recuerdo_Alemania_Occ=prtvede1, recuerdo_Alemania_Ori=prtvede2,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, confianza_social=ppltrst,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, tradicionalismo=imptrad, educacion=eisced,
                isco08, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH" | pais=="IT" | pais=="GB" | pais=="DE" | pais=="ES") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca", "IT"="Italia", "ES"="España", "DE"="Alemania"))) %>%
  mutate(recuerdo_Austria=as.character(as_factor(recuerdo_Austria, levels = "labels"))) %>%
  mutate(recuerdo_Belgica=as.character(as_factor(recuerdo_Belgica, levels = "labels"))) %>%
  mutate(recuerdo_Dinamarca=as.character(as_factor(recuerdo_Dinamarca, levels = "labels"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo_Italia=as.character(as_factor(recuerdo_Italia, levels = "labels"))) %>%
  mutate(recuerdo_Alemania_Occ=as.character(as_factor(recuerdo_Alemania_Occ, levels = "labels"))) %>%
  mutate(recuerdo_Alemania_Ori=as.character(as_factor(recuerdo_Alemania_Ori, levels = "labels"))) %>%
  mutate(recuerdo_Espana=as.character(as_factor(recuerdo_Espana, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Austria),   recuerdo_Austria,
                           ifelse(!is.na(  recuerdo_Belgica),   recuerdo_Belgica, 
                                  ifelse(!is.na(recuerdo_Dinamarca), recuerdo_Dinamarca, 
                                         ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                                                ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                                       ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                                              ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                                     ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega,  
                                                                            ifelse(!is.na(recuerdo_Suecia), recuerdo_Suecia,
                                                                                   ifelse(!is.na(recuerdo_Italia), recuerdo_Italia,
                                                                                          ifelse(!is.na(recuerdo_Alemania_Occ), recuerdo_Alemania_Occ,
                                                                                                 ifelse(!is.na(recuerdo_Alemania_Ori), recuerdo_Alemania_Ori, recuerdo_Espana))))))))))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 9) %>%
  mutate(sexo = as.numeric(sexo)) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))



### CARGAMOS LA ESS10 ##########################################################

ESS10 <-  read_sav("row_data/ESS10.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,sexo=gndr,edad=agea,
                recuerdo_Suiza=prtvthch, recuerdo_Finlandia=prtvtefi, recuerdo_Francia=prtvtefr, recuerdo_NL=prtvthnl, recuerdo_Noruega=prtvtbno, recuerdo_Italia=prtvtdit,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, confianza_social=ppltrst,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, educacion=eisced, tradicionalismo=imptrad,
                isco08, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH" | pais=="IT" | pais=="DE") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca", "IT"="Italia", "ES"="Espana", "DE"="Alemania"))) %>%
  mutate(recuerdo_Suiza=as.character(as_factor(recuerdo_Suiza, levels = "labels"))) %>%
  mutate(recuerdo_Finlandia=as.character(as_factor(recuerdo_Finlandia, levels = "labels"))) %>%
  mutate(recuerdo_Francia=as.character(as_factor(recuerdo_Francia, levels = "labels"))) %>%
  mutate(recuerdo_NL=as.character(as_factor(recuerdo_NL, levels = "labels"))) %>%
  mutate(recuerdo_Noruega=as.character(as_factor(recuerdo_Noruega, levels = "labels"))) %>%
  mutate(recuerdo_Italia=as.character(as_factor(recuerdo_Italia, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Suiza),   recuerdo_Suiza,
                           ifelse(!is.na(  recuerdo_Finlandia),   recuerdo_Finlandia,
                                  ifelse(!is.na(  recuerdo_Francia),   recuerdo_Francia,
                                         ifelse(!is.na(  recuerdo_NL),   recuerdo_NL,
                                                ifelse(!is.na(  recuerdo_Noruega),   recuerdo_Noruega, recuerdo_Italia)))))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(tradicionalismo = scales::rescale(as.numeric(tradicionalismo), to = c(0, 10))) %>%
  mutate(tradicionalismo = 1-tradicionalismo) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, tradicionalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 10) %>%
  mutate(sexo = as.numeric(sexo)) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))


## COMPLETAMOS LA ESS10 ########################################################

ESS10_r <-  read_sav("row_data/ESS10SC.sav") %>%
  dplyr::select(pais=cntry, oleada=essround,  sexo=gndr, edad=agea,
                recuerdo_Austria=prtvtcat, recuerdo_Suecia=prtvtdse, recuerdo_Espana=prtvtfes,
                economia=gincdif, multiculturalismo=imueclt, LGTB=freehms, confianza_social=ppltrst,
                ingresos_subjetivos=hincfel, ingresos=hinctnta, indefinido=wrkctra, horas=wkhct, interes=polintr, ideologia=lrscale, educacion=eisced,
                isco08, emplrel, emplno,
                habitat=domicil, fuente_ingresos=hincsrca,
                dweight,pspwght,pweight) %>%
  filter(pais=="AT" | pais=="BE" | pais=="DK" | pais=="FI" | pais=="FR" | pais=="NL" | pais=="NO" | pais=="SE" | pais=="CH" | pais=="IT" | pais=="ES") %>%
  mutate(pais = as.character(recode_factor(pais, "AT"="Austria", "BE"="Belgica", "FI"="Finlandia", "FR"="Francia", "NL"="Países Bajos", "NO"="Noruega", "SE"="Suecia", "CH"="Suiza", "DK"="Dinamarca", "IT"="Italia", "ES"="España", "DE"="Alemania"))) %>%
  mutate(recuerdo_Austria=as.character(as_factor(recuerdo_Austria, levels = "labels"))) %>%
  mutate(recuerdo_Suecia=as.character(as_factor(recuerdo_Suecia, levels = "labels"))) %>%
  mutate(recuerdo_Espana=as.character(as_factor(recuerdo_Espana, levels = "labels"))) %>%
  mutate(recuerdo = ifelse(!is.na(  recuerdo_Austria),   recuerdo_Austria,
                           ifelse(!is.na(recuerdo_Suecia), recuerdo_Suecia, recuerdo_Espana))) %>% 
  dplyr::select(-starts_with("recuerdo_")) %>%
  mutate(economia = (as.numeric(economia)/10)) %>%
  mutate(multiculturalismo = (1-as.numeric(multiculturalismo)/10)) %>%
  mutate(LGTB = scales::rescale(as.numeric(LGTB), to = c(0, 10))) %>%
  mutate(eje_cultural = rowMeans(dplyr::select(., multiculturalismo, LGTB), na.rm = TRUE)) %>%
  mutate(eje_economico = rowMeans(dplyr::select(., economia), na.rm = TRUE)) %>%
  mutate(oleada = 10) %>%
  mutate(sexo = as.numeric(sexo)) %>%
  mutate(peso = dweight*pspwght*pweight) %>%
  dplyr::select(-dweight,-pspwght,-pweight) %>%
  mutate(educacion = case_when(educacion == 1 ~ "Primaria",
                               educacion == 2 ~ "Secundaria",
                               educacion == 3 ~ "Secundaria superior",
                               educacion == 4 ~ "Secundaria superior",
                               educacion == 5 ~ "Formación profesional",
                               educacion == 6 ~ "Superiores",
                               educacion == 7 ~ "Superiores",
                               TRUE ~ NA))

ESS <- full_join(ESS4, full_join(ESS5, full_join(ESS6, full_join(ESS7, full_join(ESS8, full_join(ESS9, full_join(ESS10, ESS10_r))))))) %>%
  mutate(sexo= case_when(sexo==1 ~ "Hombre", sexo==2 ~ "Mujer", TRUE ~ NA)) %>%
  mutate(polo = dplyr::recode(recuerdo, "Ecolo"="IZQ", "Grüne"="IZQ", "SPÖ"="IZQ", "KPÖ"="IZQ", "LIF"="DT", "NEOS"="DT", "ÖVP"="DT", "FPÖ"="DRP", "BZÖ"="DRP",
                              "Agalev"="IZQ", "Agalev/Groen!"="IZQ", "Groen!"="IZQ", "ECOLO"="IZQ", "PS"="IZQ", "SP"="IZQ", "PTB"="IZQ", "SP.A-Spirit"="IZQ", "SP.A"="IZQ", "PVDA+"="IZQ", "CD&V"="DT", "PRL"="DT", "FDF"="DT", "PRL-FDF"="DT" ,"CVP"="DT", "VIVANT"="DT", "VLD"="DT", "Lijst Dedecker"="DT", "CD&V + N-VA"="DT", "MR"="DT", "VU-ID"="DT", "SP.A. + Vlaams - Progressieven (Spirit)"="IZQ", "Open VLD + Vivant"="DT", "N-VA"="DT", "CDH"="DT", "Open VLD"="DT", "Parti Populaire"="DT", "FRONT NATIONAL"="DRP", "Vlaams Blok"="DRP", "Front National"="DRP",
                              "Det Radikale Venstre"="IZQ", "Enhedslisten"="IZQ", "Socialdemokratiet"="IZQ", "Socialistisk Folkeparti"="IZQ",
                              "Centrum-Demokraterne"="DT", "Det Konservative Folkeparti"="DT", "Kristeligt Folkeparti"="DT", "Venstre"="DT",
                              "Dansk Folkeparti"="DRP", "Fremskridtspartiet"="DRP",
                              "Dansk Folkeparti - Danish peoples party" = "DT", "Det Konservative Folkeparti - Conservative"="DT", "Det Radikale Venstre - Danish Social-Liberal Party"="IZQ","Enhedslisten, De Rød-Grønne - The Red-Green Alliance"="IZQ", "Enhedslisten - Unity List - The Red-Green Alliance"="IZQ", "Kristendemokraterne - Christian democtrats"="DT","Ny Alliance - New alliance"="DT","SF- Socialistisk Folkeparti - the Socialist People's Party"="IZQ","Socialdemokraterne - the Danish social democtrats"="IZQ","Venstre, Danmarks Liberale Parti - Venstre"="DT", "SF Socialistisk Folkeparti - Socialist People's Party"="IZQ", "Liberal Alliance - Liberal Alliance"="DT", "Liberal Alliance - Liberal Alliance"="DT",
                              "Christian Democrats"="DT", "Communist Party"="IZQ", "Green League"="IZQ","Left Alliance"="IZQ", "Liberals, (The liberal party of Finland)"="DT", "Social Democratic Party"="IZQ", "The Centre Party"="DT", "Workers' Party"="IZQ","The National Coalition Party"="DT", "The Swedish People's Party (SPP)"="DT", "True Finns"="DRP",
                              "EELV (Europe Ecologie Les Verts)"="IZQ", "FDG (Front de Gauche)"="IZQ", "LO (Lutte Ouvrière)"="IZQ","Parti Radical de Gauche"="IZQ", "NPA (Nouveau Parti Anti-Capitaliste)"="IZQ", "PS (Parti Socialiste)"="IZQ", "Les Verts"="IZQ", "LO"="IZQ", "LCR"="IZQ", "PC"="IZQ", "PS"="IZQ", "PC (Parti communiste)"="IZQ", "LCR (ligue communiste révolutionnaire)"="IZQ", "PRG (Parti Radical de Gauche)"="IZQ",
                              "MODEM (Mouvement Démocrate)"="DT","MPF (Mouvement pour la France)"="DT", "UMP (Union pour un Mouvement Populaire)"="DT", "CPNT"="DT", "MPF"="DT", "RPF"="DT", "UMP"="DT", "CPNT (Chasse, Pêche, Nature et Traditions)"="DT", "RPF (Rassemblement du Peuple Français)"="DT", "UDF-MoDem (Mouvement Democrate)"="DT",
                              "FN (Front National)"="DRP", "FN"="DRP", "MNR"="DRP", "MNR (Mouvement National Républicain)"="DRP",
                              "Christian Democratic Appeal"="DT", "Christian Union"="DT", "Democrats '66"="IZQ", "Green Left"="IZQ", "Labour Party"="IZQ", "List Pim Fortuyn"="DRP", "People's Party for Freedom and Democracy"="DT", "Socialist Party"="IZQ", "Party for Freedom"="DRP",
                              "Det norske Arbeiderparti"="IZQ", "Fremskrittspartiet"="DRP", "Kristelig Folkeparti"="DT", "Rød Valgallianse"="IZQ", "Venstre"="DT", "Sosialistisk Venstreparti"="IZQ", "Fremskrittspartiet"="DRP", "Arbeiderpartiet"="IZQ", "Rødt"="IZQ", "Miljøpartiet De Grønne"="IZQ",
                              "Centern"="DT", "Folkpartiet liberalerna"="DT", "Kristdemokraterna"="DT", "Moderata samlingspartiet"="DT",
                              "FI (Feministiskt initiativ)"="IZQ", "Miljöpartiet de gröna"="IZQ", "Socialdemokraterna"="IZQ", "Vänsterpartiet"="IZQ",
                              "Sverigedemokraterna"="DRP",
                              "Centre Party"="DT", "Christian Democrats"="DT", "Conservative"="DT", "Liberals"="DT",
                              "Green Party"="IZQ", "Left"="IZQ", "Social Democrats"="IZQ",
                              "Christian Democratic Party"="DT", "Christian Social Party"="DT", "Evangelical People's Party"="DT", "Liberal Party"="DT",  "Radicals"="DT", "Green Liberal Party"="DT",
                              "Freiheits-Partei"="DRP", "Swiss Democrats"="DRP", "Swiss People's Party"="DRP", "Ticino League"="DRP",
                              "Green Party"="IZQ", "Social Democratic Party / Socialist Party"="IZQ", "Swiss Labour Party"="IZQ", "Alternative Left"="IZQ", "FDP. The Liberals"="DT",
                              "Alliance 90/The Greens (Bündnis 90/Die Grünen)"="IZQ", "Alternative for Germany (AFD)"="DRP", "Christian Democratic Union/Christian Social Union (CDU/CSU)"="DT", "Free Democratic Party (FDP)"="DT","National Democratic Party (NPD)"="DRP", "Pirate Party (Piratenpartei)"="IZQ", "Social Democratic Party (SPD)"="IZQ", "The Left (Die Linke)"="IZQ",
                              "Conservative"="DT","Green Party"="IZQ","Labour"="IZQ","Liberal Democrat"="DT", "UK Independence"="DRP",
                              "Forza Italia"="DT", "Fratelli d'Italia"="DRP", "Fratelli d'Italia con Giorgia Meloni"="DRP", "La destra"="DT", "Lega"="DRP", "Lega Nord"="DRP", "Liberi e Uguali"="IZQ", "Liberi e Uguali (LEU)"="IZQ", "Partido Democratico (PD)"="IZQ", "Popolo delle Libertà (PdL)"="DT", "Potere al popolo"="IZQ",
                              "Ciudadanos"="DT", "En Comú Podem"="IZQ", " Más País"="IZQ", "PP"="DT", "PSOE"="IZQ", "Unidas Podemos"="IZQ", "VOX"="DRP"
                              ,                            
                              .default = NA_character_)) %>%
  filter(pais %in% c("Dinamarca", "Noruega", "Suiza", "España", "Italia", "Reino Unido", "Alemania") |
           (pais == "Suecia" & oleada == 1) |
           (pais == "Austria" & oleada %in% c(3, 4, 5, 6, 7)) |
           (pais == "Belgica" & oleada %in% c(2, 3, 4)) |
           (pais == "Finlandia" & oleada %in% c(6, 7)) |
           (pais == "Francia" & oleada %in% c(1, 2, 3, 6, 7)) |
           (pais == "Países Bajos" & oleada %in% c(1, 5))) %>%
  mutate(isco08 = ifelse(!is.na(isco08), isco08, iscoco))



### Creamos las clases sociales ################################################

tail(freq(ESS$isco08, total = T))

ESS$isco_mainjob <- ESS$isco08
ESS$isco_mainjob[is.na(ESS$isco_mainjob)] <- -9
var_label(ESS$isco_mainjob) <- "Current occupation of respondent - isco08 4-digit"

head(freq(ESS$isco_mainjob, total = T))


#### Recode employment status for respondents

freq(ESS$emplrel, total = T)
freq(ESS$emplno, total = T)

ESS$emplrel_r <- ESS$emplrel
ESS$emplrel_r[is.na(ESS$emplrel_r)] <- 9
val_label(ESS$emplrel_r, 9) <- "Missing"
freq(ESS$emplrel_r, total = T)


ESS$emplno_r <- ESS$emplno
ESS$emplno_r[is.na(ESS$emplno_r)] <- 0
ESS$emplno_r[ESS$emplno_r >= 1 & ESS$emplno_r <= 9] <- 1
ESS$emplno_r[ESS$emplno_r >= 10 & ESS$emplno_r <= 66665] <- 2
val_labels(ESS$emplno_r) <- c("0 employees" = 0,
                                  "1-9 employees" = 1,
                                  "10+ employees" = 2)
freq(ESS$emplno_r, total = T)

ESS$selfem_mainjob <- NA
ESS$selfem_mainjob[ESS$emplrel_r == 1 | ESS$emplrel_r == 9] <- 1
ESS$selfem_mainjob[ESS$emplrel_r == 2 & ESS$emplno_r == 0] <- 2
ESS$selfem_mainjob[ESS$emplrel_r == 3] <- 2
ESS$selfem_mainjob[ESS$emplrel_r == 2 & ESS$emplno_r == 1] <- 3
ESS$selfem_mainjob[ESS$emplrel_r == 2 & ESS$emplno_r == 2] <- 4

val_labels(ESS$selfem_mainjob) <- c("Not self-employed" = 1,
                                        "Self-empl without employees" = 2,
                                        "Self-empl with 1-9 employees" = 3,
                                        "Self-empl with 10 or more" = 4)
var_label(ESS$selfem_mainjob) <- "Employment status for respondants"
freq(ESS$selfem_mainjob, total = T)

#################################################
# Create Wright schema for capitalist
#################################################

ESS <- ESS %>%
  mutate(clase_capitalismo = case_when(selfem_mainjob==1 ~ "Clase obrera",
                                       selfem_mainjob==2 ~ "Pequeña burguesía",
                                       selfem_mainjob==3 ~ "Pequeña burguesía",
                                       selfem_mainjob==4 ~ "Clase capitalista",
                                       TRUE ~ NA))

#################################################
# Create Oesch class schema for respondents
#################################################

ESS$class16_r <- -9

# Large employers (1)

ESS$class16_r[ESS$selfem_mainjob == 4] <- 1

# Self-employed professionals (2)

ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2000 & ESS$isco_mainjob <= 2162] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2164 & ESS$isco_mainjob <= 2165] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2200 & ESS$isco_mainjob <= 2212] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob == 2250] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2261 & ESS$isco_mainjob <= 2262] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2300 & ESS$isco_mainjob <= 2330] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2350 & ESS$isco_mainjob <= 2352] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2359 & ESS$isco_mainjob <= 2432] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2500 & ESS$isco_mainjob <= 2619] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob == 2621] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2630 & ESS$isco_mainjob <= 2634] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2636 & ESS$isco_mainjob <= 2640] <- 2
ESS$class16_r[(ESS$selfem_mainjob == 2 | ESS$selfem_mainjob == 3) & ESS$isco_mainjob >= 2642 & ESS$isco_mainjob <= 2643] <- 2

# Small business owners with employees (3)

ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 1000 & ESS$isco_mainjob <= 1439] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2163] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2166] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2220 & ESS$isco_mainjob <= 2240] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2260] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2263 & ESS$isco_mainjob <= 2269] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2340 & ESS$isco_mainjob <= 2342] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2353 & ESS$isco_mainjob <= 2356] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2433 & ESS$isco_mainjob <= 2434] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2620] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2622] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2635] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob == 2641] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 2650 & ESS$isco_mainjob <= 2659] <- 3
ESS$class16_r[ESS$selfem_mainjob == 3 & ESS$isco_mainjob >= 3000 & ESS$isco_mainjob <= 9629] <- 3

# Small business owners without employees (4)

ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 1000 & ESS$isco_mainjob <= 1439] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2163] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2166] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2220 & ESS$isco_mainjob <= 2240] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2260] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2263 & ESS$isco_mainjob <= 2269] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2340 & ESS$isco_mainjob <= 2342] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2353 & ESS$isco_mainjob <= 2356] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2433 & ESS$isco_mainjob <= 2434] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2620] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2622] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2635] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob == 2641] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 2650 & ESS$isco_mainjob <= 2659] <- 4
ESS$class16_r[ESS$selfem_mainjob == 2 & ESS$isco_mainjob >= 3000 & ESS$isco_mainjob <= 9629] <- 4

# Technical experts (5)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2100 &  ESS$isco_mainjob <= 2162] <- 5
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2164 &  ESS$isco_mainjob <= 2165] <- 5
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2500 &  ESS$isco_mainjob <= 2529] <- 5

# Technicians (6)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3100 &  ESS$isco_mainjob <= 3155] <- 6
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3210 &  ESS$isco_mainjob <= 3214] <- 6
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3252] <- 6
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3500 &  ESS$isco_mainjob <= 3522] <- 6

# Skilled manual (7)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 6000 &  ESS$isco_mainjob <= 7549] <- 7
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 8310 &  ESS$isco_mainjob <= 8312] <- 7
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 8330] <- 7
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 8332 &  ESS$isco_mainjob <= 8340] <- 7
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 8342 &  ESS$isco_mainjob <= 8344] <- 7

# Low-skilled manual (8)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 8000 &  ESS$isco_mainjob <= 8300] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 8320 &  ESS$isco_mainjob <= 8321] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 8341] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 8350] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 9200 &  ESS$isco_mainjob <= 9334] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 9600 &  ESS$isco_mainjob <= 9620] <- 8
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 9622 &  ESS$isco_mainjob <= 9629] <- 8

# Higher-grade managers and administrators (9)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 1000 &  ESS$isco_mainjob <= 1300] <- 9
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 1320 &  ESS$isco_mainjob <= 1349] <- 9
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2400 &  ESS$isco_mainjob <= 2432] <- 9
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2610 &  ESS$isco_mainjob <= 2619] <- 9
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2631] <- 9
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 100 &  ESS$isco_mainjob <= 110] <- 9

# Lower-grade managers and administrators (10)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 1310 &  ESS$isco_mainjob <= 1312] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 1400 &  ESS$isco_mainjob <= 1439] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2433 &  ESS$isco_mainjob <= 2434] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3300 &  ESS$isco_mainjob <= 3339] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3343] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3350 &  ESS$isco_mainjob <= 3359] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3411] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5221] <- 10
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 200 &  ESS$isco_mainjob <= 210] <- 10

# Skilled clerks (11)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3340 &  ESS$isco_mainjob <= 3342] <- 11
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3344] <- 11
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 4000 &  ESS$isco_mainjob <= 4131] <- 11
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 4200 &  ESS$isco_mainjob <= 4221] <- 11
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 4224 &  ESS$isco_mainjob <= 4413] <- 11
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 4415 &  ESS$isco_mainjob <= 4419] <- 11

# Unskilled clerks (12)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 4132] <- 12
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 4222] <- 12
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 4223] <- 12
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5230] <- 12
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 9621] <- 12

# Socio-cultural professionals (13)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2200 &  ESS$isco_mainjob <= 2212] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2250] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2261 &  ESS$isco_mainjob <= 2262] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2300 &  ESS$isco_mainjob <= 2330] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2350 &  ESS$isco_mainjob <= 2352] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2359] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2600] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2621] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2630] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2632 &  ESS$isco_mainjob <= 2634] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2636 &  ESS$isco_mainjob <= 2640] <- 13
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2642 &  ESS$isco_mainjob <= 2643] <- 13

# Socio-cultural semi-professionals (14)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2163] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2166] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2220 &  ESS$isco_mainjob <= 2240] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2260] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2263 &  ESS$isco_mainjob <= 2269] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2340 &  ESS$isco_mainjob <= 2342] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2353 &  ESS$isco_mainjob <= 2356] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2620] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2622] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2635] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 2641] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 2650 &  ESS$isco_mainjob <= 2659] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3200] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3220 &  ESS$isco_mainjob <= 3230] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3250] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3253 &  ESS$isco_mainjob <= 3257] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3259] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3400 &  ESS$isco_mainjob <= 3410] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3412 &  ESS$isco_mainjob <= 3413] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3430 &  ESS$isco_mainjob <= 3433] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3435] <- 14
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 4414] <- 14

# Skilled service (15)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3240] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3251] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3258] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 3420 &  ESS$isco_mainjob <= 3423] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 3434] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5000 &  ESS$isco_mainjob <= 5120] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5140 &  ESS$isco_mainjob <= 5142] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5163] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5165] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5200] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5220] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5222 &  ESS$isco_mainjob <= 5223] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5241 &  ESS$isco_mainjob <= 5242] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5300 &  ESS$isco_mainjob <= 5321] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5400 &  ESS$isco_mainjob <= 5413] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5419] <- 15
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 8331] <- 15

# Low-skilled service (16)

ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5130 &  ESS$isco_mainjob <= 5132] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5150 &  ESS$isco_mainjob <= 5162] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5164] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5169] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5210 &  ESS$isco_mainjob <= 5212] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5240] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5243 &  ESS$isco_mainjob <= 5249] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 5322 &  ESS$isco_mainjob <= 5329] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 5414] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob == 8322] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 9100 &  ESS$isco_mainjob <= 9129] <- 16
ESS$class16_r[ESS$selfem_mainjob == 1 & ESS$isco_mainjob >= 9400 &  ESS$isco_mainjob <= 9520] <- 16


ESS$class16_r[ESS$class16_r == -9] <- NA
val_labels(ESS$class16_r) <- c("Large employers" = 1,
                                   "Self-employed professionals" = 2,
                                   "Small business owners with employees" = 3,
                                   "Small business owners without employees" = 4,
                                   "Technical experts" = 5,
                                   "Technicians" = 6,
                                   "Skilled manual" = 7,
                                   "Low-skilled manual" = 8,
                                   "Higher-grade managers and administrators" = 9,
                                   "Lower-grade managers and administrators" = 10,
                                   "Skilled clerks" = 11,
                                   "Unskilled clerks" = 12,
                                   "Socio-cultural professionals" = 13,
                                   "Socio-cultural semi-professionals" = 14,
                                   "Skilled service" = 15,
                                   "Low-skilled service" = 16)
var_label(ESS$class16_r) <- "Respondent's Oesch class position - 16 classes"
freq(ESS$class16_r, total = T)


ESS$class8_r <- NA
ESS$class8_r[ESS$class16_r <= 2] <- 1
ESS$class8_r[ESS$class16_r == 3 | ESS$class16_r == 4] <- 2
ESS$class8_r[ESS$class16_r == 5 | ESS$class16_r == 6] <- 3
ESS$class8_r[ESS$class16_r == 7 | ESS$class16_r == 8] <- 4
ESS$class8_r[ESS$class16_r == 9 | ESS$class16_r == 10] <- 5
ESS$class8_r[ESS$class16_r == 11 | ESS$class16_r == 12] <- 6
ESS$class8_r[ESS$class16_r == 13 | ESS$class16_r == 14] <- 7
ESS$class8_r[ESS$class16_r == 15 | ESS$class16_r == 16] <- 8
val_labels(ESS$class8_r) <- c("Self-employed professionals and large employers" = 1,
                                  "Small business owners" = 2,
                                  "Technical (semi-)professionals" = 3,
                                  "Production workers" = 4,
                                  "(Associate) managers" = 5,
                                  "Clerks" = 6,
                                  "Socio-cultural (semi-)professionals" = 7,
                                  "Service workers" = 8)
var_label(ESS$class8_r) <- "Respondent's Oesch class position - 8 classes"
freq(ESS$class8_r, total = T)


ESS <- ESS %>%
  mutate(class8_r = case_when(class8_r==1 ~ "Self-employed professionals and large employers",
                              class8_r==2 ~ "Small business owners",
                              class8_r==3 ~ "Technical (semi-)professionals",
                              class8_r==4 ~ "Production workers",  
                              class8_r==5 ~ "(Associate) managers",
                              class8_r==6 ~ "Clerks", 
                              class8_r==7 ~ "Socio-cultural (semi-)professionals",
                              class8_r==8 ~ "Service workers",
                              TRUE ~ NA)) %>%
  mutate(DRP = case_when(polo == "DRP" ~ 1, TRUE ~ 0),
         IZQ = case_when(polo == "IZQ" ~ 1, TRUE ~ 0),
         DT = case_when(polo == "DT" ~ 1, TRUE ~ 0)) %>%
  mutate(class16_r = case_when(class16_r==1 ~ "Large employers",
                               class16_r==2 ~ "Self-employed professionals",
                               class16_r==3 ~ "Small business owners with employees",
                               class16_r==4 ~ "Small business owners without employees",
                               class16_r==5 ~ "Technical experts",
                               class16_r==6 ~ "Technicians",
                               class16_r==7 ~ "Skilled manual",
                               class16_r==8 ~ "Low-skilled manual",
                               class16_r==9 ~ "Higher-grade managers and administrators",
                               class16_r==10 ~ "Lower-grade managers and administrators",
                               class16_r==11 ~ "Skilled clerks",
                               class16_r==12 ~ "Unskilled clerk",
                               class16_r==13 ~ "Socio-cultural professionals",
                               class16_r==14 ~ "Socio-cultural semi-professionals",
                               class16_r==15 ~ "Skilled service",
                               class16_r==16 ~ "Low-skilled service",
                               TRUE ~ NA)) %>%
  rename(clase16_r = class16_r) %>%
  mutate(ingresos_subjetivos = case_when(ingresos_subjetivos == 1 ~ 0,
                                         ingresos_subjetivos == 2 ~ 1,
                                         ingresos_subjetivos == 3 ~ 2,
                                         ingresos_subjetivos == 4 ~ 3,
                                         TRUE ~ NA)) %>%
  mutate(pedir_dinero = case_when(pedir_dinero == 1 ~ 0,
                                  pedir_dinero == 2 ~ 0,
                                  pedir_dinero == 3 ~ 0,
                                  pedir_dinero == 4 ~ 0,
                                  pedir_dinero == 5 ~ 3,
                                  pedir_dinero == 6 ~ 3,
                                  pedir_dinero == 7 ~ 0,
                                  pedir_dinero == 8 ~ 0,
                                  TRUE ~ NA)) %>% 
  mutate(ingresos = 11 - as.numeric(ingresos)) %>% 
  mutate(indefinido = case_when(indefinido == 1 ~ 0,
                                indefinido == 2 ~ 1,
                                indefinido == 3 ~ 2,
                                TRUE ~ NA)) %>%
  mutate(ansiedad_material = rowSums(.[c("ingresos_subjetivos", "pedir_dinero", "indefinido", "ingresos")], na.rm = TRUE),
         ansiedad_material = rescale(ansiedad_material, to = c(0, 1))) %>%
  mutate(eje_cultural = rescale(eje_cultural, to=c(0,1)),
         ideologia = rescale(as.numeric(ideologia), to=c(0,1)),
         confianza_social = rescale(as.numeric(confianza_social), to=c(0,1))) %>%
  mutate(interes = case_when(interes==1 ~ "Mucho",
                             interes==2 ~ "Bastante",
                             interes==3 ~ "Poco",
                             interes==4 ~ "Nada",
                             TRUE ~ NA)) %>%
  mutate(pais = paste0(pais, "_", oleada)) %>%
  mutate(habitat = case_when(habitat==1~"Urbe", habitat==2~"Periferia", habitat==3~"Urbe", habitat==4~"Ciudad pequeña", habitat==5~"Rural", habitat==6~"Vivienda aislada", TRUE ~ NA)) %>%
  dplyr::select(-oleada, -ingresos_subjetivos, -pedir_dinero, -ingresos, -indefinido, -fuente_ingresos, -tradicionalismo, -europeismo, -economia, -empleo_todos, -multiculturalismo, -LGTB, -horas, -isco08, -iscoco, -emplrel, -emplno, -isco_mainjob, -emplrel_r, -emplno_r, -selfem_mainjob)

ESS %>%
  na.omit() %>%
  write.csv(file = "ESS.csv", row.names = FALSE)

ESS %>%
  write.csv(file = "ESS_COMPLETA.csv", row.names = FALSE)

  
