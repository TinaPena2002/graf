## Prevalencia de la mora 

df_aux <- df


df_aux$mora <- factor(df_aux$mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
df_aux <- df_aux %>% count(mora) %>% mutate(pct = round(n/sum(n)*100))



ggplot(df_aux, aes(x = "", y = n, fill = mora)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribución de la mora", y = "frecuencia", x = "", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#9BCD9B", "Moroso" = "#9c5641")) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))+
  theme_bw()

ggplot(df_aux, aes(x = mora, y = pct, fill = mora)) + 
  geom_bar(stat = "identity") + labs(title = "Prevalencia de la mora", y = "frecuencia", x = "",fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) +
  geom_text(data = a, aes(y = n, label = n), vjust = -0.2)

ggplot(df_aux, aes(x = "", y = n, fill = mora)) + 
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") +
  labs(title = "Prevalencia de la mora", y = "frecuencia", x = "", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))




## Proporción de atrasos severos

df_aux <- df

df_aux <- df %>% filter(atraso > 0) 

quantile(df_aux$atraso)
df_aux$atraso <- cut(df_aux$atraso, breaks = c(0,21,245), labels = c("Moderados/Estándares", "Severos"))
df_aux <- df_aux %>% count(atraso) %>% mutate(pct = round(n/sum(n)*100))



ggplot(df_aux, aes(x = atraso, y = n, fill = atraso)) + 
  geom_bar(stat = "identity") + labs(title = "Severidad de atrasos", y = "frecuencia",fill = "Atraso") + 
  scale_fill_manual(values = c("Moderados/Estándares" = "#CAFF70", "Severos" = "#9BCD9B")) + 
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))



ggplot(df_aux, aes(x = "", y = n, fill = atraso)) + 
  geom_bar(stat = "identity") +labs(title = "Severidad de atrasos", y = "frecuencia", x = "",fill = "Atraso") + 
  scale_fill_manual(values = c("Moderados/Estándares" = "#CAFF70", "Severos" = "#9BCD9B")) + 
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))



ggplot(df_aux, aes(x = "", y = pct, fill = atraso)) + 
  geom_bar(stat = "identity") + coord_polar(theta = "y") + 
  labs(title = "Prevalencia de la mora", y = "", x = "",fill = "Atraso") + 
  scale_fill_manual(values = c("Moderados/Estándares" = "#CAFF70", "Severos" = "#9BCD9B")) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5))




## Mora según nivel de ingreso:

df_aux <- df %>% select(ingreso, mora) %>% 
  mutate(mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))) %>%
  mutate(ingreso = cut(ingreso, breaks = c(0,1360,4900, 30000), labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos"))) %>% 
  count(mora, ingreso)

ggplot(df_aux, aes(x = "", y = n, fill = as.factor(mora))) +
  facet_wrap(~ingreso) + 
  geom_bar(stat = "identity") + labs(title = "Mora según nivel de ingreso", x = "", y = "frecuencia", fill = "Mora") + 
  scale_fill_manual(values = c( "No Moroso" = "#9BCD9B", "Moroso" = "#9e7062")) + 
  theme_bw()

ggplot(df_aux, aes(x = "", y = n, fill = as.factor(mora))) +
  facet_wrap(~ingreso) + 
  geom_bar(stat = "identity", position = "fill") + labs(title = "Mora según nivel de ingreso", x = "", y = "frecuencia", fill = "Mora") + 
  scale_fill_manual(values = c( "No Moroso" = "#9BCD9B", "Moroso" = "#9e7062")) + 
  theme_bw()



ggplot(a, aes(x = "", fill = as.factor(mora))) + 
  geom_bar(position = "fill") + coord_polar(theta = "y") + 
  labs(title = "Prevalencia de la mora", y = "", x = "",fill = "Mora") + 
  facet_wrap(~ nivelDeIngreso) + 
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 

        #Si se quiere comentar distribución de ingreso: Se puede correr anteriores y esta hace sentido
        df_aux <- df_aux %>% mutate (nivelDeIngreso = cut(ingreso, breaks = 3 , labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos")))
        ggplot(df_aux, aes(x = nivelDeIngreso, fill = mora)) + geom_bar()+
          labs(title = "Mora según nivel de ingreso", x = "", y = "frecuencia", fill = "Mora") + 
          scale_fill_manual(values = c( "No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 


## Mora según nivel educativo:
df_aux <- df
  general <- data.frame(nivelEducativo = df_aux$nivel_educ_clean, mora = df_aux$mora)
  general$nivelEducativo <- factor(general$nivelEducativo,labels = c("General", "General"))
  general$mora <- factor(general$mora, levels = c(0,1), labels = c("No Moroso","Moroso"))
  dist <- data.frame(nivelEducativo = df_aux$nivel_educ_clean, mora = df_aux$mora)
  dist$nivelEducativo <- factor(dist$nivelEducativo, levels = c("Alta", "Media o menos"), labels = c("Nivel Educativo: Alto","Nivel Educativo: Medio o bajo"))
  dist$mora <- factor(dist$mora, levels = c(0,1), labels = c("No Moroso","Moroso"))
  aux <- rbind(general, dist)
        
ggplot(aux, aes(x = "", fill = mora)) + 
  geom_bar(position = "fill") + coord_polar(theta = "y") + 
  facet_wrap(~ nivelEducativo) + labs(title = "Morosidad: Nivel Educativo", y = "", fill = "Mora") + 
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 

ggplot(aux, aes(x = nivelEducativo, fill = mora)) + geom_bar(position = "fill") +
  labs(title = "Morosidad según Nivel educativo", x = "", y = "frecuencia", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 

        
ggplot(aux, aes(x = nivelEducativo, fill = mora)) + geom_bar() +
  labs(title = "Morosidad según Nivel educativo", x = "", y = "frecuencia", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 



## Mora según nivel educativo y nivel de ingreso

df_aux <- df
df_aux <- df_aux %>% mutate (nivelDeIngreso = cut(ingreso, breaks = c(0,1360,4900, 30000), labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos")))
df_aux$mora <- factor(df_aux$mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))

ggplot(df_aux, aes(x = nivelDeIngreso, fill = as.factor(mora))) +
  geom_bar() +
  facet_wrap(~ nivel_educ_clean) +
  labs(title = "Mora según nivel de ingreso y nivel educativo",x = "Nivel de ingresos", y = "Cantidad", fill = "Mora") +
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 


## Mora según score y experiencia financiera

df_aux <- df
quantile(df_aux$exp_sf_clean, probs = c(0,0.2,0.8,1))
df_aux$exp_sf_clean <- cut(df_aux$exp_sf_clean, breaks = c(0,7,50,300), include.lowest = TRUE, right = FALSE,labels = c("Baja", "Media","Alta"))

df_aux <- df_aux %>% filter(exp_sf_clean != "Media")
df_aux$mora <- factor(df_aux$mora, levels = c(0,1), labels = c("No Moroso", "Moroso"))
df_aux$score <- cut(df_aux$score, breaks = 4)

ggplot(df_aux, aes(x = score, fill = as.factor(mora))) + 
  geom_bar() + labs(title = "Morosidad según score y experiencia financiera", x = "Score", y = "frecuencia", fill = "Mora") + 
  facet_wrap(~exp_sf_clean) +   
  scale_fill_manual(values = c("No Moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 





## Atraso según calificación


df_aux <- df %>% filter(atraso > 0)

ggplot(df_aux,aes(x = as.factor(clasif_sbs), y = log(atraso), fill = factor(clasif_sbs))) + geom_boxplot() +
  labs(title = "Atraso según clasificación crediticia", x = "clasificación crediticia", y = "días de atraso (log)") + 
  scale_fill_manual(values = c("lightblue","lightblue1","lightblue2","lightblue3","lightblue4"))




## Atraso según score


df_aux <- df %>% filter(atraso > 0)
df_aux$score <- cut(df_aux$score, breaks = 4)

  #BOXPLOT
    ggplot(df_aux,aes(x = as.factor(score), y = log(atraso), fill = factor(score))) + geom_boxplot() +
      labs(title = "Atraso según score", x = "clasificación crediticia", y = "días de atraso (log)")+ 
      scale_fill_manual(values = c("plum","plum1","plum2","plum3","plum4"))

    
## Calificación según deuda
  
df_aux <- df %>% filter(mora == 1)%>% 
    select(deuda_sf_clean, clasif_sbs) %>% 
    mutate(catDeuda = cut(deuda_sf_clean, breaks = quantile(deuda_sf_clean, probs = c(0,0.15,0.85,1)), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
    filter(catDeuda != "media") %>% 
    count(catDeuda, clasif_sbs) %>% 
    mutate(n = ifelse(catDeuda == "baja", -n,n))
    
ggplot(df_aux, aes(x = clasif_sbs, y = n, fill = catDeuda)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Clasificación según tipo de deuda", y = "", x = "Clasificación financiera", fill = "Magnitud de deuda") + 
  scale_fill_manual(values = c("plum","plum4"))

## Calificación según ingreso


df_aux <- df %>% filter(mora == 1)%>% 
  select(ingreso, clasif_sbs) %>% 
  mutate(catIngreso = cut(ingreso, breaks = quantile(ingreso, probs = c(0,0.15,0.85,1)), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
  filter(catIngreso != "media") %>% 
  count(catIngreso, clasif_sbs) %>% 
  mutate(n = ifelse(catIngreso == "baja", -n,n))

ggplot(df_aux, aes(x = clasif_sbs, y = n, fill = catIngreso)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Clasificación según nivel de ingreso", y = "", x = "Clasificación financiera", fill = "Nivel de ingreso") + 
  scale_fill_manual(values = c("pink1","pink4"))


## Score según deuda
df_aux <- df %>% filter(mora == 1) %>% select(score, deuda_sf_clean) %>% 
  mutate(score = cut(score, breaks = 5)) %>% 
  mutate(catDeuda = cut(deuda_sf_clean, breaks = quantile(deuda_sf_clean, probs = c(0,0.15,0.85,1)), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
  filter(catDeuda != "media") %>% 
  count(score, catDeuda) %>% 
  mutate(n = ifelse(catDeuda == "baja", -n,n))

ggplot(df_aux, aes(x = score, y = n, fill = catDeuda)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Score según tipo de deuda", y = "", x = "Score", fill = "Magnitud de deuda") + 
  scale_fill_manual(values = c("yellow3","green4"))

## Score según nivel de ingreso

df_aux <- df %>% filter(mora == 1) %>% select(score, ingreso) %>% 
  mutate(score = cut(score, breaks = 5)) %>% 
  mutate(catIngreso = cut(ingreso, breaks = quantile(ingreso, probs = c(0,0.15,0.85,1)), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
  filter(catIngreso != "media") %>% 
  count(score, catIngreso) %>% 
  mutate(n = ifelse(catIngreso == "baja", -n,n))
    
ggplot(df_aux, aes(x = score, y = n, fill = catIngreso)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Score según nivel de ingreso", y = "", x = "Score", fill = "Nivel de ingreso") + 
  scale_fill_manual(values = c("yellow3","green4"))




## Morosidad según zona

df_aux <- df %>% 
          select(zona_clean, mora)

ggplot(df_aux, aes(x = zona_clean, fill = as.factor(mora))) + geom_bar(position = "fill") + 
  labs(title = "Morosidad según zona", x = "", y = "frecuencia", fill = "Morosidad") +
  scale_fill_manual(values = c("plum3","orange2"))


#Severidad de atraso según zona

df_aux <- df %>% filter(atraso > 0) %>% 
  select(zona_clean, atraso) %>% mutate(atraso = log(atraso))

ggplot(df_aux, aes( x = zona_clean, y = (atraso), fill = zona_clean)) + geom_boxplot() + 
  scale_fill_manual(values = c("lightblue","lightblue1")) +
  labs(title = "Atrasos según zona", x = "", y = "Días de atraso, log", fill = "Zona")


#Vivienda y linea de crédito y score

df_aux <- df %>% select(vivienda_clean, linea_sf_clean, score) %>% 
  filter(linea_sf_clean > 0) %>% 
  mutate(linea_sf_clean = log(linea_sf_clean))

ggplot(df_aux, aes(x = vivienda_clean, y = linea_sf_clean, fill = vivienda_clean)) + geom_boxplot() +
  labs(title = "Línea de crédito según vivienda", x = "vivienda", y = "Línea de crédito, log") + 
  scale_fill_manual(values = c("lightblue","lightblue1")) 
  

ggplot(df_aux, aes(x = vivienda_clean, y = score, fill = vivienda_clean)) + geom_boxplot() +
  labs(title = "Score según vivienda", x = "vivienda", y = "Score") + 
  scale_fill_manual(values = c("lightblue","lightblue1")) 



#Ahorro y severidad de mora

df_aux <- df %>% filter(atraso > 0) %>% filter(mora == 1) %>% 
  select(atraso, nivel_ahorro) %>% 
  mutate(atraso = log(atraso)) %>%
  mutate(nivel_ahorro = factor(nivel_ahorro, levels = 0:12, labels = c("Bajo", "Bajo", 0, 0, 0, "Medio","Medio", "Medio", 0, 0, 0, "Alto", "Alto"))) %>% 
  filter(nivel_ahorro != 0)

ggplot(df_aux, aes(x = as.factor(nivel_ahorro), y = atraso, fill = nivel_ahorro)) + 
  geom_boxplot() + labs(title = "Atraso según nivel de ahorro", x = "Nivel de ahorro", y = "Días de atraso, log", fill = "") +
  scale_fill_manual(values = c("palevioletred2","palevioletred3", "palevioletred4"))

#Score según nivel de ahorro

df_aux <- df %>%  select(score, nivel_ahorro) %>% 
  mutate(nivel_ahorro = factor(nivel_ahorro, levels = 0:12, labels = c("Bajo", "Bajo", 0, 0, 0, "Medio","Medio", "Medio", 0, 0, 0, "Alto", "Alto"))) %>% 
  filter(nivel_ahorro != 0)

ggplot(df_aux, aes(x = as.factor(nivel_ahorro), y = score, fill = nivel_ahorro)) +
  geom_boxplot() + labs(title = "Score según nivel de ahorro", x = "Nivel de ahorro", y = "Score", fill = "") +
  scale_fill_manual(values = c("palevioletred2","palevioletred3", "palevioletred4"))


# Línea de crédito y score, comparado con edad 

df_aux <- df %>% select(linea_sf_clean, score, edad) %>% 
  mutate(score = cut(score, breaks = 4)) %>% 
  filter(linea_sf_clean > 0) %>% 
  mutate(linea_sf_clean = log(linea_sf_clean))
         
a <- quantile(df$edad, probs = c(0,0.3,0.7,1))

ggplot(df_aux, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + geom_boxplot() +
  labs(title = "Linea de crédito según score", x = "Nivel de Score", y = "Valor de linea de crédito") + 
  scale_fill_manual(values = c("peachpuff","peachpuff2", "peachpuff3", "peachpuff4"))


df_aux1 <- df_aux %>% filter(edad <= 30) 

ggplot(df_aux1, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + geom_boxplot() +
  labs(title = "Linea de crédito según score", x = "Nivel de Score", y = "Valor de linea de crédito") + 
  scale_fill_manual(values = c("lightsalmon1","lightsalmon2", "lightsalmon3", "lightsalmon4"))


df_aux2 <- df_aux %>% filter(edad >= 40 )

ggplot(df_aux2, aes(x = as.factor(score), y = linea_sf_clean, fill = score)) + geom_boxplot() +
  labs(title = "Linea de crédito según score", x = "Nivel de Score", y = "Valor de linea de crédito") + 
  scale_fill_manual(values = c("lightsalmon1","lightsalmon2", "lightsalmon3", "lightsalmon4"))



#Dias laborales, morosidad y atraso

a <- quantile(df$dias_lab, probs = c(0,0.1,0.45,0.55,0.9,1)) 

df_aux <- df %>%
  select(dias_lab, mora, atraso, ingreso) %>%
  mutate(dias_lab = cut(dias_lab, breaks = quantile(dias_lab, probs = c(0, 0.1, 0.45, 0.55, 0.9, 1), na.rm = TRUE),labels = c("Pocos", "1", "Media", "1", "Muchos"), include.lowest = TRUE)) %>%
  filter(dias_lab != "1") %>% 
  filter(atraso > 0) %>% 
  mutate(atraso = log(atraso)) %>% 
  mutate(mora = factor(mora, levels = c(0,1), labels = c("No moroso", "Moroso"))) 

ggplot(df_aux, aes(x = dias_lab, fill = as.factor(mora))) + geom_bar(position = "fill") + 
  labs(title = "Mora según días de trabajo", x = "Días de trabajo", y = "frecuencia", fill = "Morosidad") +
  scale_fill_manual(values = c("No moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 


ggplot(df_aux, aes(x = dias_lab, y = atraso, fill = dias_lab)) + geom_boxplot() + 
  labs(title = "Atraso según días de trabajo", x = "Días de trabajo", y = "Días de atraso, log", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "plum4")) 

ggplot(df_aux, aes(x = "", fill = as.factor(mora))) + 
  geom_bar(position = "fill") + coord_polar(theta = "y") + 
  labs(title = "Prevalencia de la mora según días de trabajo", y = "Días de trabajo", x = "",fill = "Mora") +
  facet_wrap(~dias_lab) +
  scale_fill_manual(values = c("No moroso" = "#CAFF70", "Moroso" = "#9BCD9B")) 


#Busco relaciones para geom_point
  
df_aux <- df %>% select(mora, ingreso, linea_sf, deuda_sf_clean, atraso, score) %>% 
  filter(atraso > 0) %>% 
  mutate(atraso = log(atraso)) %>% 
  mutate(atraso = cut(atraso, breaks = quantile(atraso), levels = c("Bajo", "Medio", "Medio", "Alto"))) %>% 
  filter(atraso != "Medio") %>% 
  mutate(mora = factor(mora, levels = c(0,1), labels = c("No Moroso", "Moroso")))






ggplot(df_aux, aes(x = score, y = linea_sf, colour = mora)) + geom_point() + geom_smooth(method = "lm")




### Nuevos gráficos: Severidad del atraso según variables demográficas: 


## Bivariadas:


#Días de atraso y tipo de vivienda

df_aux <- df %>% select(atraso, vivienda, edad, nivel_educ_clean, ingreso, deuda_sf_clean) %>% 
  filter(atraso>0) %>% 
  mutate(Tipoedad = cut(edad, breaks = c(20,24,59,85), include.lowest = TRUE,labels = c("Jovenes adultos", "Adultos", "Adultos mayores"))) %>% 
  mutate(Tipoatraso = cut(atraso, breaks = c(0,21,245), include.lowest = TRUE,labels = c("Atraso Leve","Atraso Severo")))%>% 
  mutate(Tipoingreso = cut(ingreso, breaks = c(0,1360,4900, 30000), labels = c("Ingresos Bajos", "Ingresos Medios", "Ingresos Altos")))





ggplot(df_aux, aes(x = vivienda, y = log(atraso), fill = vivienda)) + geom_boxplot() +
  labs(title = "Severidad del atraso según tipo de vivienda", x = "Tipo de vivienda", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "azure4")) + 
  theme_bw()

#Días de atraso y edad

##boxplot
ggplot(df_aux, aes(x = (Tipoedad), y = log(atraso), fill = Tipoedad)) + geom_boxplot() +
  labs(title = "Severidad del atraso según edad", x = "Edad", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "azure4")) + 
  theme_bw()

##Barras

ggplot(df_aux, aes(x = Tipoedad, fill = Tipoatraso)) + geom_bar(position = "fill")+
  labs(title = "Severidad del atraso según edad", x = "Edad", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B"))+
  theme_bw()

##Torta
ggplot(df_aux, aes(x = "", fill = Tipoatraso)) + geom_bar(position = "fill")+
  coord_polar(theta = "y")+
  facet_wrap(~Tipoedad) +
  labs(title = "Severidad del atraso según edad", x = "Edad", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("azure4", "#CAFF70", "#9BCD9B")) + 
  theme_bw()




##Dias de atraso y nivel educativo

##Barras
ggplot(df_aux, aes(x = nivel_educ_clean, fill = Tipoatraso)) + geom_bar(position = "fill")+
  labs(title = "Severidad del atraso según Nivel Educativo", x = "Nivel Educativo", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) + 
  theme_bw()

##Boxplot
ggplot(df_aux, aes(x = nivel_educ_clean, y = log(atraso), fill = nivel_educ_clean)) + geom_boxplot()+
  labs(title = "Severidad del atraso según Nivel Educativo", x = "Nivel Educativo", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) + 
  theme_bw()

## Dias de atraso segun nivel de ingresos


ggplot(df_aux, aes(x = Tipoingreso, y = log(atraso), fill = Tipoingreso)) + geom_boxplot()+
  labs(title = "Severidad del atraso según Nivel de ingresos", x = "", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "azure3")) + 
  theme_bw()

ggplot(df_aux, aes(x=Tipoatraso, y = log(ingreso), fill = Tipoatraso)) + geom_boxplot()+
  labs(title = "Distribucion del ingreso segun severidad de atrasos", x = "", y = "Ingreso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "azure3")) + 
  theme_bw()

ggplot(df_aux, aes(x = Tipoingreso, fill = (Tipoatraso))) + geom_bar(position = "fill")+
  labs(title = "Severidad del atraso según Nivel de ingresos", x = "", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) + 
  theme_bw()



## Dias de atraso segun nivel de deuda

df_aux <- df %>% select(atraso, deuda_sf_clean, mora) %>% 
  filter(mora == 1) %>% 
  mutate(catDeuda = cut(deuda_sf_clean, breaks = quantile(deuda_sf_clean, probs = c(0,0.15,0.85,1)), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
  mutate(Tipoatraso = cut(atraso, breaks = c(0,21,245), include.lowest = TRUE,labels = c("Atraso Leve","Atraso Severo")))

ggplot(df_aux, aes(x = catDeuda, y = log(atraso), fill = catDeuda)) + geom_boxplot() + 
  labs(title = "Severidad del atraso según Tipo de Deuda", x = "Tipo de Deuda", y = "Duración de atraso (log)", fill = "") +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B", "azure2")) + 
  theme_bw()


##Dias de atraso segun experiencia financiera

df_aux <- df %>% select(atraso, exp_sf_clean) %>% 
  mutate(Tipoexp = cut(exp_sf_clean, breaks = quantile(exp_sf_clean, probs = c(0,0.2,0.8,1)), include.lowest = TRUE, labels  =c("Baja", "Media", " Alta"))) %>% 
  mutate(Tipoatraso = cut(atraso, breaks = c(0,21,245), include.lowest = TRUE,labels = c("Atraso Leve","Atraso Severo")))
  
df_aux <- df_aux %>%
  group_by(Tipoexp, Tipoatraso) %>%
  mutate(n = ifelse(Tipoatraso == "Atraso Severo", -n, n)) 


ggplot(df_aux, aes(x = Tipoexp, y = n, fill = Tipoatraso)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Experiencia financiera según el tipo de atraso", y = "", x = "Experiencia financiera", fill = "Tipo de Atraso:") + 
  scale_fill_manual(values = c("plum","plum4"))


## Atraso y linea de credito 


df_aux <- df %>% select(atraso, linea_sf, mora) %>% filter(!is.na(linea_sf)) %>% 
  mutate(LINEA = cut(linea_sf, breaks = c(quantile(linea_sf, probs = c(0,0.25,0.75,1))), labels = c("Bajas", "Medias", "Altas"), include.lowest = TRUE)) %>% 
  mutate(Tipoatraso = cut(atraso, breaks = c(0,21,245), include.lowest = TRUE,labels = c("Atraso Leve","Atraso Severo"))) %>% 
  mutate(AA = factor(mora, levels = 0:1, labels = c("No Moroso, Moroso")))

quantile(df_aux$linea_sf)

ggplot(df_aux, aes(x = LINEA, y = log(atraso), fill = LINEA))+ geom_boxplot() +
  labs(title = "Longitud de atraso según linea de crédito", y = "Días de atraso (log)", x = "Tipo de línea de crédito") +
  scale_fill_manual(values = c("aquamarine2","aquamarine3", "aquamarine4"))

ggplot(df_aux, aes(x = LINEA, fill = Tipoatraso)) + geom_bar(position = "dodge") +
  labs(title = "Tipo de atraso según linea de crédito", x = "Tipo de línea de crédito", y = "", fill = "Tipo de Atraso") +
  scale_fill_manual(values = c("plum","aquamarine3")) + 
  theme_bw()

ggplot(df_aux,aes( x = LINEA, fill = as.factor(mora))) + geom_bar() + 
  scale_fill_manual(values = c("plum","aquamarine3")) + 
  labs(title = "Morosidad según línea de crédito", x = "Tipo de línea de crédito", y = "", fill = "Mora") 

ggplot(df_aux, aes(x = as.factor(mora), y = log(linea_sf), fill = as.factor(mora))) + geom_boxplot() + 
  scale_fill_manual(values = c("plum","aquamarine3")) + 
  labs(title = "Línea de crédito según morosidad de clientes", x = "", y = "", fill = "Mora") 


##Mora según tipo de deuda 

df_aux <- df %>% select(mora, deuda_sf) %>%
  mutate(catDeuda = cut(deuda_sf, breaks = quantile(deuda_sf, probs = c(0,0.15,0.85,1), na.rm = TRUE), labels = c("baja", "media", "alta"), include.lowest = TRUE)) %>% 
  filter(!is.na(catDeuda)) %>% 
  mutate(mora = factor(mora, levels = 0:1, labels = c("No moroso", "Moroso"))) 

ggplot(df_aux, aes(x = catDeuda, fill = as.factor(mora))) + geom_bar(position = "dodge") + 
  theme_bw() +
  scale_fill_manual(values = c("plum","lightblue")) + 
  labs(title = "Mora según tipo de deuda", x = "Tipo de Deuda", y = "", fill = "Mora")

  


##Mora segun score

df_aux <- df %>% select(mora, clasif_sbs) %>% mutate(mora = factor(mora, levels = 0:1, labels = c("No moroso", "Moroso"))) 
  
ggplot(df_aux, aes(x = clasif_sbs, fill = as.factor(mora))) + geom_bar(position = "dodge") +
  labs(title = "Mora según clasificación crediticia", x = "Clasificación crediticia", y = "Frecuencia absoluta", fill = "Mora") + 
  theme_bw() + 
  scale_fill_manual(values = c("plum","lightblue")) 
  
ggplot(df_aux, aes(x = "", fill = (mora))) + 
  geom_bar(position = "fill") + coord_polar(theta = "y") + 
  facet_wrap(~ clasif_sbs) + labs(title = "Morosidad: Clasificación crediticia", y = "", fill = "Mora", x = "") + 
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) +
  theme_bw()

df_aux1 <- df_aux %>%
  count(clasif_sbs, mora)


ggplot(df_aux1, aes(x = clasif_sbs, y = mora, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "plum") +
  labs(title = "Frecuencia de combinaciones de morosidad con Clasificación crediticia", x = "Clasificación crediticia", y = "Mora", fill = "Frecuencia") +
  theme_minimal()


##Mora según experiencia financiera

df_aux <- df %>% select(mora, exp_sf_clean)  %>% 
  mutate(exp_sf_clean = cut(exp_sf_clean, breaks  = c(0,7,50,300), include.lowest = TRUE, right = FALSE,labels = c("Baja", "Media","Alta"))) %>% 
  filter(exp_sf_clean != "Media") %>% 
  mutate(mora = factor(mora, levels = 0:1, labels = c("No moroso", "Moroso"))) 

ggplot(df_aux, aes(x = exp_sf_clean, fill = as.factor(mora))) + geom_bar(position = "fill") +
  labs(title = "Morosidad según experiencia financiera", x = "Experiencia financiera", y = "Frecuencia relativa", fill = "") + 
  theme_bw() + 
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) 

## Mora según vivienda

df_aux <- df %>% select(vivienda_clean, mora) %>% 
  mutate(mora = factor(mora, levels = 0:1, labels = c("No moroso", "Moroso"))) 

ggplot(df_aux, aes(x = "", fill = as.factor(mora))) + 
  geom_bar(position = "fill") + 
  coord_polar(theta = "y") + 
  facet_wrap(~vivienda_clean) +
  scale_fill_manual(values = c("#CAFF70", "#9BCD9B")) +
  labs(title = "Mora según tipo de vivienda", x = "", y ="", fill = "") + 
  theme_bw()






